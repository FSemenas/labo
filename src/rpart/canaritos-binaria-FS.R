# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")
require("rpart.plot")

# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/flore/OneDrive/Escritorio/Flor/Maestria/DMEyF")
# Poner sus semillas
semillas <- c(412339, 869587, 417227, 290923, 437357)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dapply <-  dataset[foto_mes == 202103]
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]
dapply[, clase_ternaria := NULL]

set.seed(semillas[1])


################ FEATURE ENGINEERING  #####################
###### Features que se pueden calcular en train y en test de forma conjunta
# Deberían ser aquellos que no usan valores globales o de otras filas.######

sufijos_visa_master <- c(  "_delinquency",  "_mfinanciacion_limite",
                           "_msaldototal",  "_msaldopesos",  "_msaldodolares",
                           "_mconsumospesos",  "_mconsumosdolares","_mlimitecompra",
                           "_madelantopesos",  "_madelantodolares")

# Creo los campos suma de visa y master
for (suf in sufijos_visa_master){
  n_visa = paste0("Visa",suf)
  n_master = paste0("Master",suf)
  n_nuevo = paste0("Visa_plus_Master",suf)
  dataset <- dataset[,(n_nuevo) := ifelse(is.na(get(n_visa)),0,get(n_visa)) + 
                       ifelse(is.na(get(n_master)),0,get(n_master))]
  
  dapply <- dapply[,(n_nuevo) := ifelse(is.na(get(n_visa)),0,get(n_visa)) + 
                     ifelse(is.na(get(n_master)),0,get(n_master))]
}

# Hay otros campos que no son sufijos, pero también son sumables.
dataset[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dataset[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dataset[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]

dapply[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dapply[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dapply[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]

# Creo campos consumo / limite como medida de actividad
dataset[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dataset[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]

dapply[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dapply[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]

# antiguedad sobre edad (proporción de su vida como cliente de banco)
dataset[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]
dapply[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]

# Rankeo variables.
prefix <- "r_"

quantiles <- list("cliente_edad"=10,
                  "cliente_antiguedad"=4,
                  "mpayroll"=10)

for (var in names(quantiles)) {
  dataset[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]
  dapply[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]
  
}

# FS - Creo campo con cantidad de productos que tiene en el banco. A mas productos, mas dificil desenchufarse
dataset[,c_productos_banco := ccuenta_corriente+ccaja_ahorro+ctarjeta_visa+ctarjeta_master+cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios+cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales+ccaja_seguridad+ccuenta_debitos_automaticos]
dapply[,c_productos_banco := ccuenta_corriente+ccaja_ahorro+ctarjeta_visa+ctarjeta_master+cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios+cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales+ccaja_seguridad+ccuenta_debitos_automaticos]

# FS - Creo campo con monto que le debe al banco por prestamos y tarjetas / limite tarjeta
dataset[,m_financiado_por_banco := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+Master_msaldototal+Visa_msaldototal+Master_mlimitecompra+Visa_mlimitecompra]
dapply[,m_financiado_por_banco := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+Master_msaldototal+Visa_msaldototal+Master_mlimitecompra+Visa_mlimitecompra]

dataset[, numero_de_cliente := NULL]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dataset[,m_ratio_rentabilidad := mrentabilidad/(mrentabilidad_annual/12)]
dapply[,m_ratio_rentabilidad := mrentabilidad/(mrentabilidad_annual/12)]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dataset[,c_ratio_trx := (cpayroll_trx +cpayroll2_trx +cpagodeservicios +cpagomiscuentas +ctransferencias_emitidas +cextraccion_autoservicio +ccallcenter_transacciones +chomebanking_transacciones +ccajas_transacciones +ccajas_depositos +ccajas_extracciones +ccajas_otras +catm_trx +catm_trx_other +ctrx_quarter +cmobile_app_trx)/(ctrx_quarter/3)]
dapply[,c_ratio_trx := (cpayroll_trx +cpayroll2_trx +cpagodeservicios +cpagomiscuentas +ctransferencias_emitidas +cextraccion_autoservicio +ccallcenter_transacciones +chomebanking_transacciones +ccajas_transacciones +ccajas_depositos +ccajas_extracciones +ccajas_otras +catm_trx +catm_trx_other +ctrx_quarter +cmobile_app_trx)/(ctrx_quarter/3)]

# FS - Armo el árbol de decisión para sacar variables
arbolbinario <- rpart("clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
                      data =      dataset,
                      xval =      0,
                      cp =       -1,
#                      minsplit =  98,
#                      minbucket = 49,
                      maxdepth =  3)

#grafico el arbol
prp(arbolbinario, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

print(arbolbinario)

print(arbolbinario$variable.importance)

# PROPUESTOS POR GUSTAVO 

dataset[ , campo1 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen < -43.6) ]
dataset[ , campo2 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen >= -43.6) ]
dataset[ , campo3 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo < -7.53) ]
dataset[ , campo4 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo >= -7.53) ]
dataset[ , campo5 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite >=15043.73) ]
dataset[ , campo6 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite < 15043.73) ]
dataset[ , campo7 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter < 37.5) ]
dataset[ , campo8 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter >= 37.5) ]

dapply[ , campo1 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen < -43.6) ]
dapply[ , campo2 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen >= -43.6) ]
dapply[ , campo3 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo < -7.53) ]
dapply[ , campo4 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo >= -7.53) ]
dapply[ , campo5 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite >=15043.73) ]
dapply[ , campo6 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite < 15043.73) ]
dapply[ , campo7 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter < 37.5) ]
dapply[ , campo8 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter >= 37.5) ]

head(dataset,10)

############################################################################
###### Features que NO pueden calcular en train y en test de forma conjunta ########
# Particionamos de forma estratificada

################## MODELO ################################
#agrego 30% de canaritos
for( i in 1:floor(0.3*length(dataset)) ){
  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]
  dapply[ , paste0("canarito", i ) :=  runif( nrow(dapply)) ]
} 

dtrain <- dataset[ foto_mes==202101 ]
#dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula= "clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"evento"]

# ##################### OPTIMIZACION BAYESIANA ##################################
 # funcion ganancia
 ganancia <- function(probabilidades, clase) {
   return(sum(
     (probabilidades >= 0.05) * ifelse(clase == "evento", 78000, -2000))
   )
 }
 
 # Armamos una función para modelar con el fin de simplificar el código futuro
 modelo_rpart <- function(dtrain, test, cp =  0, ms = 20, mb = 1, md = 10) {
   modelo <- rpart(clase_binaria ~ ., data = train,
                   xval = 0,
                   cp = cp,
                   minsplit = ms,
                   minbucket = mb,
                   maxdepth = md)
   
   test_prediccion <- predict(modelo, test, type = "prob")
   ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
   
 }
 
 # Una función auxiliar para los experimentos
 experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
   gan <- c()
   for (s in semillas) {
     set.seed(s)
     in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                               list = FALSE)
     train  <-  ds[in_training, ]
     test   <-  ds[-in_training, ]
     #train_sample <- tomar_muestra(train)
     r <- modelo_rpart(dtrain, test, 
                       cp = cp, ms = ms, mb = mb, md = md)
     gan <- c(gan, r)
   }
   mean(gan)
 }
 
 obj_fun_md_ms <- function(x) {
   experimento_rpart(dataset, semillas
                     , md = x$maxdepth
                     , ms = x$minsplit
                     , mb = floor(x$minbucket*x$minsplit))
 }
 
 obj_fun <- makeSingleObjectiveFunction(
   minimize = FALSE,
   fn = obj_fun_md_ms,
   par.set = makeParamSet(
     makeIntegerParam("maxdepth",  lower = 4L, upper = 30L),
     makeIntegerParam("minsplit",  lower = 1L, upper = 200L),
     makeNumericParam("minbucket",  lower = 0L, upper = 1L)),
   has.simple.signature = FALSE
 )
 
 ctrl <- makeMBOControl()
 ctrl <- setMBOControlTermination(ctrl, iters = 100L)
 ctrl <- setMBOControlInfill(
   ctrl,
   crit = makeMBOInfillCritEI(),
   opt = "focussearch",
   opt.focussearch.points = 20
 )
 
 lrn <- makeMBOLearner(ctrl, obj_fun)
 
 surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
 
 run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
 print(run_md_ms)

#Recommended parameters:
#maxdepth=11; minsplit=10; minbucket=0.284
# Particionamos de forma estratificada



for (corte in c(0.01,0.025,0.03,0.05,0.075,0.09, 0.1)){
  entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                    "Predicted"= as.integer(  prediccion > corte ) ) )
  fwrite( entrega, paste0( "./canaritos_binaria_",corte,".csv"), sep="," )
}

print(prediccion)

#pdf(file = "./stopping_at_canaritos.pdf", width=28, height=4)
#prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#dev.off()

