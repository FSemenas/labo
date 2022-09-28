# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7240_1"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.44750218   #0.0142501265
PARAM$finalmodel$num_iterations    <-    570  #615
PARAM$finalmodel$num_leaves        <-   230  #784
PARAM$finalmodel$min_data_in_leaf  <-   246  #5628
PARAM$finalmodel$feature_fraction  <-      0.23437758  #0.8382482539
PARAM$finalmodel$semilla           <- 412339

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

################ FEATURE ENGINEERING  #####################

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
  
}

# Hay otros campos que no son sufijos, pero también son sumables.
dataset[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dataset[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dataset[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]

# Creo campos consumo / limite como medida de actividad
dataset[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dataset[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]

# antiguedad sobre edad (proporción de su vida como cliente de banco)
dataset[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]

# Rankeo variables.
prefix <- "r_"

quantiles <- list("cliente_edad"=10,
                  "cliente_antiguedad"=4,
                  "mpayroll"=10)

for (var in names(quantiles)) {
  dataset[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]
}

# FS - Creo campo con cantidad de productos que tiene en el banco. A mas productos, mas dificil desenchufarse
dataset[,c_productos_banco := ccuenta_corriente+ccaja_ahorro+ctarjeta_visa+ctarjeta_master+cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios+cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales+ccaja_seguridad+ccuenta_debitos_automaticos]

# FS - Creo campo con monto que le debe al banco por prestamos y tarjetas / limite tarjeta
dataset[,m_financiado_por_banco := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+Master_msaldototal+Visa_msaldototal+Master_mlimitecompra+Visa_mlimitecompra]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dataset[,m_ratio_rentabilidad := mrentabilidad/(mrentabilidad_annual/12)]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dataset[,c_ratio_trx := (cpayroll_trx +cpayroll2_trx +cpagodeservicios +cpagomiscuentas +ctransferencias_emitidas +cextraccion_autoservicio +ccallcenter_transacciones +chomebanking_transacciones +ccajas_transacciones +ccajas_depositos +ccajas_extracciones +ccajas_otras +catm_trx +catm_trx_other +ctrx_quarter +cmobile_app_trx)/(ctrx_quarter/3)]

#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
