# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(412339, 869587, 417227, 290923, 437357)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -1.0, upper=    0.1),
          makeNumericParam("minsplit" , lower=   1,   upper= 5000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]


  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #debe ir 1 si es Windows

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 1 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd("C:/Users/flore/OneDrive/Escritorio/Flor/Maestria/DMEyF")

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]

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
  dtrain <- dtrain[,(n_nuevo) := ifelse(is.na(get(n_visa)),0,get(n_visa)) + 
                       ifelse(is.na(get(n_master)),0,get(n_master))]
  
}

# Hay otros campos que no son sufijos, pero también son sumables.
dtrain[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dtrain[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dtrain[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]

# Creo campos consumo / limite como medida de actividad
dtrain[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dtrain[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]

# antiguedad sobre edad (proporción de su vida como cliente de banco)
dtrain[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]

# Rankeo variables.
prefix <- "r_"

quantiles <- list("cliente_edad"=10,
                  "cliente_antiguedad"=4,
                  "mpayroll"=10)

for (var in names(quantiles)) {
  dtrain[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]

}

# FS - Creo campo con cantidad de productos que tiene en el banco. A mas productos, mas dificil desenchufarse
dtrain[,c_productos_banco := ccuenta_corriente+ccaja_ahorro+ctarjeta_visa+ctarjeta_master+cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios+cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales+ccaja_seguridad+ccuenta_debitos_automaticos]

# FS - Creo campo con monto que le debe al banco por prestamos y tarjetas / limite tarjeta
dtrain[,m_financiado_por_banco := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+Master_msaldototal+Visa_msaldototal+Master_mlimitecompra+Visa_mlimitecompra]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dtrain[,m_ratio_rentabilidad := mrentabilidad/(mrentabilidad_annual/12)]

# FS - La rentabilidad anual la divido por 12 para ver la mensual y calculo este mes, a que % esta de esa mensual.
dtrain[,c_ratio_trx := (cpayroll_trx +cpayroll2_trx +cpagodeservicios +cpagomiscuentas +ctransferencias_emitidas +cextraccion_autoservicio +ccallcenter_transacciones +chomebanking_transacciones +ccajas_transacciones +ccajas_depositos +ccajas_extracciones +ccajas_otras +catm_trx +catm_trx_other +ctrx_quarter +cmobile_app_trx)/(ctrx_quarter/3)]

# FS - Armo el árbol de decisión para sacar variables que quiero construir en base a las reglas del decision tree
#arbolbinario <- rpart("clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
#                      data =      dataset,
#                      xval =      0,
#                      cp =       -1,
                      #                      minsplit =  98,
                      #                      minbucket = 49,
#                      maxdepth =  3)

#grafico el arbol
#prp(arbolbinario, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

#print(arbolbinario)

#print(arbolbinario$variable.importance)

# PROPUESTOS POR GUSTAVO 

dtrain[ , campo1 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen < -43.6) ]
dtrain[ , campo2 := as.integer(ctrx_quarter <13.5 & c_productos_banco < 3.5 & mactivos_margen >= -43.6) ]
dtrain[ , campo3 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo < -7.53) ]
dtrain[ , campo4 := as.integer(ctrx_quarter <13.5 & c_productos_banco >= 3.5 & mcuentas_saldo >= -7.53) ]
dtrain[ , campo5 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite >=15043.73) ]
dtrain[ , campo6 := as.integer(ctrx_quarter >=13.5 & Visa_status >= 8 & Visa_plus_Master_mfinanciacion_limite < 15043.73) ]
dtrain[ , campo7 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter < 37.5) ]
dtrain[ , campo8 := as.integer(ctrx_quarter >=13.5 & Visa_status < 8 & ctrx_quarter >= 37.5) ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT4110_1/", showWarnings = FALSE )
setwd("./exp/HT4110_1/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT4110_1.txt"
archivo_BO   <- "HT4110_1.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

