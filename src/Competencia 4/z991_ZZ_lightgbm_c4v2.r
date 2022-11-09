#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ401_v2"
PARAM$exp_input  <- "HT401_v2"

PARAM$modelos  <- 2
# FIN Parametros del script

ksemilla  <- c(3, 5, 7, 11, 13, 17, 19, 23, 31, 43, 61, 79, 101, 127, 167, 191, 199, 313, 347, 701, 1709, 2617, 3539, 5807, 10501, 10691, 11279, 12391, 14479, 42737, 83339, 95369, 117239, 127031, 138937, 141079, 267017, 269987, 374321)

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
print("Leyendo log")
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
print("Leyendo TrainingStrategy")
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
print("Leyendo datasets")
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  print("Iniciando entrenamiento de primer modelo: creando dataset de train y parametrizando")
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  arch_modelo  <- paste0( "modelo_" ,
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".model" )


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  # Voy a loopear por varias semillas a partir de acá:
  for( ksemilla in  ksemillas)
{
    
  #Utilizo la semilla definida en este script
  print(paste("Iniciando entrenamiento para semilla:",ksemilla))
    
  #Utilizo la semilla definida en este script
  parametros$seed  <- ksemilla
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_", 
                        sprintf( "%02d", i ),
                        "_",
                        sprintf( "%03d", iteracion_bayesiana ),
                        "_seed",
                        sprintf(as.character(ksemilla)),
                        ".txt"),
          sep= "\t" )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]

  print("Guardando predicciones")
  nom_pred  <- paste0( "pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       "_seed",
                       sprintf(as.character(ksemilla)),
                       ".csv" )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from=  7000,
                  to=   11000,
                  by=     500 )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           "_seed",
                           sprintf(as.character(ksemilla)),
                           ".csv" )

    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

  }


  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}

