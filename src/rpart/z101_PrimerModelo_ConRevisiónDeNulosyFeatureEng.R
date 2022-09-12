#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/flore/OneDrive/Escritorio/Flor/Maestria/DMEyF")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# ----- NULOS -----
# Voy a ver la cantidad de nulos en las 10 variables con mas importancia para el modelo calculadas en z401_Sobre_Campos_FS.R print(modelo$variable.importance)
# Numero de nulos en las variables
print(sum(is.na(dtrain$ctrx_quarter)))
print(sum(is.na(dtrain$active_quarter)))
print(sum(is.na(dtrain$mprestamos_personales)))
print(sum(is.na(dtrain$cprestamos_personales)))
print(sum(is.na(dtrain$cdescubierto_preacordado)))
print(sum(is.na(dtrain$mcuentas_saldo)))
print(sum(is.na(dtrain$mactivos_margen)))
print(sum(is.na(dtrain$mcaja_ahorro)))
print(sum(is.na(dtrain$mcuenta_corriente)))
print(sum(is.na(dtrain$ccomisiones_otras)))

# ----- DISPERSION -----
# ctrx_quarter
# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=ctrx_quarter)) + geom_boxplot()
# Vemos la distribución de los deciles
quantile(dtrain$ctrx_quarter, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

# Reduzcamos la enorme disperción usando un logaritmo
dtrain[, ctrx_quarter_2 := log(ctrx_quarter + 1)]

quantile(dtrain$ctrx_quarter_2, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

dtrain[, r_ctrx_quarter := ntile(ctrx_quarter, 10)]

head(dtrain, 10L)

# active_quarter --> Solo tiene 1 y 0
tail(dtrain$active_quarter)

# mprestamos_personales
head(dtrain$mprestamos_personales)
# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=mprestamos_personales)) + geom_boxplot()
# Vemos la distribución de los deciles
quantile(dtrain$mprestamos_personales, probs = c(0,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1))

# Reduzcamos la enorme disperción usando un logaritmo
dtrain[, mprestamos_personales_2 := log(mprestamos_personales + 1)]

quantile(dtrain$mprestamos_personales_2, probs = c(0,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1))

dtrain[, r_mprestamos_personales := ntile(mprestamos_personales, 12)]

head(dtrain, 10L)

# cprestamos_personales
head(dtrain$cprestamos_personales)
# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=cprestamos_personales)) + geom_boxplot()

# cdescubierto_preacordado -- TOdos 0 o 1
head(dtrain$cdescubierto_preacordado)
# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=cdescubierto_preacordado)) + geom_boxplot()

# mcuentas_saldo
head(dtrain$mcuentas_saldo)
# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=mcuentas_saldo)) + geom_boxplot()
# Vemos la distribución de los deciles
quantile(dtrain$mcuentas_saldo, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

# Reduzcamos la enorme disperción usando un logaritmo
dtrain[, mcuentas_saldo_2 := log(mcuentas_saldo + 1)]

# In log(mcuentas_saldo + 1) : NaNs produced
dtrain[is.na(dtrain$mcuentas_saldo_2)] <- 0

quantile(dtrain$mcuentas_saldo_2, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

dtrain[, r_mcuentas_saldo_2 := ntile(mcuentas_saldo_2, 11)]

head(dtrain, 10L)


ctrx_quarter_2, mprestamos_personales_2 eliminar????

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )
