library(tidyverse)
rutaDatos <- "./data/18517.csv"
rutaGraficos <- "./graphs/"
rutaEval <- "./data/eval.csv"
rutaEvalX <- "./data/evalX.csv"

# 1
# Uso un tibble en lugar de un dataframe. Se podria cargar como df con read.csv y despues hacer tibble(df), pero con read_csv me lo ahorro
# hago default a double y despues cambio las que no lo son (sexo, dietaEsp, nivEstPad, nivEstudios y nivIngresos) a factor
df <- read_csv(rutaDatos,
               col_types = cols(
                 .default = col_double(),
                 sexo = col_factor(),
                 dietaEsp = col_factor(),
                 nivEstPad = col_factor(),
                 nivEstudios = col_factor(),
                 nivIngresos = col_factor()
               ))

# 2
# Añadimos una columna IMC que se calcula como peso(kg) / [altura(m) ^ 2]
df$IMC <- df$peso / (df$altura^2)
df

# 3
# Eliminar las filas con NA
df <- na.omit(df)
df

# 4
# media y desv.tipica para cada columna numerica
# para la desviacion tipica, solo tenemos sd, que calcula la QUASIdesviacion
# defino por tanto (desvTipica), que calcula la desviacion que queremos
medias <- df %>% keep(is.numeric) %>% map_dbl(mean)
medias

desvTipica <- function(columna) {
  sqrt(mean((columna - mean(columna))^2))
}

desviaciones <- df %>% keep(is.numeric) %>% map_dbl(desvTipica)
desviaciones

# 5
# defino una funcion para calcular los modelos y valores pedidos de una columna
# despues, hago un map_dlb como en el ejercicio anterior para las columnas solicitadas
variablesPredictoras <- names(df[3:14])

# Separo esta funcion y uso collapse="+" para poder reutilizarla mas adelante
ajusteLineal <- function(x, y, df) {
  lm(str_c(y, "~", str_c(x, collapse="+")), df)
}

calcularCoeficienteReg <- function(x, y, df){
  modelo <- ajusteLineal(x, y, df)
  summary(modelo)$coefficients[2]
}

calcularCoeficienteDet <- function(x, y, df){
  modelo <- ajusteLineal(x, y, df)
  summary(modelo)$r.squared
}

coefRegresion <- variablesPredictoras %>% map_dbl(calcularCoeficienteReg, y="IMC", df=df)
coefRegresion <- tibble(columna=variablesPredictoras, coeficientes=coefRegresion)
coefRegresion

coefDeterminacion <- variablesPredictoras %>% map_dbl(calcularCoeficienteDet, y="IMC", df=df)
coefDeterminacion <- tibble(columna=variablesPredictoras, coeficientes=coefDeterminacion)
coefDeterminacion

# 6
# grafico de dispersion + recta de regresion para numericas
# boxplot para cualitativas
# plot devuelve uno de dispersion para numericas y boxplot para cualitativas
# por lo que solo compruebo si numerica para la recta

dibujarGrafica <- function(nombre_columna){
  jpeg(str_c(rutaGraficos, nombre_columna, ".jpeg"))
  plot(df[[nombre_columna]], df$IMC, xlab = nombre_columna, ylab = "IMC")
  if(is.numeric(df[[nombre_columna]])){
    # Si la columna es numérica, gráfico de dispersión y cálculo del lm para la recta
    mod <- ajusteLineal(nombre_columna, "IMC", df)
    abline(mod)
  }
  dev.off()
}

# Llamamos a la función utilizando walk (para que no devuelva nada, solo haga los graficos)
variablesPredictoras %>% walk(dibujarGrafica)

# 7
# Separar en tres conjuntos
# 60 train, 20 test, 20 valid
separarSets <- function(df, propTrain, propTest){
  rDf    <- 1:nrow(df)
  rTrain <- sample(rDf, propTrain * length(rDf))
  rTemp  <- setdiff(rDf, rTrain)
  rTest  <- sample(rTemp, propTest * length(rTemp))
  rValid <- setdiff(rTemp, rTest)
  
  list(train=df[rTrain,], test=df[rTest,], valid=df[rValid,]) 
}
dfSep <- separarSets(df, 0.6, 0.5)

# 8
# seleccionar de las 12 la que mejor explica el IMC
# funcion para calcular el R2 ajustado de un modelo sobre un df arbitrario
calcR2ajustado <- function(df, modelo, y) {
  MSE  <- mean((df[[y]] - predict.lm(modelo, df)) ^ 2)
  varY <- mean(df[[y]] ^ 2) - mean(df[[y]]) ^ 2
  R2   <- 1 - MSE / varY
  R2ajustado  <- 1 - (1- R2) * (nrow(df) - 1) / (nrow(df) - modelo$rank)
  
  tibble(MSE=MSE, varY=varY, R2=R2, R2ajustado=R2ajustado)
}
# calcular el R2 ajustado de una variable usando train y test
calcModR2 <- function(dfTrain, dfTest, y, x) {
  mod <- ajusteLineal(x, y, dfTrain)
  calcR2ajustado(dfTest, mod, y)$R2ajustado
}

R2ajustado <- variablesPredictoras %>% map_dbl(calcModR2, dfTrain = dfSep$train, dfTest=dfSep$test, y="IMC")
mejorVariable <- variablesPredictoras[which.max(R2ajustado)]
mejorVariable

# 9 seleccionar el mejor modelo
# la siguiente funcion toma como parametros mi df de train y test, y las variables predictoras
# la funcion itera sobre todas las variables, y calcula el mejor r2.
# Añade dicha variable al modelo, y repetimos sobre las variables restantes hasta que le modelo deje de mejorar
encontrarMejorAjuste <- function(dfTrain, dfTest, variables) {
  bestVars <- character(0)
  aR2      <- 0
  
  repeat {
    aR2v <- map_dbl(variables, ~calcModR2(dfTrain, dfTest, "IMC", c(bestVars, .)))
    i    <- which.max(aR2v)
    aR2M <- aR2v[i]
    if (aR2M <= aR2) break
    
    cat(sprintf("%1.4f %s\n", aR2M, variables[i]))
    aR2 <- aR2M
    bestVars <- c(bestVars, variables[i])
    variables   <- variables[-i]
  }
  
  mod <- ajusteLineal(df=dfTrain, y="IMC", x=bestVars)
  
  list(vars=bestVars, mod=mod)
}
modeloMultivariable <- encontrarMejorAjuste(dfSep$train, dfSep$test, variablesPredictoras)

# 10
# evaluacion del modelo

calcR2ajustado(df=dfSep$valid, modelo=modeloMultivariable$mod, y="IMC")

# 11
# Usar el modelo creado para añadir una columna IMC y una columna Peso al df eval
# Cargamos el df "eval"
dfEval <- read_csv(rutaEval,
                   col_types = cols(
                     .default = col_double(),
                     sexo = col_factor(),
                     dietaEsp = col_factor(),
                     nivEstPad = col_factor(),
                     nivEstudios = col_factor(),
                     nivIngresos = col_factor()
                   ))
dfEval$IMC <- predict.lm(modeloMultivariable$mod, dfEval)
dfEval$peso <- dfEval$IMC * (dfEval$altura^2)

write_csv(dfEval, rutaEvalX)

# 12
# Conclusiones sobre el modelo creado
# Utilidad podría tener el modelo matemático que has obtenido
# Qué se puede deducir a partir del modelo sobre la relación entre las variables
# Problemas que has encontrado en el desarrollo
# Qué te ha llamado la atención en el proceso
# Qué más podría hacerse y cómo plantearlo
