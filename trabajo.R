library(tidyverse)
rutaDatos <- "C:/Users/Kal/dev/estadisticaRStudio/data/18517.csv"
rutaGraficos <- "C:/Users/Kal/dev/estadisticaRStudio/graphs/"
rutaEval <- "C:/Users/Kal/dev/estadisticaRStudio/data/eval.csv"

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
calcularCoeficienteReg <- function(columna){
  modelo <- lm(df$IMC ~ columna)
  coeficienteRegresion <- summary(modelo)$coefficients[2]
}

coefRegresion <- df[3:14] %>% map_dbl(calcularCoeficienteReg)
coefRegresion

calcularCoeficienteDet <- function(columna){
  modelo <- lm(df$IMC ~ columna)
  coefDeterminacion <- summary(modelo)$r.squared
}

coefDeterminacion <- df[3:14] %>% map_dbl(calcularCoeficienteDet)
coefDeterminacion

# 6
# grafico de dispersion + recta de regresion para numericas
# boxplot para cualitativas
# plot devuelve uno de dispersion para numericas y boxplot para cualitativas
# por lo que solo compruebo si numerica para la recta

dibujarGrafica <- function(columna, nombre_columna){
  jpeg(str_c(rutaGraficos, nombre_columna, ".jpeg"))
  plot(columna, df$IMC, xlab = nombre_columna, ylab = "IMC")
  if(is.numeric(columna)){
    # Si la columna es numérica, gráfico de dispersión y cálculo del lm para la recta
    mod <- lm(df$IMC ~ columna)
    abline(mod)
  }
  dev.off()
}

# Llamamos a la función utilizando iwalk (para que no devuelva nada, solo haga los graficos)
# uso iwalk en lugar de walk para poder pasar los nombres de las columnas
df[3:14] %>% iwalk(dibujarGrafica)
