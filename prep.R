
library(dplyr)
library(ggplot2)
library(nnet)        # para regresión multinomial
library(randomForest) # para Random Forest
library(caret)        # para validación cruzada
library(mice)         # para imputación múltiple si hace falta
library(DescTools)  # para Pseudo R²
library(car)        # para VIF
library(lsr)         # para Cramér's V
library(margins)    # para efectos marginales

# Cargar datos
datos <- read.csv("base_desenlace.csv", sep = ";")

# Convierte a factor y elimina niveles vacíos
datos$desenlace <- factor(datos$desenlace)
datos$sexo <- factor(datos$sexo)
datos$patologia <- factor(datos$patologia)
datos$ccaa <- factor(datos$ccaa)
datos$tipo_procedimiento <- factor(datos$tipo_procedimiento)

# Convierte a factor, ordenando según el orden que se establece en levels, sino sería alfabético (y se ordenaría diferente)
# Agrupar grupos problemáticos
datos$edad_agrupada <- factor(
  ifelse(datos$edad %in% c("<30", "30-39"), "(<40)",
  ifelse(datos$edad %in% c("40-49", "50-59"), "(40-59)", 
  ifelse(datos$edad %in% c("60-69", "70-79"), "(60-79)",
         "(>80)"))),
  levels = c("(40-59)", "(60-79)", "(<40)", "(>80)")
)

# Establece "prestacion" como nivel de referencia para la regresion multinomial, que usara "desenlace" como variable dependiente
datos$desenlace <- relevel(datos$desenlace, ref = "prestacion")

# Establece "no" como nivel de referencia para la regresión multinomial, que usara "adelanto_segunda_solicitud" como variable independiente
datos$adelanto_segunda_solicitud <- factor(datos$adelanto_segunda_solicitud, levels = c("no", "si"))

# EXPLORACIÓN INCIAL
## Comprobar la dimensión de los datos
dim(datos)

## Conteo de los desenlaces, en valor absoluto y en proporción
table(datos$desenlace)
prop.table(table(datos$desenlace))

# Comprobar si existen valores perdidos por variable
colSums(is.na(datos))

# Analizar Multicolinealidad con V de Cramer (evalúa dependencia entre variables categóricas)
# Selecciona las variables explicativas categóricas
vars_cat <- datos %>% 
  select(sexo, edad, patologia, ccaa, tipo_procedimiento, adelanto_segunda_solicitud)

# Calcula el V de Cramér entre todas las combinaciones de pares
comb <- combn(names(vars_cat), 2)
cramer_results <- apply(comb, 2, function(x) {
  v <- cramersV(vars_cat[[x[1]]], vars_cat[[x[2]]])
  data.frame(var1 = x[1], var2 = x[2], V = v)
}) %>%
  do.call(rbind, .) %>%
  arrange(desc(V))

# Muestra las 10 asociaciones más altas
print(head(cramer_results, 10))

# Identifica posibles pares con colinealidad fuerte (V > 0.5)
colineales <- cramer_results %>% filter(V > 0.5)
if (nrow(colineales) == 0) {
  cat("No existen asociaciones fuertes (V > 0.5) entre variables categóricas.\n")
} else {
  cat("Existen posibles variables colineales (V > 0.5):\n")
  print(colineales)
}


