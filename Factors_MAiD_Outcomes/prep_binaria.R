
setwd("c:/Users/Usuario/Desktop/INEDyTO/ICEL 5 - Australia/Factors_MAiD_Outcomes")

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
library(ResourceSelection) # para test de Hosmer-Lemeshow
library(pROC) # para ROC y AUC


# Cargar datos
datos <- read.csv("base_desenlace_binaria.csv", sep = ";", encoding = "UTF-8")

# EXPLORACIÓN INCIAL
## Comprobar la dimensión de los datos
dim(datos)

## Conteo de los desenlaces, en valor absoluto y en proporción
table(datos$desenlace)
prop.table(table(datos$desenlace))

## Comprobar si existen valores perdidos por variable
colSums(is.na(datos))

## Convierte a factor y elimina niveles vacíos
datos$desenlace <- factor(datos$desenlace)
datos$sexo <- factor(datos$sexo)
datos$patologia <- factor(datos$patologia)
datos$ccaa <- factor(datos$ccaa)
datos$tipo_procedimiento <- factor(datos$tipo_procedimiento)
datos$adelanto_segunda_solicitud <- factor(datos$adelanto_segunda_solicitud)


## Crear nueva variable de edad agrupada
datos$edad_agrupada <- factor(
  ifelse(datos$edad %in% c("<30", "30-39"), "(<40)",
  ifelse(datos$edad %in% c("40-49", "50-59"), "(40-59)", 
  ifelse(datos$edad %in% c("60-69", "70-79"), "(60-79)",
         "(>80)"))),
  levels = c("(<40)", "(40-59)", "(60-79)", "(>80)")
)

## Crear nueva variable de patología agrupada
datos$patologia_agrupada <- factor(
  ifelse(datos$patologia == "neurologica", "neurologica",
  ifelse(datos$patologia == "oncologica", "oncologica", 
  ifelse(datos$patologia %in% c("cardiovascular", "respiratoria"), "cardiopulmonar",
  ifelse(datos$patologia == "pluripatologia", "pluripatologia",
         "otra/Desconocida")))),
  levels = c("oncologica", "neurologica", "cardiopulmonar", "pluripatologia", "otra/Desconocida")
)

## Crear nueva variable de CCAA agrupada
datos$ccaa_agrupada <- factor(
  ifelse(datos$ccaa == "Catalu�a", "Cataluña",
  ifelse(datos$ccaa == "Madrid", "Madrid",
  ifelse(datos$ccaa == "Pais Vasco", "País Vasco",
  ifelse(datos$ccaa == "Andalucia", "Andalucía", 
  ifelse(datos$ccaa == "Canarias", "Canarias",
  ifelse(datos$ccaa == "Comunidad Valenciana", "C. Valenciana",
  ifelse(datos$ccaa == "Islas Baleares", "I. Baleares",
  ifelse(datos$ccaa == "Castilla y Leon", "Castilla y León",
  ifelse(datos$ccaa == "Galicia", "Galicia",
  ifelse(datos$ccaa == "Navarra", "Navarra",
  ifelse(datos$ccaa == "Asturias", "Asturias",
  ifelse(datos$ccaa == "Aragon", "Aragón",
  ifelse(datos$ccaa == "Castilla-La Mancha", "Castilla-La Mancha",
         "Otras CCAA"))))))))))))),
  levels = c("Cataluña", "Madrid", "País Vasco", "Andalucía", "Canarias", 
             "C. Valenciana", "I. Baleares", "Castilla y León", "Galicia", 
             "Navarra", "Asturias", "Aragón", "Castilla-La Mancha", "Otras CCAA")
)


# Verificar las nuevas agrupaciones
names(datos)
table(datos$edad_agrupada)
prop.table(table(datos$edad_agrupada))
table(datos$patologia_agrupada)
table(datos$ccaa_agrupada)


