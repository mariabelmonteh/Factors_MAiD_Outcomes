
# REGRESIÓN LOGÍSTICA MULTINOMIAL CON REVOCACIÓN (más de 2 categorías en la variable dependiente)
## Establece "desenlace" como variable dependiente, y las demás como independientes. Na.omit elimina filas con valores perdidos (no hay pero por si acaso)
modelo_multinom <- multinom(desenlace ~ edad_agrupada + sexo + patologia + ccaa + 
                            tipo_procedimiento + adelanto_segunda_solicitud,
                            data = datos, na.action = na.omit)

# COEFICIENTES Y ODDS RATIOS
## Extrae los coeficientes beta del modelo (son dificiles de interpretar directamente porque están en escala logarítmica)
coef(modelo_multinom)

## Por eso se aplica la función exponencial a los coeficientes para convertirlos en Odds Ratios, que son más fáciles de interpretar. Los OR dicen cuánto cambian las probabilidades de cada desenlace cuando cambia una variable predictora:
### OR = 1: No hay efecto
### OR > 1: Aumenta la probabilidad (ej: OR = 2 → duplica las probabilidades)
### OR < 1: Disminuye la probabilidad (ej: OR = 0.5 → reduce a la mitad)
exp(coef(modelo_multinom)) 

# INTERVALOS DE CONFIANZA al 95%
## Los intervalos de confianza indican el rango donde probablemente está el verdadero valor del OR
## Si el intervalo NO incluye 1 → el efecto es estadísticamente significativo
## Si el intervalo incluye 1 → no hay evidencia de efecto significativo
## Intervalos más estrechos = estimaciones más precisas
se <- summary(modelo_multinom)$standard.errors
z <- qnorm(0.975)
IC_inf <- exp(coef(modelo_multinom) - z * se)
IC_sup <- exp(coef(modelo_multinom) + z * se)
IC <- list(inferior = IC_inf, superior = IC_sup)
IC

# SIGNIFICANCIA ESTADÍSTICA (p-valores)
## Evalúa qué coeficientes son estadísticamente significativos
summary_modelo <- summary(modelo_multinom)
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))  # Bilateral

## Combina todo en una tabla resumen ordenada y legible
resultado_coef <- data.frame(
  Categoria = rep(rownames(coef(modelo_multinom)), each = ncol(coef(modelo_multinom))),
  Variable = rep(colnames(coef(modelo_multinom)), times = nrow(coef(modelo_multinom))),
  Coef = as.vector(coef(modelo_multinom)),
  OR = as.vector(exp(coef(modelo_multinom))),
  IC_inf = as.vector(IC_inf),
  IC_sup = as.vector(IC_sup),
  z = as.vector(z_values),
  p = as.vector(p_values)
)

## Muestra las 10 asociaciones más relevantes ordenadas por significancia
resultado_coef <- resultado_coef %>% arrange(p)
print(head(resultado_coef, 10))

# PSEUDO R²
## En regresión logística multinomial se usan diferentes tipos de Pseudo R² para medir el ajuste del modelo
## Es recomendable reportar ambos para tener una visión completa del rendimiento

## McFadden R²: Más conservador, valores típicos 0.2-0.4 = muy buen ajuste
PseudoR2(modelo_multinom, which = "McFadden")

## Nagelkerke R²: Más intuitivo (rango 0-1), predominante en ciencias de la salud
### 0: El modelo no explica nada, 1: explica perfectamente
### 0.2-0.4: Ajuste moderado, >0.4: Buen ajuste
PseudoR2(modelo_multinom, which = "Nagelkerke")

# DIAGNÓSTICO DE RESIDUOS Y OBSERVACIONES INFLUYENTES
## Residuos de devianza
residuos_dev <- residuals(modelo_multinom, type = "deviance")

hist(residuos_dev, breaks = 20, col = "lightblue",
     main = "Distribución de los residuos de devianza",
     xlab = "Residuos de devianza")

## Residuos de Pearson
residuos_pearson <- residuals(modelo_multinom, type = "pearson")

plot(residuos_pearson, pch = 20, col = "darkblue",
     main = "Residuos de Pearson",
     ylab = "Residuo", xlab = "Observación")
abline(h = 0, lty = 2, col = "red")

## Evaluación de ajuste por probabilidad máxima
p_max <- apply(fitted(modelo_multinom), 1, max)

plot(p_max, pch = 20, col = "tomato",
     main = "Probabilidades máximas por observación",
     ylab = "Probabilidad máxima de la clase predicha",
     xlab = "Observación")
abline(h = c(0.05, 0.95), col = "red", lty = 2)

## Detección de outliers o observaciones influyentes
outliers <- which(abs(residuos_dev) > 2 | p_max < 0.05 | p_max > 0.95)
cat("Número de posibles outliers o observaciones influyentes:", length(outliers), "\n")
if (length(outliers) > 0) print(outliers)

# PREDICCIONES Y MÉTRICAS DE RENDIMIENTO
## Predicciones sobre los datos usados en el modelo, seleccionando solo filas sin NA. Esto es necesario para comparar con los valores reales, es decir, sabiendo que el modelo explica un 30% de la variabilidad, ¿cómo de bien lo hace? Esto se calcula con las diferentes métricas asociadas a la predicción.
datos_modelo <- na.omit(datos)
pred <- predict(modelo_multinom, newdata = datos_modelo, type = "class")

## Matriz de confusión completa 
## Esta función calcula automáticamente: Accuracy, Kappa, Sensitivity, Specificity, Precision, F1-Score. También muestra la tabla de confusión y intervalos de confianza
conf_matrix <- table(Predicho = pred, Real = datos_modelo$desenlace)
confusionMatrix(conf_matrix)


# ==========================================================
# REGRESIÓN LOGÍSTICA MULTINOMIAL SIN LA CATEGORÍA "revocacion"
## Crear subconjunto de datos sin la categoría 'revocacion'
datos_sin_revocacion <- subset(datos, desenlace != "revocacion")

## Eliminar niveles no utilizados del factor
datos_sin_revocacion$desenlace <- droplevels(datos_sin_revocacion$desenlace)

# Verificar distribución edad_agrupada x desenlace
table(datos_sin_revocacion$edad_agrupada, datos_sin_revocacion$desenlace)
# Verificar distribución patología x desenlace
table(datos_sin_revocacion$patologia, datos_sin_revocacion$desenlace)
# Verificar distribución tipo_procedimiento x desenlace
table(datos_sin_revocacion$tipo_procedimiento, datos_sin_revocacion$desenlace)

# Cambiar referencia de CCAA a Cataluña
datos_sin_revocacion$ccaa <- relevel(
  datos_sin_revocacion$ccaa, 
  ref = "Catalu\xf1a"  
)

# Cambiar referencia a oncológica
datos_sin_revocacion$patologia <- relevel(
  datos_sin_revocacion$patologia, 
  ref = "oncologica")

# Cambiar referencia de edad a 60-79 años
datos_sin_revocacion$edad_agrupada <- relevel(
  datos_sin_revocacion$edad_agrupada, 
  ref = "(60-79)"  
)

# Cambiar referencia de tipo_procedimiento a primera solicitud
datos_sin_revocacion$tipo_procedimiento <- relevel(
  datos_sin_revocacion$tipo_procedimiento, 
  ref = "primera solicitud"  
)

# Verificar el cambio
levels(datos_sin_revocacion$ccaa)
levels(datos_sin_revocacion$patologia)
levels(datos_sin_revocacion$edad_agrupada)
levels(datos_sin_revocacion$tipo_procedimiento)

## Ajustar el modelo multinomial
modelo_multinom_sin_rev <- multinom(
  desenlace ~ edad_agrupada + sexo + patologia + ccaa + tipo_procedimiento + adelanto_segunda_solicitud,
  data = datos_sin_revocacion,
  na.action = na.omit,
  model = TRUE,
  Hess = TRUE)

# Modelo nulo (solo intercepto)
modelo_nulo <- multinom(desenlace ~ 1, data = datos_sin_revocacion, trace = FALSE)

# TEST DE SIGNIFICANCIA GLOBAL (Razón de verosimilitud)
## Compara el modelo completo vs modelo nulo (solo intercepto) para evaluar si las variables predictoras en conjunto tienen efecto significativo
lrt_stat <- 2 * (logLik(modelo_multinom_sin_rev) - logLik(modelo_nulo))
df_diff <- df.residual(modelo_nulo) - df.residual(modelo_multinom_sin_rev)
p_value <- 1 - pchisq(lrt_stat, df_diff)

cat("Test de razón de verosimilitud (Significancia Global):\n")
cat("LRT statistic:", lrt_stat, "\n")
cat("df:", df_diff, "\n") 
cat("p-value:", p_value, "\n")
if (p_value < 0.001) {
  cat("*** El modelo es altamente significativo (p < 0.001)\n")
  cat("*** Las variables predictoras explican significativamente la variabilidad en los desenlaces\n")
} else if (p_value < 0.05) {
  cat("** El modelo es significativo (p < 0.05)\n")
} else {
  cat("El modelo no es significativo (p >= 0.05)\n")
}

# COEFICIENTES Y ODDS RATIOS
coef(modelo_multinom_sin_rev)
exp(coef(modelo_multinom_sin_rev))

# INTERVALOS DE CONFIANZA AL 95%
## Valores extremos (>1000) y NaN indican separación perfecta causada por:
## - CCAA con pocos casos (ej: Melilla, La Rioja)
## - Combinaciones raras de patología+CCAA
## - Estos intervalos NO son confiables para interpretación

## IDENTIFICAR CATEGORÍAS PROBLEMÁTICAS
cat("=== DIAGNÓSTICO DE CATEGORÍAS PEQUEÑAS ===\n")
cat("Distribución de CCAA:\n")
ccaa_counts <- table(datos_sin_revocacion$ccaa)
print(ccaa_counts)
ccaa_pequenas <- names(ccaa_counts)[ccaa_counts < 15]
cat("\nCCAA con <15 casos (problemáticas):", paste(ccaa_pequenas, collapse = ", "), "\n")

cat("\nDistribución de Patologías:\n")
pat_counts <- table(datos_sin_revocacion$patologia)
print(pat_counts)
pat_pequenas <- names(pat_counts)[pat_counts < 15]
cat("\nPatologías con <15 casos (problemáticas):", paste(pat_pequenas, collapse = ", "), "\n\n")

## CREAR DATOS AGRUPADOS SOLO PARA INTERVALOS DE CONFIANZA (Agrupo las categorías con baja frecuencia (<15 casos) para garantizar la estabilidad numérica del modelo.)
cat("=== CREANDO VERSIÓN AGRUPADA PARA IC ESTABLES ===\n")
datos_ic <- datos_sin_revocacion

# Agrupar CCAA pequeñas
datos_ic$ccaa_agrupada <- ifelse(
  datos_ic$ccaa %in% ccaa_pequenas,
  "Otras CCAA",
  as.character(datos_ic$ccaa)
)
datos_ic$ccaa_agrupada <- factor(datos_ic$ccaa_agrupada)

# Agrupar patologías pequeñas  
datos_ic$patologia_agrupada <- ifelse(
  datos_ic$patologia %in% pat_pequenas,
  "Otras patologias",
  as.character(datos_ic$patologia)
)
datos_ic$patologia_agrupada <- factor(datos_ic$patologia_agrupada)

cat("CCAA después de agrupar:\n")
print(table(datos_ic$ccaa_agrupada))
cat("\nPatologías después de agrupar:\n")
print(table(datos_ic$patologia_agrupada))

## MODELO ESPECÍFICO PARA INTERVALOS DE CONFIANZA
cat("\n=== AJUSTANDO MODELO CON CATEGORÍAS AGRUPADAS ===\n")
modelo_ic <- multinom(
  desenlace ~ edad + sexo + patologia_agrupada + ccaa_agrupada + 
  tipo_procedimiento + adelanto_segunda_solicitud,
  data = datos_ic,
  na.action = na.omit,
  trace = FALSE
)

## CALCULAR INTERVALOS DE CONFIANZA ESTABLES
se_ic <- summary(modelo_ic)$standard.errors
z <- qnorm(0.975)
IC_inf_estables <- exp(coef(modelo_ic) - z * se_ic)
IC_sup_estables <- exp(coef(modelo_ic) + z * se_ic)

# Verificar si hay problemas numéricos
valores_extremos_ic <- which(IC_inf_estables < 1e-5 | IC_sup_estables > 1e5 | 
                            is.nan(IC_inf_estables) | is.nan(IC_sup_estables))

cat("RESULTADO:\n")
if (length(valores_extremos_ic) == 0) {
  cat("Intervalos de confianza ESTABLES calculados exitosamente\n")
  cat("No se detectaron problemas numéricos\n")
} else {
  cat("Aún hay", length(valores_extremos_ic), "coeficientes problemáticos\n")
}

IC_estables <- list(inferior = IC_inf_estables, superior = IC_sup_estables)
print("Intervalos de confianza con categorías agrupadas:")
IC_estables

# SIGNIFICANCIA ESTADISTICA (p-valores)
## Combina coeficientes del modelo original con IC estables del modelo agrupado
# Calcular p-valores del modelo original (para variables originales)
summary_modelo <- summary(modelo_multinom_sin_rev)
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))  # Prueba bilateral

# Crear tabla con variables originales pero IC estables (cuando sea posible)
resultado_coef <- data.frame(
  Categoria = rep(rownames(coef(modelo_multinom_sin_rev)), each = ncol(coef(modelo_multinom_sin_rev))),
  Variable = rep(colnames(coef(modelo_multinom_sin_rev)), times = nrow(coef(modelo_multinom_sin_rev))),
  Coef = as.vector(coef(modelo_multinom_sin_rev)),
  OR = as.vector(exp(coef(modelo_multinom_sin_rev))),
  z = as.vector(z_values),
  p = as.vector(p_values)
)

# Agregar significancia
resultado_coef$significancia <- ifelse(resultado_coef$p < 0.001, "***",
                                     ifelse(resultado_coef$p < 0.01, "**",
                                          ifelse(resultado_coef$p < 0.05, "*", "")))

# Ordenar por significancia y mostrar los más relevantes
resultado_coef <- resultado_coef %>% arrange(p)

cat("=== TOP 10 VARIABLES MÁS SIGNIFICATIVAS (para el paper) ===\n")
print(head(resultado_coef[,c("Categoria", "Variable", "OR", "p", "significancia")], 10))

cat("\nNOTA: Para intervalos de confianza confiables, usar los calculados con categorías agrupadas\n")
cat("*** p < 0.001, ** p < 0.01, * p < 0.05\n")

# EFECTOS MARGINALES MANUALES (interpretación en probabilidades)
# Calcular probabilidades predichas para el conjunto de datos
prob_pred <- predict(modelo_multinom_sin_rev, type = "probs")

# Función para calcular efectos marginales de variables categóricas
calcular_efecto_marginal_categorica <- function(modelo, datos, variable, valor_base, valor_comparacion) {
  # Crear datos con valor base
  datos_base <- datos
  datos_base[[variable]] <- valor_base
  
  # Crear datos con valor de comparación
  datos_comp <- datos
  datos_comp[[variable]] <- valor_comparacion
  
  # Calcular probabilidades
  prob_base <- predict(modelo, newdata = datos_base, type = "probs")
  prob_comp <- predict(modelo, newdata = datos_comp, type = "probs")
  
  # Calcular diferencias promedio
  diferencias <- colMeans(prob_comp - prob_base, na.rm = TRUE)
  return(diferencias)
}

# EFECTOS MARGINALES PARA VARIABLES CLAVE
## EFECTO DEL SEXO (Mujer vs Hombre)
efecto_sexo <- calcular_efecto_marginal_categorica(
  modelo_multinom_sin_rev, 
  datos_sin_revocacion, 
  "sexo", 
  "Hombre", 
  "Mujer"
)
print(efecto_sexo)
cat("Interpretación: Ser mujer cambia las probabilidades en:\n")
for(i in 1:length(efecto_sexo)) {
  cat(sprintf("  %s: %+.3f (%.1f%% más/menos probable)\n", 
              names(efecto_sexo)[i], 
              efecto_sexo[i], 
              efecto_sexo[i] * 100))
}

## EFECTO DEL TIPO DE PROCEDIMIENTO
print(table(datos_sin_revocacion$adelanto_segunda_solicitud))

efecto_adelanto <- calcular_efecto_marginal_categorica(
  modelo_multinom_sin_rev,
  datos_sin_revocacion,
  "adelanto_segunda_solicitud",
  "no",  # valor base
  "si"   # valor de comparación
)
print(efecto_adelanto)
cat("Interpretación: Adelanto segunda solicitud (vs no adelanto) cambia probabilidades en:\n")
for(i in 1:length(efecto_adelanto)) {
  cat(sprintf("  %s: %+.3f (%.1f%% más/menos probable)\n", 
              names(efecto_adelanto)[i], 
              efecto_adelanto[i], 
              efecto_adelanto[i] * 100))
}

## 3. PROBABILIDADES PROMEDIO POR GRUPO (más interpretable)
# Por sexo
cat("\nPor SEXO:\n")
prob_por_sexo <- aggregate(prob_pred, by = list(datos_sin_revocacion$sexo), mean, na.rm = TRUE)
names(prob_por_sexo)[1] <- "Sexo"
print(prob_por_sexo)

# Por patología (top 3)
cat("\nPor PATOLOGÍA (principales):\n")
patologias_principales <- c("oncologica", "neurologica", "cardiovascular")
datos_pat_principales <- subset(datos_sin_revocacion, patologia %in% patologias_principales)
prob_datos_pat <- predict(modelo_multinom_sin_rev, newdata = datos_pat_principales, type = "probs")
prob_por_patologia <- aggregate(prob_datos_pat, by = list(datos_pat_principales$patologia), mean, na.rm = TRUE)
names(prob_por_patologia)[1] <- "Patologia"
print(prob_por_patologia)

# Por CCAA (principales)
cat("\nPor CCAA (principales):\n")
ccaa_principales <- c("Andalucia", "Madrid", "Cataluña", "Pais Vasco")
datos_ccaa_principales <- subset(datos_sin_revocacion, ccaa %in% ccaa_principales)
if(nrow(datos_ccaa_principales) > 0) {
  prob_datos_ccaa <- predict(modelo_multinom_sin_rev, newdata = datos_ccaa_principales, type = "probs")
  prob_por_ccaa <- aggregate(prob_datos_ccaa, by = list(datos_ccaa_principales$ccaa), mean, na.rm = TRUE)
  names(prob_por_ccaa)[1] <- "CCAA"
  print(prob_por_ccaa)
}

cat("\n=== INTERPRETACIÓN ===\n")
cat("Los efectos marginales muestran cambios en probabilidades predichas:\n")
cat("- Valores positivos: aumentan la probabilidad de ese desenlace\n")
cat("- Valores negativos: disminuyen la probabilidad de ese desenlace\n")
cat("- La suma de cambios por fila debe ser aproximadamente 0\n")

# PSEUDO R²
PseudoR2(modelo_multinom_sin_rev, which = "McFadden")
PseudoR2(modelo_multinom_sin_rev, which = "Nagelkerke")

# DIAGNÓSTICO DE RESIDUOS Y OBSERVACIONES INFLUYENTES
residuos_dev <- residuals(modelo_multinom_sin_rev, type = "deviance")

hist(residuos_dev, breaks = 20, col = "lightblue",
     main = "Distribución de los residuos de devianza (sin revocación)",
     xlab = "Residuos de devianza")

residuos_pearson <- residuals(modelo_multinom_sin_rev, type = "pearson")

plot(residuos_pearson, pch = 20, col = "darkblue",
     main = "Residuos de Pearson (sin revocación)",
     ylab = "Residuo", xlab = "Observación")
abline(h = 0, lty = 2, col = "red")

p_max <- apply(fitted(modelo_multinom_sin_rev), 1, max)

plot(p_max, pch = 20, col = "tomato",
     main = "Probabilidades máximas por observación (sin revocación)",
     ylab = "Probabilidad máxima de la clase predicha",
     xlab = "Observación")
abline(h = c(0.05, 0.95), col = "red", lty = 2)

outliers <- which(abs(residuos_dev) > 2 | p_max < 0.05 | p_max > 0.95)
cat("Número de posibles outliers o observaciones influyentes:", length(outliers), "\n")
if (length(outliers) > 0) print(outliers)

mean(abs(residuos_dev))


# PREDICCIONES Y MÉTRICAS DE RENDIMIENTO
datos_modelo_sin_rev <- na.omit(datos_sin_revocacion)
pred_sin_rev <- predict(modelo_multinom_sin_rev, newdata = datos_modelo_sin_rev, type = "class")

conf_matrix_sin_rev <- table(Predicho = pred_sin_rev, Real = datos_modelo_sin_rev$desenlace)
confusionMatrix(conf_matrix_sin_rev)




#AIC y BIC
# Modelo con revocación
AIC(modelo_multinom)
BIC(modelo_multinom)

# Modelo sin revocación
AIC(modelo_multinom_sin_rev)
BIC(modelo_multinom_sin_rev)



