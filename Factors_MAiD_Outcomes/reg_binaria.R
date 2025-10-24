
##TABLAS DE CONTINGENCIA

# Edad x desenlace
table(datos$edad, datos$desenlace)
# Edad agrupada x desenlace
table(datos$edad_agrupada, datos$desenlace)
# Patología x desenlace
table(datos$patologia, datos$desenlace)
# Patología agrupada x desenlace
table(datos$patologia_agrupada, datos$desenlace)
# Tipo de procedimiento x desenlace
table(datos$tipo_procedimiento, datos$desenlace)
# Comunidad Autónoma x desenlace
table(datos$ccaa, datos$desenlace)  
# Comunidad Autónoma agrupada x desenlace
table(datos$ccaa_agrupada, datos$desenlace)
# Sexo xs desenlace
table(datos$sexo, datos$desenlace)  
# Adelanto segunda solicitud x desenlace
table(datos$adelanto_segunda_solicitud, datos$desenlace)




## COMPARACIÓN MODELOS: EDAD ACTUAL VS EDAD AGRUPADA
# Establecer categoría de referencia para la nueva variable
datos$edad_agrupada <- relevel(datos$edad_agrupada, ref = "(>80)")
table(datos$edad_agrupada)

# Modelo 1: Edad original
modelo_edad_original <- glm(desenlace ~ edad, data = datos, family = binomial)

# Modelo 2: Edad agrupada
modelo_edad_agrupada <- glm(desenlace ~ edad_agrupada, data = datos, family = binomial)

# Criterios de información
cat("MODELO EDAD ORIGINAL:\n")
cat("AIC:", AIC(modelo_edad_original), "\n")
cat("BIC:", BIC(modelo_edad_original), "\n")
cat("Deviance:", deviance(modelo_edad_original), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_edad_original, which = "McFadden"), "\n\n")

cat("MODELO EDAD AGRUPADA:\n")
cat("AIC:", AIC(modelo_edad_agrupada), "\n")
cat("BIC:", BIC(modelo_edad_agrupada), "\n")
cat("Deviance:", deviance(modelo_edad_agrupada), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_edad_agrupada, which = "McFadden"), "\n\n")

# ANOVA (Test de razón de verosimilitud)
cat("=== ANOVA - LIKELIHOOD RATIO TEST ===\n")
anova_resultado <- anova(modelo_edad_agrupada, modelo_edad_original, test = "Chisq")
print(anova_resultado)

# Resúmenes detallados
cat("\n=== RESUMEN MODELO EDAD ORIGINAL ===\n")
summary(modelo_edad_original)

cat("\n=== RESUMEN MODELO EDAD AGRUPADA ===\n")
summary(modelo_edad_agrupada)




## COMPARACIÓN MODELOS: PATOLOGÍA ACTUAL VS PATOLOGÍA AGRUPADA
# Establecer categoría de referencia para la nueva variable
datos$patologia_agrupada <- relevel(datos$patologia_agrupada, ref = "neurologica")
table(datos$patologia_agrupada)

# Modelo 1: Patología original
modelo_patologia_original <- glm(desenlace ~ patologia, data = datos, family = binomial)

# Modelo 2: Patología agrupada
modelo_patologia_agrupada <- glm(desenlace ~ patologia_agrupada, data = datos, family = binomial)

# Criterios de información
cat("MODELO PATOLOGÍA ORIGINAL:\n")
cat("AIC:", AIC(modelo_patologia_original), "\n")
cat("BIC:", BIC(modelo_patologia_original), "\n")
cat("Deviance:", deviance(modelo_patologia_original), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_patologia_original, which = "McFadden"), "\n\n")

cat("MODELO PATOLOGÍA AGRUPADA:\n")
cat("AIC:", AIC(modelo_patologia_agrupada), "\n")
cat("BIC:", BIC(modelo_patologia_agrupada), "\n")
cat("Deviance:", deviance(modelo_patologia_agrupada), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_patologia_agrupada, which = "McFadden"), "\n\n")

# ANOVA (Test de razón de verosimilitud)
cat("=== ANOVA - LIKELIHOOD RATIO TEST ===\n")
anova_resultado <- anova(modelo_patologia_agrupada, modelo_patologia_original, test = "Chisq")
print(anova_resultado)

# Resúmenes detallados
cat("\n=== RESUMEN MODELO PATOLOGÍA ORIGINAL ===\n")
summary(modelo_patologia_original)

cat("\n=== RESUMEN MODELO PATOLOGÍA AGRUPADA ===\n")
summary(modelo_patologia_agrupada)




## COMPARACIÓN MODELOS: CCAA ACTUAL VS CCAA AGRUPADA
# Establecer categoría de referencia para la nueva variable     
datos$ccaa_agrupada <- relevel(datos$ccaa_agrupada, ref = "Cataluña")
table(datos$ccaa_agrupada)

# Modelo 1: CCAA original
modelo_ccaa_original <- glm(desenlace ~ ccaa, data = datos, family = binomial)

# Modelo 2: CCAA agrupada
modelo_ccaa_agrupada <- glm(desenlace ~ ccaa_agrupada, data = datos, family = binomial)

# Criterios de información
cat("MODELO CCAA ORIGINAL:\n")
cat("AIC:", AIC(modelo_ccaa_original), "\n")    
cat("BIC:", BIC(modelo_ccaa_original), "\n")
cat("Deviance:", deviance(modelo_ccaa_original), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_ccaa_original, which = "McFadden"), "\n\n")

cat("MODELO CCAA AGRUPADA:\n")
cat("AIC:", AIC(modelo_ccaa_agrupada), "\n")
cat("BIC:", BIC(modelo_ccaa_agrupada), "\n")
cat("Deviance:", deviance(modelo_ccaa_agrupada), "\n")
cat("Pseudo R² (McFadden):", PseudoR2(modelo_ccaa_agrupada, which = "McFadden"), "\n\n")

# ANOVA (Test de razón de verosimilitud)
cat("=== ANOVA - LIKELIHOOD RATIO TEST ===\n")
anova_resultado <- anova(modelo_ccaa_agrupada, modelo_ccaa_original, test = "Chisq")
print(anova_resultado)

# Resúmenes detallados
cat("\n=== RESUMEN MODELO CCAA ORIGINAL ===\n")
summary(modelo_ccaa_original)

cat("\n=== RESUMEN MODELO CCAA AGRUPADA ===\n")
summary(modelo_ccaa_agrupada)




## MODELO DE REGRESIÓN LOGÍSTICA BINARIA 

# ESTABLECER CATEGORÍAS DE REFERENCIA
datos$edad_agrupada <- relevel(datos$edad_agrupada, ref = "(>80)")
datos$sexo <- relevel(datos$sexo, ref = "Hombre")
datos$ccaa_agrupada <- relevel(datos$ccaa_agrupada, ref = "Cataluña")
datos$patologia_agrupada <- relevel(datos$patologia_agrupada, ref = "oncologica")
datos$tipo_procedimiento <- relevel(datos$tipo_procedimiento, ref = "primera solicitud")
datos$adelanto_segunda_solicitud <- relevel(datos$adelanto_segunda_solicitud, ref = "no")

# AJUSTAR MODELOS
## Modelo nulo (solo intercepto)
modelo_nulo <- glm(desenlace ~ 1, data = datos, family = binomial)

## Modelo completo con todas las variables agrupadas
modelo_binomial <- glm(desenlace ~ sexo + edad_agrupada + patologia_agrupada + 
                      ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud, 
                      data = datos, family = binomial)

# Resumen del modelo
summary(modelo_binomial)

# TEST DE SIGNIFICANCIA GLOBAL
anova(modelo_nulo, modelo_binomial, test = "Chisq")

# VERIFICACIÓN DE MULTICOLINEALIDAD
cat("\n=== MULTICOLINEALIDAD (GVIF) ===\n")
vif_values <- vif(modelo_binomial)
print(vif_values)

# Interpretación de los valores de GVIF
if(any(vif_values[, "GVIF^(1/(2*Df))"] > 2.5)) {
  cat("Posible multicolinealidad (GVIF > 2.5)\n")
} else {
  cat("No hay problemas de multicolinealidad\n")
}

# BONDAD DE AJUSTE
cat("=== PSEUDO R² COMPARATIVO ===\n")
pseudo_r2 <- PseudoR2(modelo_binomial, which = c("McFadden", "CoxSnell", "Nagelkerke"))
print(pseudo_r2)

cat("\n=== TEST DE HOSMER-LEMESHOW ===\n")
hl_test <- hoslem.test(datos$desenlace_bin, fitted(modelo_binomial))
print(hl_test)

# Test de calibración alternativo (menos sensible)
cat("\n=== ANÁLISIS DE CALIBRACIÓN COMPLEMENTARIO ===\n")

## Predicciones
predicciones_prob <- predict(modelo_binomial, type = "response")

# Crear grupos de calibración
deciles_prob <- quantile(predicciones_prob, probs = seq(0, 1, 0.1))
grupos_calib <- cut(predicciones_prob, breaks = deciles_prob, include.lowest = TRUE)

# Calcular observado vs esperado por grupo
obs_por_grupo <- aggregate(datos$desenlace_bin, by = list(grupos_calib), FUN = mean, na.rm = TRUE)
esp_por_grupo <- aggregate(predicciones_prob, by = list(grupos_calib), FUN = mean, na.rm = TRUE)

# Calcular correlación entre observado y esperado
if(nrow(obs_por_grupo) > 2) {
  correlacion_calib <- cor(obs_por_grupo$x, esp_por_grupo$x, use = "complete.obs")
  cat("Correlación Observado vs Esperado:", round(correlacion_calib, 3), "\n")
  
  if(correlacion_calib >= 0.9) {
    cat("Excelente calibración (r ≥ 0.9)\n")
  } else if(correlacion_calib >= 0.8) {
    cat("Buena calibración (r ≥ 0.8)\n")
  } else if(correlacion_calib >= 0.7) {
    cat("Calibración moderada (r ≥ 0.7)\n")
  } else {
    cat("Calibración pobre (r < 0.7)\n")
  }
}

# Pendiente de calibración (idealmente = 1)
if(nrow(obs_por_grupo) > 2) {
  modelo_calib <- lm(obs_por_grupo$x ~ esp_por_grupo$x)
  pendiente_calib <- coef(modelo_calib)[2]
  cat("Pendiente de calibración:", round(pendiente_calib, 3), "(ideal = 1.0)\n")
  
  if(abs(pendiente_calib - 1) <= 0.1) {
    cat("Pendiente adecuada (≈ 1.0)\n")
  } else {
    cat("Desviación en pendiente\n")
  }
}

# Prueba de especificación del modelo (LINKTEST)
cat("\n=== PRUEBA DE ESPECIFICACIÓN (LINKTEST) ===\n")
predicciones_lineales <- predict(modelo_binomial, type = "link")
datos_temp <- datos
datos_temp$hat <- predicciones_lineales
datos_temp$hatsq <- predicciones_lineales^2

linktest_model <- glm(desenlace ~ hat + hatsq, data = datos_temp, family = binomial)
linktest_summary <- summary(linktest_model)

cat("Coeficiente _hatsq p-value:", round(linktest_summary$coefficients["hatsq", "Pr(>|z|)"], 4), "\n")

if(linktest_summary$coefficients["hatsq", "Pr(>|z|)"] > 0.05) {
  cat("Especificación del modelo adecuada\n")
} else {
  cat("Posible mala especificación del modelo\n")
}


# Métricas adicionales de ajuste
cat("\n=== MÉTRICAS ADICIONALES DE AJUSTE ===\n")
cat("--- Modelo Completo vs Modelo Nulo ---\n")
cat("AIC Modelo Completo:", AIC(modelo_binomial), "\n")
cat("AIC Modelo Nulo:", AIC(modelo_nulo), "\n")
cat("Mejora AIC:", round(AIC(modelo_nulo) - AIC(modelo_binomial), 2), "puntos\n")

cat("\nBIC Modelo Completo:", BIC(modelo_binomial), "\n")
cat("BIC Modelo Nulo:", BIC(modelo_nulo), "\n")
cat("Mejora BIC:", round(BIC(modelo_nulo) - BIC(modelo_binomial), 2), "puntos\n")

cat("\nLog-Likelihood Modelo Completo:", round(as.numeric(logLik(modelo_binomial)), 2), "\n")
cat("Log-Likelihood Modelo Nulo:", round(as.numeric(logLik(modelo_nulo)), 2), "\n")
cat("Mejora Log-Likelihood:", round(as.numeric(logLik(modelo_binomial)) - as.numeric(logLik(modelo_nulo)), 2), "puntos\n")

cat("\nDeviance Modelo Completo:", round(deviance(modelo_binomial), 2), "\n")
cat("Deviance Modelo Nulo:", round(modelo_binomial$null.deviance, 2), "\n")
cat("Reducción Deviance:", round(modelo_binomial$null.deviance - deviance(modelo_binomial), 2), "puntos\n")

pct_deviance <- round((1 - deviance(modelo_binomial)/modelo_binomial$null.deviance) * 100, 2)
cat("% Deviance Explained:", pct_deviance, "%\n")


# ODDS RATIOS E INTERVALOS DE CONFIANZA
cat("\n=== INTERVALOS DE CONFIANZA PARA ODDS RATIOS ===\n")
OR_CI <- exp(cbind(OR = coef(modelo_binomial), confint(modelo_binomial)))
print(OR_CI)

# CAPACIDAD DISCRIMINATIVA
## Curva ROC y AUC
roc_curve <- roc(datos$desenlace, predicciones_prob)
auc_value <- auc(roc_curve)
ci_auc <- ci.auc(roc_curve)

cat("\n=== ÁREA BAJO LA CURVA (AUC) ===\n")
cat("AUC:", round(auc_value, 6), "\n")
cat("AUC 95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[3], 3), "\n")

# Gráfico ROC
plot(roc_curve, main = "Curva ROC - Modelo MAiD",
     xlab = "1 - Especificidad", ylab = "Sensibilidad")
abline(a = 0, b = 1, lty = 2, col = "red")

# Añadir AUC y 95% CI en la esquina (texto más grande)
texto_auc <- paste0("AUC = ", round(auc_value, 3),
                    "\n95% CI: ", round(ci_auc[1], 3), " - ", round(ci_auc[3], 3))
legend("bottomright", legend = texto_auc, bty = "n", cex = 1.4, text.font = 2, inset = c(0.02, 0.02))

# VALIDACIÓN CRUZADA
## VALIDACIÓN CRUZADA (enfoque explicativo)
set.seed(123)

# Definir esquema de validación cruzada
ctrl <- trainControl(method = "cv", number = 5, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary, 
                     savePredictions = TRUE)

# Reentrenar el modelo con caret usando el mismo conjunto de variables
modelo_cv <- train(desenlace ~ sexo + edad_agrupada + patologia_agrupada +
                     ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud,
                   data = datos,
                   method = "glm",
                   family = binomial,
                   metric = "ROC",
                   trControl = ctrl)

# Evaluar la estabilidad de los coeficientes entre pliegues
# Extraer los índices de entrenamiento de cada pliegue
folds <- modelo_cv$control$index

# Ajustar un modelo por pliegue y guardar coeficientes
coef_list <- lapply(folds, function(idx) {
  m <- glm(
    desenlace ~ sexo + edad_agrupada + patologia_agrupada +
      ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud,
    data = datos[idx, ],
    family = binomial
  )
  coef(m)
})

# Convertir lista a data.frame
coef_df <- do.call(rbind, coef_list)

# Calcular media y desviación estándar de los coeficientes
coef_summary <- data.frame(
  Variable = colnames(coef_df),
  Media = apply(coef_df, 2, mean),
  SD = apply(coef_df, 2, sd)
)

cat("\n=== ESTABILIDAD DE LOS COEFICIENTES ENTRE PLIEGUES ===\n")
print(coef_summary)

# AUC promedio
auc_medio <- mean(modelo_cv$resample$ROC)
cat("\nAUC promedio (validación cruzada):", round(auc_medio, 3), "\n")

# ANÁLISIS DE ESTABILIDAD (SPLIT-SAMPLE)
cat("\n=== ANÁLISIS DE ESTABILIDAD ===\n")
set.seed(123)
indices_train <- sample(nrow(datos), size = 0.7 * nrow(datos))
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

modelo_train <- glm(desenlace ~ sexo + edad_agrupada + patologia_agrupada + 
                    ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud,
                    data = datos_train, family = binomial)

pred_test <- predict(modelo_train, newdata = datos_test, type = "response")
roc_test <- roc(datos_test$desenlace, pred_test)

cat("AUC modelo completo:", round(auc(roc_curve), 3), "\n")
cat("AUC muestra test:", round(auc(roc_test), 3), "\n")
diferencia_split <- auc(roc_curve) - auc(roc_test)
cat("Diferencia AUC:", round(diferencia_split, 3), "\n")

if(abs(diferencia_split) < 0.05) {
  cat("Modelo estable\n")
} else {
  cat("Revisar estabilidad\n")
}

# MATRIZ DE CONFUSIÓN
## Punto de corte óptimo
coords_roc <- coords(roc_curve, "best", ret = c("threshold", "sensitivity", "specificity"))
punto_corte <- coords_roc$threshold

## Predicciones binarias
predicciones_bin <- ifelse(predicciones_prob > punto_corte, "prestacion", "denegacion")
predicciones_bin <- factor(predicciones_bin, levels = levels(datos$desenlace))

## Matriz de confusión
conf_matrix <- confusionMatrix(predicciones_bin, datos$desenlace, positive = "prestacion")
cat("\n=== MATRIZ DE CONFUSIÓN ===\n")
print(conf_matrix)

# DIAGNÓSTICO DE RESIDUOS
cat("\n=== DIAGNÓSTICO DE RESIDUOS ===\n")

# Residuos de desviación
residuos_dev <- residuals(modelo_binomial, type = "deviance")
residuos_pears <- residuals(modelo_binomial, type = "pearson")

# Gráficos de diagnóstico
par(mfrow = c(2, 2))

plot(fitted(modelo_binomial), residuos_dev, 
     main = "Residuos de Desviación vs Valores Ajustados",
     xlab = "Valores Ajustados", ylab = "Residuos de Desviación")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuos_dev, main = "Q-Q Plot - Residuos de Desviación")
qqline(residuos_dev, col = "red")

plot(residuos_dev, main = "Residuos de Desviación vs Índice",
     xlab = "Índice", ylab = "Residuos de Desviación")
abline(h = 0, col = "red", lty = 2)

hist(residuos_dev, main = "Distribución de Residuos de Desviación",
     xlab = "Residuos de Desviación", freq = FALSE)

par(mfrow = c(1, 1))

# DETECCIÓN DE OUTLIERS E INFLUENCIA
cat("\n=== DETECCIÓN DE OUTLIERS ===\n")

outliers_residuos <- which(abs(residuos_dev) > 2)
cat("Observaciones con residuos |z| > 2:", length(outliers_residuos), "\n")

cook_d <- cooks.distance(modelo_binomial)
outliers_cook <- which(cook_d > 4/length(cook_d))
cat("Observaciones influyentes (Cook's D):", length(outliers_cook), "\n")

leverage <- hatvalues(modelo_binomial)
outliers_lev <- which(leverage > 2*length(coef(modelo_binomial))/nrow(datos))
cat("Observaciones con alto leverage:", length(outliers_lev), "\n")

# ANÁLISIS DE SENSIBILIDAD SIN OUTLIERS
cat("\n=== ANÁLISIS DE SENSIBILIDAD ===\n")
outliers_combined <- unique(c(outliers_residuos, outliers_cook, outliers_lev))
cat("Total outliers:", length(outliers_combined), "\n")

if(length(outliers_combined) > 0 && length(outliers_combined) < nrow(datos) * 0.05) {
  datos_sin_outliers <- datos[-outliers_combined, ]
  
  modelo_sin_outliers <- glm(desenlace ~ sexo + edad_agrupada + patologia_agrupada + 
                            ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud,
                            data = datos_sin_outliers, family = binomial)
  
  pred_sin_outliers <- predict(modelo_sin_outliers, type = "response")
  roc_sin_outliers <- roc(datos_sin_outliers$desenlace, pred_sin_outliers)
  
  cambio_auc <- auc(roc_sin_outliers) - auc(roc_curve)
  cat("Cambio AUC sin outliers:", round(cambio_auc, 3), "\n")
  
  if(abs(cambio_auc) < 0.02) {
    cat("Modelo robusto a outliers\n")
  } else {
    cat("Modelo sensible a outliers\n")
  }
} else if(length(outliers_combined) == 0) {
  cat("No se detectaron outliers significativos\n")
} else {
  cat("Demasiados outliers para análisis de sensibilidad\n")
}

# EFECTOS MARGINALES
cat("\n=== EFECTOS MARGINALES PROMEDIO ===\n")
efectos_marg <- margins(modelo_binomial)
summary(efectos_marg)

# PROBABILIDADES PROMEDIO POR CATEGORÍAS
cat("\n=== PROBABILIDADES PROMEDIO POR CATEGORÍAS ===\n")

prob_sexo <- aggregate(predicciones_prob, by = list(datos$sexo), FUN = mean)
names(prob_sexo) <- c("Sexo", "Prob_Promedio")
print(prob_sexo)

prob_edad <- aggregate(predicciones_prob, by = list(datos$edad_agrupada), FUN = mean)
names(prob_edad) <- c("Edad_Agrupada", "Prob_Promedio")
print(prob_edad)

prob_patologia <- aggregate(predicciones_prob, by = list(datos$patologia_agrupada), FUN = mean)
names(prob_patologia) <- c("Patologia_Agrupada", "Prob_Promedio")
print(prob_patologia)

prob_ccaa <- aggregate(predicciones_prob, by = list(datos$ccaa_agrupada), FUN = mean)
names(prob_ccaa) <- c("CCAA_Agrupada", "Prob_Promedio")
print(prob_ccaa)

prob_adelanto <- aggregate(predicciones_prob, by = list(datos$adelanto_segunda_solicitud), FUN = mean)
names(prob_adelanto) <- c("Adelanto_Segunda_Solicitud", "Prob_Promedio")
print(prob_adelanto)

prob_tipo <- aggregate(predicciones_prob, by = list(datos$tipo_procedimiento), FUN = mean)
names(prob_tipo) <- c("Tipo_Procedimiento", "Prob_Promedio")
print(prob_tipo)

# Prueba de calibración del modelo (correlacion)
cat("\n=== PRUEBA DE CALIBRACIÓN DEL MODELO (CORRELACIÓN) ===\n")
correlacion_modelo <- cor(datos$desenlace_bin, predicciones_prob)
cat("Correlación Observado vs Predicho:", round(correlacion_modelo, 3), "\n")
if(correlacion_modelo >= 0.9) {
  cat("Excelente calibración (r ≥ 0.9)\n")
} else if(correlacion_modelo >= 0.8) {
  cat("Buena calibración (r ≥ 0.8)\n")
} else if(correlacion_modelo >= 0.7) {
  cat("Calibración moderada (r ≥ 0.7)\n")
} else {
  cat("Calibración pobre (r < 0.7)\n")
}



#Sobremuestreo de la clase minoritaria (denegación) para balancear clases
cat("\n=== ANÁLISIS CON SOBREMUESTREO DE LA CLASE MINORITARIA ===\n")
library(ROSE)
set.seed(123)
datos_oversampled <- ovun.sample(desenlace ~ sexo + edad_agrupada + patologia_agrupada + 
                                   ccaa_agrupada + tipo_procedimiento + adelanto_segunda_solicitud,
                                 data = datos, method = "over", N = max(table(datos$desenlace)) * 2)$data
table(datos_oversampled$desenlace)


# Prueba de especificación del modelo ponderado (LINKTEST)
predicciones_lineales_os <- predict(modelo_binomial, newdata = datos_oversampled, type = "link")
datos_oversampled$hat <- predicciones_lineales_os
datos_oversampled$hatsq <- predicciones_lineales_os^2
linktest_model_os <- glm(desenlace ~ hat + hatsq, data = datos_oversampled, family = binomial)
linktest_summary_os <- summary(linktest_model_os)
cat("Coeficiente _hatsq p-value (sobremuestreo):", round(linktest_summary_os$coefficients["hatsq", "Pr(>|z|)"], 4), "\n")
if(linktest_summary_os$coefficients["hatsq", "Pr(>|z|)"] > 0.05) {
  cat("Especificación del modelo adecuada (sobremuestreo)\n")
} else {
  cat("Posible mala especificación del modelo (sobremuestreo)\n")
}