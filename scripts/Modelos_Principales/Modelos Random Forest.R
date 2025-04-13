## Script Modelos Random Forest:

##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

# Random forest:

# === 1. Definir la grilla de hiperparámetros ===
grid <- expand.grid(
  mtry = c(2, 4, 6),
  min.node.size = c(1, 5, 10)
)

# === 2. Crear dataframe para almacenar resultados ===
resultados <- data.frame()

# === 3. Bucle para entrenar y evaluar ===
for (i in 1:nrow(grid)) {
  cat("Probando combinación", i, "de", nrow(grid), "\n")
  
  # Extraer valores actuales de la grilla
  mtry_val <- grid$mtry[i]
  min_node_val <- grid$min.node.size[i]
  
  # Entrenar el modelo con ranger
  rf <- ranger::ranger(
    formula = pobre ~ ., 
    data = sub_TRAIN, 
    num.trees = 500,
    mtry = mtry_val,
    min.node.size = min_node_val,
    importance = "impurity",
    probability = TRUE,
    seed = 123
  )
  
  # 3. Predicciones y optimización de umbral (igual que antes)
  rf_probs <- predict(rf, data = sub_TEST)$predictions[, "Yes"]
  
  thresholds <- seq(0.1, 0.9, by = 0.01)
  f1_scores <- sapply(thresholds, function(t) {
    pred_labels <- ifelse(rf_probs > t, "Yes", "No")
    F1_Score(y_pred = as.factor(pred_labels), y_true = sub_TEST$pobre, positive = "Yes")
  })
  
  best_threshold <- thresholds[which.max(f1_scores)]
  cat("Mejor umbral según F1:", best_threshold, "\n")
  
  final_preds <- ifelse(rf_probs > best_threshold, "Yes", "No")
  cat("Matriz de confusión con ranger:\n")
  print(table(Real = sub_TEST$pobre, Predicha = final_preds))
  
  f1_final <- F1_Score(y_pred = as.factor(final_preds), y_true = sub_TEST$pobre, positive = "Yes")
  cat("F1-score optimizado con ranger:", round(f1_final, 4), "\n") # F1 = 0.6337 
  
  resultados <- rbind(resultados, data.frame(
    mtry = mtry_val,
    min.node.size = min_node_val,
    best_threshold = best_threshold,
    f1_score = f1_final
  ))
}

# === Encontrar y mostrar la mejor combinación ===
mejor_modelo <- resultados[which.max(resultados$f1_score), ]
cat("Mejores hiperparámetros encontrados:\n")
print(mejor_modelo)
#mtry min.node.size best_threshold  f1_score
#    2            10           0.36 0.6344338

umbral_optimo <- mejor_modelo$best_threshold

# vuelvo a entrenar el modelo pero con los mejores hiperparametrosy 
#con TRAIN total para luego hacerlo con TEST:
rf_optimo <- ranger::ranger(
  formula = pobre ~ ., 
  data = TRAIN, 
  num.trees = 500,
  mtry = mejor_modelo$mtry,
  min.node.size = mejor_modelo$min.node.size,
  importance = "impurity",
  probability = TRUE,
  seed = 123
)

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(
    pobre_lab = predict(rf_optimo, data = TEST, type = "response")$predictions[, "Yes"],
    
    # Clasificamos los Casos Usando el umbral optimizado
    clasificacion_prec_recall = factor(
      ifelse(pobre_lab >= umbral_optimo, "Yes", "No"),
      levels = c("No", "Yes"))
  ) %>%
  select(id, clasificacion_prec_recall)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_prec_recall=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##

name<- paste0(
  "stores\\sub\\Random_forest_mtry_", mejor_modelo$mtry,
  "_min.node.size_", mejor_modelo$min.node.size,
  "_best_threshold_", round(umbral_optimo, 2), ".csv") 

write.csv(predictSample,name, row.names = FALSE)
##-----------------------------------------------------------------------------##










##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##





