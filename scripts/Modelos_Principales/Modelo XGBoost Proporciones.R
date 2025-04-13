p_load(xgboost)


ctrl <- trainControl(method = "cv",
                     number = 3,
                     summaryFunction = prSummary,
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     savePredictions = TRUE)

grid_xbgoost <- expand.grid(nrounds = c(100),
                            max_depth = c(3, 6),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10),
                            colsample_bytree = c(0.5, 0.8), 
                            subsample = c(0.8))
grid_xbgoost


form_modelo_CART1=pobre~dominio+arrienda + num_personas+ pers_por_cuarto + prop_mujeres+ prop_menores+
  prop_adulto_mayor+ meanEducLevel+ prop_ocupados + prop_inactivos+
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor+prop_ss

set.seed(91519) # Importante definir la semilla antes entrenar
Xgboost_tree <- train(form_modelo_CART1,
                      data = sub_TRAIN, 
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid=grid_xbgoost,
                      metric = "F",
                      verbosity = 1
)         
Xgboost_tree

# === 1. Predicción de probabilidades ===
xgb_probs <- predict(Xgboost_tree, newdata = sub_TEST, type = "prob")[, "Yes"]

# === 2. Rango de umbrales a probar ===
thresholds <- seq(0.1, 0.9, by = 0.01)

# === 3. Calcular F1-score para cada umbral ===
f1_scores <- sapply(thresholds, function(t) {
  pred_labels <- ifelse(xgb_probs > t, "Yes", "No")
  F1_Score(y_pred = as.factor(pred_labels), y_true = sub_TEST$pobre, positive = "Yes")
})

# === 4. Selección del mejor umbral ===
best_threshold <- thresholds[which.max(f1_scores)]
cat("Mejor umbral según F1 (XGBoost):", best_threshold, "\n")

# === 5. Clasificación final con umbral óptimo ===
final_preds <- ifelse(xgb_probs > best_threshold, "Yes", "No")
cat("Matriz de confusión con XGBoost:\n")
print(table(Real = sub_TEST$pobre, Predicha = final_preds))

f1_final <- F1_Score(y_pred = as.factor(final_preds), y_true = sub_TEST$pobre, positive = "Yes")
cat("F1-score optimizado con XGBoost:", round(f1_final, 4), "\n")
#F1-score optimizado con XGBoost: 0.6481 sin smote
## 
mejor_grid <- Xgboost_tree$bestTune
ctrl_final <- trainControl(method = "none", classProbs = TRUE)


# 2. Reentrenar el modelo en toda la base `TRAIN` con esa combinación
set.seed(91519)
Xgboost_final <- train(form_modelo_CART1,
                       data = TRAIN,  # <- Usa toda la data ahora
                       method = "xgbTree",
                       trControl = ctrl_final,      
                       tuneGrid = mejor_grid,  # Solo una fila: los hiperparámetros óptimos
                       metric = "F",
                       verbosity = 0)
Xgboost_final

# 3. Predicciones finales sobre TEST
xgb_probs_final <- predict(Xgboost_final, newdata = TEST, type = "prob")[, "Yes"]

# 4. Clasificación con el mejor umbral calculado previamente
predictSample <- TEST %>%
  mutate(
    pobre = ifelse(xgb_probs_final > best_threshold, 1, 0)
  ) %>%
  select(id, pobre)
head(predictSample)
# 5. Exportar CSV final
name <- paste0("stores\\sub\\XGBoost_FINAL_", round(best_threshold, 2), "_F1_", round(f1_final, 4), ".csv")
write.csv(predictSample, name, row.names = FALSE)