#Modelo 13: Cambio de variables RF ajustado y SMOTE

ctrl <- trainControl(method = "cv",
                     number = 5,                  # 5-fold cross-validation
                     classProbs = TRUE,           # Enable class probabilities
                     summaryFunction = prSummary, # Precision, recall, and F1-score
                     savePredictions = TRUE,      # Save predicted values
                     verboseIter = TRUE)          # Show iteration progress

ctrl_smote <- trainControl(method = "cv",
                           number = 5,                  
                           classProbs = TRUE,           
                           summaryFunction = prSummary,
                           savePredictions = TRUE,      
                           verboseIter = TRUE,
                           sampling = "smote")

# Grid de combinaciones de hiperparámetros

grid_rf <- expand.grid(
  mtry = 2,           
  splitrule = "gini",           
  min.node.size = 20)

#Modelo a utilizar: 

modelo_RF3=pobre~dominio+arrienda + num_personas + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor

# Entrenamos Random forest en el sub_TRAIN

modelo13_rf_sub <- train(modelo_RF3, 
                        data = sub_TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl_smote,          
                        tuneGrid = grid_rf,    
                        num.trees = 200,
                        importance = "impurity")             

modelo13_rf_sub

# Porbabilidades predichas en sub_TEST
test_probs <- predict(modelo13_rf_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

# Umbral que maximice F1
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$pobre, predicted_probs = test_probs)

# Encontrar el F1 más alto
umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)


# Valores F1
print(paste("Optimal Threshold:", umbral_optimo))       # 0.42
print(paste("F1-score at Optimal Threshold:", f1_optimo))  # 63.21 con SMOTE. 

set.seed(3698521)

modelo13_rf <- train(modelo_RF3, 
                    data = TRAIN, 
                    method = "ranger",           
                    metric = "F",               
                    trControl = ctrl_smote,          
                    tuneGrid = grid_rf,    
                    num.trees = 200,
                    importance = "impurity")  
modelo13_rf



# Submission
test_probs <- predict(modelo13_rf, newdata = TEST, type = "prob")[, "Yes"]

# Usamos el umbral optimo
final_test_preds <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo13_rf, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("stores\\sub\\RF3ajust_SMOTE_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)

