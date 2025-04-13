
#OJO: Acá se pone el método de sampling de SMOTE que sirve para el modelo

ctrl_smote <- trainControl(method = "cv",
                           number = 5,                  
                           classProbs = TRUE,           
                           summaryFunction = prSummary,
                           savePredictions = TRUE,      
                           verboseIter = TRUE,
                           sampling = "smote")

#Se plantea el grid en el que se va a mover el modelo

grid_xg <- expand.grid(
  nrounds = 200,         
  max_depth = 6,            
  eta = 0.05,            
  gamma = 0,                     
  colsample_bytree = 0.8,         
  min_child_weight = 10,         
  subsample = 0.8               
)

#Modelo que se va a utilizar para entrenar


modelo_XGB2=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor

#Entrear el mdoelo con los subdatos de TRAIN

modelo15_xgb_sub <- train(modelo_XGB2, 
                         data = sub_TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl_smote,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo15_xgb_sub

# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo15_xgb_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

#Buscamos el umbral que maximise F1 
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontramos ese umbral

umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

print(paste("Umbral Optimo:", umbral_optimo))       # 0.4
print(paste("F1 optimo de acuerdo a umbral:", f1_optimo))  # 0.636

# Entrenamos el modelo ahora con toda la muestra

set.seed(56856)

modelo15_xgb <- train(modelo_XGB2, 
                     data = TRAIN, 
                     method = "xgbTree",        
                     metric = "F",               
                     trControl = ctrl_smote,       
                     tuneGrid = grid_xg,  
                     nthread = 4)              

modelo15_xgb

# Se prepara la Submission

#Ajustar las probabilidades y el umbral para la submission

test_probs <- predict(modelo15_xgb, newdata = TEST, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora la submission

predictSample <- TEST %>%
  mutate(pobre_lab = test_preds_fin) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGBajust2_smote_eta_0.05_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)

