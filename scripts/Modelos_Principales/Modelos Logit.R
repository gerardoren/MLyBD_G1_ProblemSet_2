# Script modelos Logit:
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
#MODEL TRAINIG: LOGIT UMBRAL OPTIMO por ROC

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

form_modelo_logit1=pobre~dominio+arrienda + num_personas + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor

set.seed(1410)
logit_1 <- train(form_modelo_logit1,
                 data = TRAIN, 
                 method = "glm",
                 trControl = ctrl,
                 family = "binomial")
logit_1

predicciones <- predict(logit_1, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit <- roc(response = TRAIN$pobre,  
                     predictor = predicciones,  
                     levels = c("No", "Yes"),  
                     direction = "<")
logit_best_threshold <- coords(roc_obj_logit, x = "best", best.method = "closest.topleft")

# Mostrar el umbral óptimo
logit_best_threshold
pred_clase <- ifelse(predicciones >= logit_best_threshold[1], "Yes", "No")


# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_nuevo_umbral <- TRAIN %>%
  mutate(pobre_prob_logit_en_sens = predict(logit_1, newdata = TRAIN, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_en_sens >= logit_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))

cm_model1_Nuevo_umbral <- confusionMatrix(Logit_nuevo_umbral$clasificacion_nuevo_umbral, TRAIN$pobre, positive = "Yes",  mode = "prec_recall")
print(cm_model1_Nuevo_umbral)


# guardo en un data frame
df_logit_1_best_threshold <- data.frame(
  logit_1_best_threshold = "logit_1_best_threshold",
  F1_Score = cm_model1_Nuevo_umbral$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_1_best_threshold)<-NULL
df_logit_1_best_threshold
##-----------------------------------------------------------------------------##

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(pobre_lab = predict(logit_1, newdata = TEST, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_lab >= logit_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))%>% select(id,clasificacion_nuevo_umbral)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_nuevo_umbral=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##
# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(logit_1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(logit_1$bestTune$alpha))

name<- paste0(
  "stores\\sub\\logit_best_threshold_0.19",
  ".csv") 


write.csv(predictSample,name,row.names = FALSE)

##-------------------------------------------------------------------------------##




##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
#MODEL TRAINIG: Logit curva PR

# Modelo usando Logit, escogiendo los hiperparámetros usando cross validación
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=pobre~dominio+arrienda + num_personas + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor

set.seed(098063)
m_logit_1 <- train(form_modelo_logit1,
                   data = TRAIN, 
                   method = "glm",
                   trControl = ctrl,
                   family = "binomial")

m_logit_1

predicciones <- predict(m_logit_1, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit<- roc(response = TRAIN$pobre,  
                           predictor = predicciones,  
                           levels = c("No", "Yes"),  
                           direction = "<")

# calculamos la curva PR para  100 valores de umbral, y para cada uno de ellos calculo la precisión y el recall del modelo:
prec_recall<-data.frame(coords(roc_obj_logit, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

# Encontramos la columna del F1-score, ya que este nos da el mejor balance entre capturar la clase minoritaria (recall) 
# sin perder demasiada precisión.
prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
umbral_optimo <- prec_recall$threshold[which.max(prec_recall$F1)]




# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_prec_recall <- TRAIN %>%
  mutate(pobre_prob_logit_sens = predict(m_logit_1, newdata = TRAIN, type = "prob")[, "Yes"],
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_sens >= umbral_optimo, "Yes", "No"),
           levels = c("No", "Yes")))

cm_Logit_prec_recall <- confusionMatrix(Logit_prec_recall$clasificacion_nuevo_umbral, TRAIN$pobre, positive = "Yes",  mode = "prec_recall")
print(cm_Logit_prec_recall)


# guardo en un data frame
df_Logit_prec_recall <- data.frame(
  Logit_prec_recall = "Logit_prec_recall",
  F1_Score = cm_Logit_prec_recall$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_Logit_prec_recall)<-NULL
df_Logit_prec_recall
##-----------------------------------------------------------------------------##

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(pobre_lab = predict(logit_ELNET, newdata = TEST, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el prec_recall
         clasificacion_prec_recall = factor(
           ifelse(pobre_lab >= umbral_optimo, "Yes", "No"),
           levels = c("No", "Yes")))%>% select(id,clasificacion_prec_recall)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_prec_recall=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##


# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(logit_ELNET$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(logit_ELNET$bestTune$alpha))

name<- paste0(
  "stores\\sub\\EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)
##-----------------------------------------------------------------------------##


