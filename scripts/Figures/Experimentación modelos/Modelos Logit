#Modelos Logit:

##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

#MODEL TRAINIG: LOGIT DESBALANCEADO y UMBRAL DEFAULT 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
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



predicciones <- predict(logit_1, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_model1 <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_model1)


# guardo en un data frame
df_logit_1 <- data.frame(
  logit_1 = "logit_1",
  F1_Score = cm_model1$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_1)<-NULL
df_logit_1

##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

#MODEL TRAINIG: LOGIT UMBRAL OPTIMO 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
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

roc_obj_logit <- roc(response = TRAIN$Pobre,  
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

cm_model1_Nuevo_umbral <- confusionMatrix(Logit_nuevo_umbral$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
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
  "logit_best_threshold_0.19",
  ".csv") 


write.csv(predictSample,name,row.names = FALSE)


##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

#MODEL TRAINIG: Logit curva PR

# Modelo usando Logit, escogiendo los hiperparámetros usando cross validación
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
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

roc_obj_logit<- roc(response = TRAIN$Pobre,  
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

cm_Logit_prec_recall <- confusionMatrix(Logit_prec_recall$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
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
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)
##-----------------------------------------------------------------------------##






##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
#Rebalanceo de Clases (Class Rebalancing) con Re-ponderar observaciones:

# Constructir los ponderadores
pos_weight <- sum(TRAIN$Pobre == "No") / sum(TRAIN$Pobre == "Yes")
wts <- ifelse(TRAIN$Pobre == "Yes" , pos_weight, 1)
pos_weight



ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor


set.seed(098063)
logit_reponderar_obs<-  train(form_modelo_logit1,
                                   data = TRAIN, 
                                   weights    = wts,
                                   method = "glm",
                                   trControl = ctrl,
                                   family = "binomial")
                                   
logit_reponderar_obs


# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_reponderar_obs, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_reponderar_obs <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_reponderar_obs

# guardo en un data frame
df_reponderar_obs <- data.frame(
  Model = "logit_reponderar_obs",
  F1_Score = cm_reponderar_obs$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_reponderar_obs)<-NULL
df_reponderar_obs

##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##



# Rebalanceo de Clases Submuestreo (Down Sampling)

set.seed(1103)
downSampledTrain <- downSample(x = TRAIN,
                               y = TRAIN$Pobre,
                               yname = "Pobre")
dim(TRAIN)
dim(downSampledTrain)
table(downSampledTrain$Pobre)


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor


set.seed(098063)
logit_down_sampling<-  train(form_modelo_logit1,
                             data = downSampledTrain, 
                             method = "glm",
                             trControl = ctrl,
                             family = "binomial")
                                  

logit_down_sampling


# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_down_sampling, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_down_sampling <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_down_sampling

# guardo en un data frame
df_logit_down_sampling <- data.frame(
  Model = "logit_down_sampling",
  F1_Score = cm_logit_down_sampling$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_down_sampling)<-NULL
df_logit_down_sampling


##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##



# Rebalanceo de Clases sobremuestreo (Up Sampling)
set.seed(1103)
upSampledTrain <- upSample(x = TRAIN,
                           y = TRAIN$Pobre,
                           yname = "Pobre")
dim(TRAIN)
dim(upSampledTrain)
table(upSampledTrain$Pobre)


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor


set.seed(098063)
logit_upsampled<- train(form_modelo_logit1,
                        data = upSampledTrain, 
                        method = "glm",
                        trControl = ctrl,
                        family = "binomial")
logit_upsampled

logit_upsampled$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_upsampled, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_upsampled <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_upsampled

# guardo en un data frame
df_logit_upsampled <- data.frame(
  Model = "logit_upsampled",
  F1_Score = cm_logit_upsampled$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_upsampled)<-NULL
df_logit_upsampled



##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##



# Tabla de resultados metrica F1

# Normalizar los nombres de columnas en todos los dataframes
colnames(df_logit_1) <- c("Modelo", "F1_Score")
colnames(df_logit_1_best_threshold) <- c("Modelo", "F1_Score")
colnames(df_Logit_prec_recall) <- c("Modelo", "F1_Score")
colnames(df_reponderar_obs) <- c("Modelo", "F1_Score")
colnames(df_logit_down_sampling) <- c("Modelo", "F1_Score")
colnames(df_logit_upsampled) <- c("Modelo", "F1_Score")

# Ahora podemos unir todos los dataframes en una sola tabla
df_desempeño_LOGIT_F1 <- rbind(
  df_logit_1, 
  df_logit_1_best_threshold,
  df_Logit_prec_recall, 
  df_reponderar_obs, 
  df_logit_down_sampling, 
  df_logit_upsampled
)

# Ver tabla final
print(df_desempeño_LOGIT_F1)


# Exportar la tabla a LaTeX
stargazer(df_desempeño_LOGIT_F1, 
          type = "latex",
          summary = FALSE,
          title = "Tabla de Resultados: Métrica F1 para Modelos Logit",
          label = "tab:logit_f1",
          rownames = FALSE,
          digits = 3,
          out = "tabla_logit_f1.tex")

