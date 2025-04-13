#Modelos Logit - Elnet

##-------------------------------------------------------------------------------##

##-------------------------------------------------------------------------------##

#MODEL TRAINIG: LOGIT ELASTIC NET DESBALANCEADO


# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
model1 <- train(form_modelo_logit1,
                data=TRAIN,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                preProcess = c("center", "scale"),  # Normaliza variables predictoras
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)
model1

model1$bestTune

##-----------------------------------------------------------------------------##
# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(model1, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_model1 <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_model1

# guardo en un data frame
df_ELNET <- data.frame(
  Model = "model_logit_ELNET",
  F1_Score = cm_model1$byClass["F1"]
  )

#  Elimino los nombres de las filas que no informan nada.
rownames(df_ELNET)<-NULL
df_ELNET

##-----------------------------------------------------------------------------##
# Preparacion para el envio a Kaggle:

predictSample <- TEST   %>% 
  mutate(pobre_lab = predict(model1, newdata = TEST, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##


# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)


##-----------------------------------------------------------------------------##




##-----------------------------------------------------------------------------##

#MODEL TRAINIG: ELASTIC NET UMBRAL OPTIMO 

# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET <- train(form_modelo_logit1,
                data=TRAIN,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                preProcess = c("center", "scale"),  # Normaliza variables predictoras
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)
logit_ELNET

logit_ELNET$bestTune

predicciones <- predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit_ELNET <- roc(response = TRAIN$Pobre,  
                     predictor = predicciones,  
                     levels = c("No", "Yes"),  
                     direction = "<")
logit_ELNET_best_threshold <- coords(roc_obj_logit_ELNET, x = "best", best.method = "closest.topleft")

# Mostrar el umbral óptimo
logit_ELNET_best_threshold
pred_clase <- ifelse(predicciones >= logit_ELNET_best_threshold[1], "Yes", "No")


# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_ELNET_nuevo_umbral <- TRAIN %>%
  mutate(pobre_prob_logit_ELNET_sens = predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_ELNET_sens >= logit_ELNET_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))

cm_logit_ELNET_Nuevo_umbral <- confusionMatrix(Logit_ELNET_nuevo_umbral$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_logit_ELNET_Nuevo_umbral)


# guardo en un data frame
df_logit_ELNET_best_threshold <- data.frame(
  logit_ELNET_best_threshold = "logit_ELNET_best_threshold",
  F1_Score = cm_logit_ELNET_Nuevo_umbral$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_best_threshold)<-NULL
df_logit_ELNET_best_threshold
##-----------------------------------------------------------------------------##

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(pobre_lab = predict(logit_ELNET, newdata = TEST, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_lab >= logit_ELNET_best_threshold$threshold, "Yes", "No"),
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
  as.character(round(logit_ELNET$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(logit_ELNET$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)


##-----------------------------------------------------------------------------##














##-----------------------------------------------------------------------------##
# LOGIT ELASTIC NET: 

#MODEL TRAINIG: ELASTIC NET curva PR

# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET <- train(form_modelo_logit1,
                     data=TRAIN,
                     metric = "F",
                     method = "glmnet",
                     trControl = ctrl,
                     preProcess = c("center", "scale"),  # Normaliza variables predictoras
                     family="binomial",
                     tuneGrid=expand.grid(
                       alpha = seq(0,1,by=.5),
                       lambda =10^seq(-1, -3, length = 10)
                     )
                     
)
logit_ELNET

logit_ELNET$bestTune

predicciones <- predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit_ELNET <- roc(response = TRAIN$Pobre,  
                           predictor = predicciones,  
                           levels = c("No", "Yes"),  
                           direction = "<")

# calculamos la curva PR para  100 valores de umbral, y para cada uno de ellos calculo la precisión y el recall del modelo:
prec_recall<-data.frame(coords(roc_obj_logit_ELNET, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

# Encontramos la columna del F1-score, ya que este nos da el mejor balance entre capturar la clase minoritaria (recall) 
# sin perder demasiada precisión.
prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
umbral_optimo <- prec_recall$threshold[which.max(prec_recall$F1)]




# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_ELNET_prec_recall <- TRAIN %>%
  mutate(pobre_prob_logit_ELNET_sens = predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"],
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_ELNET_sens >= umbral_optimo, "Yes", "No"),
           levels = c("No", "Yes")))

cm_Logit_ELNET_prec_recall <- confusionMatrix(Logit_ELNET_prec_recall$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_Logit_ELNET_prec_recall)


# guardo en un data frame
df_Logit_ELNET_prec_recall <- data.frame(
  Logit_ELNET_prec_recall = "Logit_ELNET_prec_recall",
  F1_Score = cm_Logit_ELNET_prec_recall$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_Logit_ELNET_prec_recall)<-NULL
df_Logit_ELNET_prec_recall
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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_reponderar_obs<- train(form_modelo_logit1,
                     data=TRAIN,
                     metric = "F",
                     weights    = wts,  # <-- pesos
                     method = "glmnet",
                     trControl = ctrl,
                     preProcess = c("center", "scale"),  # Normaliza variables predictoras
                     family="binomial",
                     tuneGrid=expand.grid(
                       alpha = seq(0,1,by=.5),
                       lambda =10^seq(-1, -3, length = 10)
                     )
                     
)
logit_ELNET_reponderar_obs

logit_ELNET_reponderar_obs$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_reponderar_obs, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_ELNET_reponderar_obs <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_ELNET_reponderar_obs

# guardo en un data frame
df_ELNET_reponderar_obs <- data.frame(
  Model = "logit_ELNET_reponderar_obs",
  F1_Score = cm_ELNET_reponderar_obs$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_ELNET_reponderar_obs)<-NULL
df_ELNET_reponderar_obs






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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_down_sampling<- train(form_modelo_logit1,
                                   data=downSampledTrain,
                                   metric = "F",
                                   method = "glmnet",
                                   trControl = ctrl,
                                   preProcess = c("center", "scale"),  # Normaliza variables predictoras
                                   family="binomial",
                                   tuneGrid=expand.grid(
                                     alpha = seq(0,1,by=.5),
                                     lambda =10^seq(-1, -3, length = 10)
                                   )
                                   
)
logit_ELNET_down_sampling

logit_ELNET_down_sampling$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_down_sampling, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_ELNET_down_sampling <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_ELNET_down_sampling

# guardo en un data frame
df_logit_ELNET_down_sampling <- data.frame(
  Model = "logit_ELNET_down_sampling",
  F1_Score = cm_logit_ELNET_down_sampling$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_down_sampling)<-NULL
df_logit_ELNET_down_sampling












##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

# Rebalanceo de Clases Submuestreo (Up Sampling)
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

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_upsampled<- train(form_modelo_logit1,
                                  data=upSampledTrain,
                                  metric = "F",
                                  method = "glmnet",
                                  trControl = ctrl,
                                  preProcess = c("center", "scale"),  # Normaliza variables predictoras
                                  family="binomial",
                                  tuneGrid=expand.grid(
                                    alpha = seq(0,1,by=.5),
                                    lambda =10^seq(-1, -3, length = 10)
                                  )
                                  
)
logit_ELNET_upsampled

logit_ELNET_upsampled$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_upsampled, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_ELNET_upsampled <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_ELNET_upsampled

# guardo en un data frame
df_logit_ELNET_upsampled <- data.frame(
  Model = "logit_ELNET_upsampled",
  F1_Score = cm_logit_ELNET_upsampled$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_upsampled)<-NULL
df_logit_ELNET_upsampled









##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
# Tabla de resultados metrica F1

# Normalizar los nombres de columnas en todos los dataframes
colnames(df_ELNET) <- c("Modelo", "F1_Score")
colnames(df_logit_ELNET_best_threshold) <- c("Modelo", "F1_Score")
colnames(df_Logit_ELNET_prec_recall) <- c("Modelo", "F1_Score")
colnames(df_ELNET_reponderar_obs) <- c("Modelo", "F1_Score")
colnames(df_logit_ELNET_down_sampling) <- c("Modelo", "F1_Score")
colnames(df_logit_ELNET_upsampled) <- c("Modelo", "F1_Score")

# Ahora podemos unir todos los dataframes en una sola tabla
df_desempeño_LOGIT_ELNET_F1 <- rbind(
  df_ELNET, 
  df_logit_ELNET_best_threshold, 
  df_Logit_ELNET_prec_recall, 
  df_ELNET_reponderar_obs, 
  df_logit_ELNET_down_sampling, 
  df_logit_ELNET_upsampled
)

# Ver tabla final
print(df_desempeño_LOGIT_ELNET_F1)

stargazer(df_desempeño_LOGIT_ELNET_F1,
          type = "latex",
          summary = FALSE,
          title = "Tabla de Resultados: Métrica F1 - Modelos Logit con Elastic Net",
          label = "tab:logit_elnet_f1",
          rownames = FALSE,
          digits = 3,
          out = "tabla_logit_elnet_f1.tex")




##-----------------------------------------------------------------------------##
df_desempeño_F1 <- rbind(
  df_desempeño_LOGIT_ELNET_F1,
  df_desempeño_LOGIT_F1)

print(df_desempeño_F1)


df_desempeño_F1 <- df_desempeño_F1[order(-df_desempeño_F1$F1_Score), ]

stargazer(df_desempeño_F1,
          type = "latex",
          summary = FALSE,
          title = "Métricas F1 de Modelos Logit y Logit con Elastic Net",
          label = "tab:resultados_f1",
          rownames = FALSE,
          digits = 3,
          out = "tabla_desempeño_F1.tex")


save(df_desempeño_F1, df_desempeño_LOGIT_ELNET_F1, df_desempeño_LOGIT_F1, 
     file = "C:/Users/samue/OneDrive/Escritorio/Economia/Big Data y Machine Learning/Taller 2/df_desempeño_logit&elnet_F1.RData")
