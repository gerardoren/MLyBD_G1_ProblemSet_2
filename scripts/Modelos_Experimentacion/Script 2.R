# install and load required packages
Packages <- c("tidyverse", 
              "ggplot2", 
              "pacman", 
              "dplyr",
              "haven",
              "boot",
              "broom",
              "lmtest", 
              "fixest", 
              "gridExtra", 
              "writexl", 
              "readxl",
              "glmnet",
              "VIM",
              "caret", 
              "MLmetrics",
              "Metrics",
              "pROC")

invisible(lapply(Packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)){ 
    install.packages(pkg)}
  library(pkg, character.only = TRUE)}))


# Clean environment
rm(list = ls())


# Set working directory
setwd("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2")


#Data

train_hogares<-read.csv("C:\\Users\\claud\\OneDrive\\Documents\\OneDrive - Universidad de los andes\\Universidad Los andes\\Noveno semestre\\Big data\\taller 2\\Data\\train_hogares.csv")
train_personas<-read.csv("C:\\Users\\claud\\OneDrive\\Documents\\OneDrive - Universidad de los andes\\Universidad Los andes\\Noveno semestre\\Big data\\taller 2\\Data\\train_personas.csv")
test_hogares<-read.csv("C:\\Users\\claud\\OneDrive\\Documents\\OneDrive - Universidad de los andes\\Universidad Los andes\\Noveno semestre\\Big data\\taller 2\\Data\\test_hogares.csv")
test_personas<-read.csv("C:\\Users\\claud\\OneDrive\\Documents\\OneDrive - Universidad de los andes\\Universidad Los andes\\Noveno semestre\\Big data\\taller 2\\Data\\test_personas.csv")


#Same variables in the train and test

same_hogares <- intersect(names(train_hogares), names(test_hogares))
same_hogares <- c(same_hogares, "Pobre")
same_personas <- intersect(names(train_personas), names(test_personas))

#Then keep only the vars who are in the both ds

train_hogares <- train_hogares[, same_hogares]
train_personas<- train_personas[, same_personas]
# organización y Pre procesamiento:

# Calculo de pobreza:
table(train_hogares$Pobre)

##-----------------------------------------------------------------------------##

# organización y Pre procesamiento:

# Calculo de pobreza:
table(train_hogares$Pobre)

#Podemos generar variables a nivel de hogar a partir de la base de datos de personas:

# Realizamos el preprocesamiento para las bases de personas:
# Creamos variables usando unformacion por persona:
preproces_personas <- function(data,...){
  data<-data %>% mutate(
    mujer = ifelse(P6020==2,1,0),
    menor_de_edad = ifelse(P6040 <= 16, 1, 0),
    adulto_mayor = ifelse(P6040 >= 60, 1, 0),
    regimen_salud = ifelse(P6100==4,3,P6100),
    regimen_salud = ifelse(is.na(regimen_salud), 3, regimen_salud),
    EducLevel = ifelse(P6210==9,0,P6210),
    Jefe_H = ifelse(P6050==1,1,0),
    ocupado = ifelse(is.na(Oc),0,1),
    desocupado = ifelse(is.na(Des),0,1),
    Inactivo = ifelse(is.na(Ina),0,1),
    Tipo_primer_empleo = ifelse(P6430==9,0,P6430),
    Tipo_primer_empleo = ifelse(is.na(Tipo_primer_empleo),1,Tipo_primer_empleo),
    segundo_empleo = ifelse(P7040==1,1,0),
    segundo_empleo = ifelse(is.na(segundo_empleo), 0, segundo_empleo),
    Recibio_horasextra = ifelse(P6510==1,1,0),
    Recibio_horasextra = ifelse(is.na(Recibio_horasextra), 0, Recibio_horasextra)) %>%
    rename(edad = P6040) %>%
    select(id, mujer, edad, menor_de_edad, adulto_mayor, regimen_salud, EducLevel, Jefe_H, 
           ocupado,desocupado, Inactivo,Tipo_primer_empleo,segundo_empleo,
           Recibio_horasextra) 
  return(data)
}

train_personas <- preproces_personas(train_personas)
test_personas <- preproces_personas(test_personas)

# generamos variables agregadas con base personas:

preproces_personas_agregacion <- function(data,...){
  agregacion <- data %>%
    group_by(id)%>%
    summarise(
      num_mujeres = sum(mujer,na.rm = TRUE),
      num_menores = sum(menor_de_edad, na.rm = TRUE),
      num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      num_ocupados = sum(ocupado, na.rm=TRUE),
      num_desocupados = sum(desocupado, na.rm = TRUE),
      num_inactivos = sum(Inactivo, na.rm = TRUE),
      num_con_segundo_empleo = sum(segundo_empleo, na.rm = TRUE),
      num_recibieron_hrextra = sum(Recibio_horasextra, na.rm = TRUE)
    )%>%
    ungroup()
  
  por_hogares_jefeh <- data %>%
    filter(Jefe_H==1) %>%
    select(id,mujer,regimen_salud,EducLevel,Jefe_H, ocupado,Inactivo,
           desocupado,Tipo_primer_empleo,segundo_empleo,edad,
           Recibio_horasextra,adulto_mayor)%>%
    rename(Jefe_H_mujer=mujer,
           Jefe_regimen_salud=regimen_salud,
           Jefe_EducLevel=EducLevel,
           Jefe_edad=edad,
           Jefe_adulto_mayor=adulto_mayor,
           Jefe_ocupado=ocupado,
           Jefe_Inactivo=Inactivo,
           Jefe_desocupado=desocupado,
           Jefe_Tipo_primer_empleo=Tipo_primer_empleo,
           Jefe_segundo_empleo=segundo_empleo,
           Jefe_Recibio_horasextra=Recibio_horasextra
    )
  resultado_final <- agregacion %>%
    left_join(por_hogares_jefeh, by = "id")
  
  return(resultado_final)
}


train_personas <- preproces_personas_agregacion(train_personas)
test_personas<-preproces_personas_agregacion(test_personas)


# generamos variables a nivel hogar con base hogar:

preproces_hogares <- function(data, ...) {
  data <- data %>%
    mutate(arrienda = ifelse(P5090 == 3, 1, 0))
  data$Dominio <- as.character(data$Dominio)
  data$Dominio[data$Dominio != "RURAL"] <- "urbano"
  data$Dominio[data$Dominio == "RURAL"] <- "rural"
  data$Dominio <- factor(data$Dominio)
  names(data)[names(data) == "P5010"] <- "pers_por_cuarto"
  data <- data %>%
    select(id, Clase, Dominio, arrienda, Nper, pers_por_cuarto, any_of("Pobre"))
  
  return(data)
}


train_hogares<-preproces_hogares(train_hogares)
test_hogares<-preproces_hogares(test_hogares)

# Unimos las bases de datos correspondientes a nivel hogar
TRAIN <-merge(train_hogares,train_personas)
TEST <- merge(test_hogares,test_personas)



#convertimos las variables a formatos adecuados
TRAIN<- TRAIN %>% 
  mutate(Pobre=factor(Pobre,
                      levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                                   levels=c(1,2,3),
                                   labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                               levels=c(0:6), 
                               labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                        'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                              levels=c(0,1),
                              labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                                levels=c(0,1),
                                labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                                        levels=c(0,1,2,3,4,5,6,7,8),
                                        labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                                 "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                                 "Trabajador familiar sin remuneración",
                                                 "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                 "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                                    levels=c(0,1),
                                    labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                                        levels=c(0,1),
                                        labels=c("No","Yes"))
  )

TEST<- TEST %>% 
  mutate(Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                                   levels=c(1,2,3),
                                   labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                               levels=c(0:6), 
                               labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                        'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                              levels=c(0,1),
                              labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                                levels=c(0,1),
                                labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                                        levels=c(0,1,2,3,4,5,6,7,8),
                                        labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                                 "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                                 "Trabajador familiar sin remuneración",
                                                 "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                 "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                                    levels=c(0,1),
                                    labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                                        levels=c(0,1),
                                        labels=c("No","Yes"))
  )


corrijo_na <- function(data, ...) {
  data <- data %>%
    mutate(Jefe_regimen_salud= ifelse(is.na(Jefe_regimen_salud), 3, Jefe_regimen_salud))
  return(data)
}
TRAIN <- corrijo_na(TRAIN)
TEST <- corrijo_na(TEST)


vars_deseadas_TRAIN<- c(
  "id", "Pobre", "Dominio", "arrienda", "Nper", "pers_por_cuarto",
  "num_mujeres", "num_menores", "num_adulto_mayor", "maxEducLevel",
  "num_ocupados", "num_inactivos", "num_con_segundo_empleo",
  "num_recibieron_hrextra", "Jefe_H_mujer", "Jefe_regimen_salud",
  "Jefe_EducLevel", "Jefe_desocupado", "Jefe_Tipo_primer_empleo",
  "Jefe_edad", "Jefe_adulto_mayor"
)

TRAIN <- TRAIN[, vars_deseadas_TRAIN]

vars_deseadas_TEST<- c(
  "id", "Dominio", "arrienda", "Nper", "pers_por_cuarto",
  "num_mujeres", "num_menores", "num_adulto_mayor", "maxEducLevel",
  "num_ocupados", "num_inactivos", "num_con_segundo_empleo",
  "num_recibieron_hrextra", "Jefe_H_mujer", "Jefe_regimen_salud",
  "Jefe_EducLevel", "Jefe_desocupado", "Jefe_Tipo_primer_empleo",
  "Jefe_edad", "Jefe_adulto_mayor"
)

TEST <- TEST[, vars_deseadas_TEST]

##-------------------------------------------------------------------------------##

vars_a_eliminar <- c("Jefe_H", "Jefe_Recibio_horasextra")

TRAIN <- TRAIN[, !(names(TRAIN) %in% vars_a_eliminar)]
TEST  <- TEST[, !(names(TEST) %in% vars_a_eliminar)]


#Crear submuestras con el fin de poder hacer la validación cruzada en el mismo código antes de mandar la submission

inTRAIN <- createDataPartition(
  y = TRAIN$Pobre,  
  p = 0.7,          # 70% of the data for training
  list = FALSE      
)

# sub_train (70%) and sub_test (30%)
sub_TRAIN <- TRAIN[inTRAIN, ]
sub_TEST  <- TRAIN[-inTRAIN, ]

# Verificar la proporción de "Pobre" en las submuestras
prop.table(table(TRAIN$Pobre))
prop.table(table(sub_TRAIN$Pobre))
prop.table(table(sub_TEST$Pobre))

# F1 Function ==================================================================

# Se crea una función para calcular el F1 Score

calculate_f1 <- function(true_labels, predicted_labels) {
  
  confusion <- confusionMatrix(as.factor(predicted_labels), as.factor(true_labels))$table
  
  TP <- confusion[2, 2]  
  FP <- confusion[1, 2]  
  FN <- confusion[2, 1]  
  
  # Calculate precision and recall
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  # Calculate F1-score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(f1)
}

#Ahora creamos una función que nos permita ajustar el F1 de manera manual para diferentes umbrales en caso de que los queramos cambiar.

calculate_f1_manual <- function(threshold, true_labels, predicted_probs) {
  preds <- ifelse(predicted_probs >= threshold, "Yes", "No")
  
  confusion <- confusionMatrix(as.factor(preds), as.factor(true_labels))$table
  
  TP <- confusion[2,2]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(f1)
}


#_____________Modelación______________________


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

modelo_RF3=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
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
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

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

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RF3ajust_SMOTE_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)



#Modelo 14 XGBoost ajustando umbral y f1 manual

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

modelo14_xgb_sub <- train(modelo_XGB2, 
                         data = sub_TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo14_xgb_sub

# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo14_xgb_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

#Buscamos el umbral que maximise F1 
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontramos ese umbral

umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

print(paste("Umbral Optimo:", umbral_optimo))       # 0.31
print(paste("F1 optimo de acuerdo a umbral:", f1_optimo))  # 0.643

# Entrenamos el modelo ahora con toda la muestra

set.seed(8545685)

modelo14_xgb <- train(modelo_XGB2, 
                     data = TRAIN, 
                     method = "xgbTree",        
                     metric = "F",               
                     trControl = ctrl,       
                     tuneGrid = grid_xg,  
                     nthread = 4)              

modelo14_xgb

# Se prepara la Submission

#Ajustar las probabilidades y el umbral para la submission

test_probs <- predict(modelo14_xgb, newdata = TEST, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora la submission

predictSample <- TEST %>%
  mutate(pobre_lab = test_preds_fin) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGBajust2_eta_0_05_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)




#_____________________________________________________

#Modelo 15: XGBoost con nuevo modelo y SMOTE

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



#Modelo 16 usando todas las variables


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


modelo_XGB3 = Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor


#Entrear el mdoelo con los subdatos de TRAIN

modelo16_xgb_sub <- train(modelo_XGB3, 
                          data = sub_TRAIN, 
                          method = "xgbTree",        
                          metric = "F",               
                          trControl = ctrl_smote,       
                          tuneGrid = grid_xg,  
                          nthread = 4)              

modelo16_xgb_sub

# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo16_xgb_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

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

modelo16_xgb <- train(modelo_XGB3, 
                      data = TRAIN, 
                      method = "xgbTree",        
                      metric = "F",               
                      trControl = ctrl_smote,       
                      tuneGrid = grid_xg,  
                      nthread = 4)              

modelo16_xgb

# Se prepara la Submission

#Ajustar las probabilidades y el umbral para la submission

test_probs <- predict(modelo9_xgb, newdata = TEST, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora la submission

predictSample <- TEST %>%
  mutate(pobre_lab = test_preds_fin) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGBajust3_smote_eta_0.05_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)
