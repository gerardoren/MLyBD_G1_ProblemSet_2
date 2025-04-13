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

#Podemos generar variables a nivel de hogar a partir de la base de datos de personas:

# Realizamos el preprocesamiento para las bases de personas:
# Creamos variables usando unformacion por persona:
preproces_personas <- function(data,...){
  data<-data %>% mutate(
    mujer = ifelse(P6020==2,1,0),
    menor_de_edad = ifelse(P6040 < 16, 1, 0),
    mayor_de_edad = ifelse(P6040 > 65, 1, 0),
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
    Recibio_horasextra = ifelse(is.na(Recibio_horasextra), 0, Recibio_horasextra)
  ) %>%
    rename(edad = P6040) %>% 
    select(id, mujer, edad, menor_de_edad, mayor_de_edad, regimen_salud, EducLevel, Jefe_H, 
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
      num_mayores = sum(mayor_de_edad, na.rm = TRUE),
      prom_edad = mean(edad, na.rm =TRUE),
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
      select(id,mujer, edad,regimen_salud,EducLevel,Jefe_H, ocupado,Inactivo,
             desocupado,Tipo_primer_empleo,segundo_empleo,
             Recibio_horasextra)%>%
      rename(Jefe_H_mujer=mujer,
             Jefe_H_edad = edad, 
             Jefe_regimen_salud=regimen_salud,
             Jefe_EducLevel=EducLevel,
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

preproces_hogares <- function(data,...){
  data<-data %>% 
  mutate(
    arrienda=ifelse(P5090==3,1,0)
  ) %>%
    select(id, Clase,Dominio,arrienda,Nper,
           Npersug, any_of("Pobre"))
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

##-------------------------------------------------------------------------------##
#Para visualizar la distribución del desempleo en nuestra muestra, 
#generamos un gráfico de barras que muestra la proporción de individuos pobres y no pobres.

ggplot(TRAIN, aes(x = Pobre,y = after_stat(count / sum(count)), fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.05)) +
  scale_fill_manual(values = c("No" = "#CAFF70", "Yes"= "coral1")) +
  labs(x = "", y = "%")  

# se tiene un desbalance leve.



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




#---------------------Modelación----------------------------------#

#Modelo 1: Logit

#Modelo para predecir

modelo_logit1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_H_mujer+Jefe_desocupado+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

ctrl <- trainControl(method = "cv",
                     number = 5,                  # 5-fold cross-validation
                     classProbs = TRUE,           # Enable class probabilities
                     summaryFunction = prSummary, # Precision, recall, and F1-score
                     savePredictions = TRUE,      # Save predicted values
                     verboseIter = TRUE)          # Show iteration progress

# Modelo logit en sub-TRAIN data
model1_logit_sub <- train(modelo_logit1, 
                          data = sub_TRAIN, 
                          method = "glm", 
                          family = "binomial", 
                          metric = "F",               # F1-score as metric
                          trControl = ctrl)

model1_logit_sub

# Predecir las propabilidades en el sub_TEST 

TEST_preds <- predict(model1_logit_sub, newdata = sub_TEST, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = TEST_preds)
print(paste("F1-score Threshold:", f1_score))  #0.5603

#Ahora vamos a utilizar toda la base para hacer la predicción 

set.seed(123321)

model1_logit <- train(modelo_logit1, 
                      data = TRAIN, 
                      method = "glm", 
                      family = "binomial", 
                      metric = "F",               # F1-score as metric
                      trControl = ctrl)

model1_logit

#Preparamos los datos para la submission

predictSample <- TEST   %>% 
  mutate(pobre_lab = predict(model1_logit, newdata = TEST, type = "raw")) %>% 
  select(id,pobre_lab)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre_lab=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre_lab)

head(predictSample)

name<- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/01_Logit.csv") 
write.csv(predictSample,name, row.names = FALSE)



#Modelo 2: LDA 

#Modelo para predecir

modelo_lda1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_H_mujer+Jefe_desocupado+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


modelo2_lda_sub <- train(modelo_lda1, 
                        data = sub_TRAIN, 
                        method = "lda",          
                        metric = "F",             
                        trControl = ctrl)

modelo2_lda_sub

# Predecir las propabilidades en el sub_TEST 3

TEST_preds <- predict(modelo2_lda_sub, newdata = sub_TEST, type = "raw")

# F1 score fuera de muestra

f1_score <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = TEST_preds)
print(paste("F1-score LDA:", f1_score))  #0.5564

#Ahora preparamos para utilizar toda TRAIN data

modelo2_lda <- train(modelo_lda1, 
                    data = TRAIN, 
                    method = "lda",          
                    metric = "F",             
                    trControl = ctrl)

modelo2_lda

# Preparamos los datos para hacer la submission

predictSample <- TEST   %>% 
  mutate(pobre_lab = predict(modelo2_lda, newdata = TEST, type = "raw")) %>% 
  select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre_lab=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre_lab)

head(predictSample)

name<- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/02_LDA.csv") 
write.csv(predictSample,name, row.names = FALSE)


#Modelo 3: Elastic Net

#Grid de combinaciones

grid_EN <- expand.grid(
  alpha = seq(0.1, 1, length = 5),    
  lambda = 10^seq(-2, 1, length = 5)) 

#Modelo para predecir

modelo_EN1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


modelo3_en_sub <- train(modelo_EN1, 
                         data = sub_TRAIN, 
                        method = "glmnet",        # Elastic Net method
                        metric = "F",             # F1-score as metric
                        trControl = ctrl,         # Control setup
                        tuneGrid = grid_EN) # Hyperparameter grid


modelo3_en_sub

# Predecir las propabilidades en el sub_TEST 

TEST_preds <- predict(modelo3_en_sub, newdata = sub_TEST, type = "raw")

# F1 score fuera de muestra

f1_score <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = TEST_preds)
print(paste("F1-score Elastic Net:", f1_score))  #0.5564

#Ahora preparamos para utilizar toda TRAIN data

set.seed(096325)
modelo3_en <- train(modelo_EN1, 
                        data = TRAIN, 
                        method = "glmnet",        # Elastic Net method
                        metric = "F",             # F1-score as metric
                        trControl = ctrl,         # Control setup
                        tuneGrid = grid_EN) # Hyperparameter grid


modelo3_en

# Preparamos los datos para hacer la submission

predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo3_en, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)


name<- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/EN_lambda_0_001_alpha_0_1.csv") 
write.csv(predictSample,name, row.names = FALSE)




#Modelo 4: Random Forest

# Grid de combinaciones de hiperparámetros

grid_rf <- expand.grid(
  mtry = c(5, 6, 8),            
  splitrule = "gini",           
  min.node.size = c(10, 20))  

#Modelo a utilizar: 

modelo_RF1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

# Entrenamos Random forest en el sub_TRAIN

modelo4_rf_sub <- train(modelo_RF1, 
                       data = sub_TRAIN, 
                       method = "ranger",           
                       metric = "F",               
                       trControl = ctrl,          
                       tuneGrid = grid_rf,    
                       num.trees = 500)             

modelo4_rf_sub


# Predecir los class labels en sub_TEST
TEST_preds <- predict(modelo4_rf_sub, newdata = sub_TEST, type = "raw")

# F1 score fuera de muestra
f1_score <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = TEST_preds)
print(paste("F1-score Random Forest:", f1_score))  #0.552




set.seed(0987523)

# Ahora lo entrenamos utilizando todos los datos

modelo4_rf <- train(modelo_RF1, 
                    data = TRAIN, 
                    method = "ranger",           
                    metric = "F",               
                    trControl = ctrl,          
                    tuneGrid = grid_rf,    
                    num.trees = 500)

modelo4_rf

# Submission
predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo4_rf, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RF_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)


#Modelo 5: Random Forest ajustando threshold - umbral

# Grid de combinaciones de hiperparámetros

grid_rf <- expand.grid(
  mtry = 6,           
  splitrule = "gini",           
  min.node.size = 20)

#Modelo a utilizar: 

modelo_RF1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

# Entrenamos Random forest en el sub_TRAIN

modelo5_rf_sub <- train(modelo_RF1, 
                        data = sub_TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl,          
                        tuneGrid = grid_rf,    
                        num.trees = 500,
                        importance = "impurity")             

modelo5_rf_sub

# Porbabilidades predichas en sub_TEST
test_probs <- predict(modelo5_rf_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

# Umbral que maximice F1
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontrar el F1 más alto
umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)


# Valores F1
print(paste("Optimal Threshold:", umbral_optimo))       # 0.31
print(paste("F1-score at Optimal Threshold:", f1_optimo))  # 0.632

set.seed(3698521)

modelo5_rf <- train(modelo_RF1, 
                        data = TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl,          
                        tuneGrid = grid_rf,    
                        num.trees = 500,
                        importance = "impurity")  
modelo5_rf



# Submission
test_probs <- predict(modelo5_rf, newdata = TEST, type = "prob")[, "Yes"]

# Usamos el umbral optimo
final_test_preds <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo5_rf, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RFajust_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)



#Modelo 6: Random Forest ajustando threshold - umbral con SMOTE 

#OJO: Acá se pone el método de sampling de SMOTE que sirve para el modelo

ctrl_smote <- trainControl(method = "cv",
                           number = 5,                  
                           classProbs = TRUE,           
                           summaryFunction = prSummary,
                           savePredictions = TRUE,      
                           verboseIter = TRUE,
                           sampling = "smote")


grid_rf <- expand.grid(
  mtry = 6,            
  splitrule = "gini",           
  min.node.size = 20)  


#Modelo a utilizar: 

modelo_RF1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

# Entrenamos Random forest en el sub_TRAIN

modelo6_rf_sub <- train(modelo_RF1, 
                        data = sub_TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl_smote,          
                        tuneGrid = grid_rf,    
                        num.trees = 500,
                        importance = "impurity")             

modelo6_rf_sub

# Predecir probabilidades en el sub_TEST
TEST_probs <- predict(modelo6_rf_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

#Ahora vamos a buscar el umbral óptimo

umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = TEST_probs)

# Cuadramos el umbral ahora mirando el F1
umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

# Miramos el umbral y el F1 optimo

print(paste("Umbral Óptimo:", umbral_optimo))       # 0.45
print(paste("Socre F1 de acuerdo al umbral óptimo :", f1_optimo))  # 0.623

# Entrenamos ahora con todos los datos

set.seed(7412589)

modelo6_rf <- train(modelo_RF1, 
                    data = TRAIN, 
                    method = "ranger",           
                    metric = "F",               
                    trControl = ctrl_smote,          
                    tuneGrid = grid_rf,    
                    num.trees = 500,
                    importance = "impurity")   

modelo6_rf

# Ajustar las probabilidades y el umbral para la submission

test_probs <- predict(modelo6_rf, newdata = TEST, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora si se propara la submission

predictSample <- TEST %>% 
  mutate(pobre_lab = ifelse(test_preds_fin == "Yes", 1, 0)) %>% 
  select(id,pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RFajus_smote_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)




#Modelo 7: XGBosst

#Se plantea el grid en el que se va a mover el modelo

grid_xg <- expand.grid(
  nrounds = 200,         
  max_depth = c(4, 6),            
  eta = c(0.05, 0.1),            
  gamma = c(0,1),                     
  colsample_bytree = 0.8,         
  min_child_weight = 10,         
  subsample = 0.8               
)

#Modelo que se va a utilizar para entrenar

modelo_XGB1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_adulto_mayor+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


#Entrear el mdoelo con los subdatos de TRAIN

modelo7_xgb_sub <- train(modelo_XGB1, 
                        data = sub_TRAIN, 
                        method = "xgbTree",        
                        metric = "F",               
                        trControl = ctrl,       
                        tuneGrid = grid_xg,  
                        nthread = 4)              

modelo7_xgb_sub


# Predecimos ahora en el sub_TEST
test_preds <- predict(modelo7_xgb_sub, newdata = sub_TEST, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = test_preds)
print(paste("F1-score XGBoost:", f1_score))  #0.5825


#Ahora pasasmos a entrenarlo con toda la base TRAIN

set.seed(66542312)

modelo7_xgb <- train(modelo_XGB1, 
                         data = TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo7_xgb

#Preparamos la submission



predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo7_xgb, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGB_eta_0.2_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)


#Modelo 8 XGBoost ajustando umbral y f1 manual

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

modelo_XGB1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


#Entrear el mdoelo con los subdatos de TRAIN

modelo8_xgb_sub <- train(modelo_XGB1, 
                         data = sub_TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo8_xgb_sub

# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo8_xgb_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

#Buscamos el umbral que maximise F1 
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontramos ese umbral

umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

print(paste("Umbral Optimo:", umbral_optimo))       # 0.29
print(paste("F1 optimo de acuerdo a umbral:", f1_optimo))  # 0.638

# Entrenamos el modelo ahora con toda la muestra

set.seed(8545685)

modelo8_xgb <- train(modelo_XGB1, 
                         data = TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo8_xgb

# Se prepara la Submission

#Ajustar las probabilidades y el umbral para la submission

test_probs <- predict(modelo8_xgb, newdata = TEST, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora la submission

predictSample <- TEST %>%
  mutate(pobre_lab = test_preds_fin) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGBajust_eta_0_05_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)


#Modelo 9 XGBoost con umbral ajustado y SMOTE. 

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

modelo_XGB1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


#Entrear el mdoelo con los subdatos de TRAIN

modelo9_xgb_sub <- train(modelo_XGB1, 
                         data = sub_TRAIN, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl_smote,       
                         tuneGrid = grid_xg,  
                         nthread = 4)              

modelo9_xgb_sub

# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo9_xgb_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

#Buscamos el umbral que maximise F1 
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontramos ese umbral

umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

print(paste("Umbral Optimo:", umbral_optimo))       # 0.47
print(paste("F1 optimo de acuerdo a umbral:", f1_optimo))  # 0.628

# Entrenamos el modelo ahora con toda la muestra

set.seed(56856)

modelo9_xgb <- train(modelo_XGB1, 
                     data = TRAIN, 
                     method = "xgbTree",        
                     metric = "F",               
                     trControl = ctrl_smote,       
                     tuneGrid = grid_xg,  
                     nthread = 4)              

modelo9_xgb

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

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/XGBajust_smote_eta_0.05_gamma_0_n200.csv") 
write.csv(predictSample, name, row.names = FALSE)

#Modelo 10 AdaBoostM1

#Definimos la función para el modelo

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

#Proponemos el control

ctrl_ada<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)


#Planteamos la grilla para el modelo

adagrid<-  expand.grid(
  mfinal = c(50, 100, 150),
  maxdepth = c(1,2),
  coeflearn = c('Freud','Zhu'))

#Planteamos el modelo con el que vamos a trabajar

modelo_ADA1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

set.seed(654789321)

mdoelo10_ada_sub <- train(modelo_ADA1,
                       data = sub_TRAIN, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl_ada,
                       metric = "ROC",
                       tuneGrid=adagrid
)

modelo10_ada_sub


# Predecimos ahora en el sub_TEST
test_probs <- predict(modelo10_ada_sub, newdata = sub_TEST, type = "prob")[, "Yes"]





#Modelo 11: RF MTRY 5, umbral ajustado


# Grid de combinaciones de hiperparámetros

grid_rf <- expand.grid(
  mtry = 5,           
  splitrule = "gini",           
  min.node.size = 20)

#Modelo a utilizar: 

modelo_RF1=Pobre~Nper+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad

# Entrenamos Random forest en el sub_TRAIN

modelo11_rf_sub <- train(modelo_RF1, 
                        data = sub_TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl,          
                        tuneGrid = grid_rf,    
                        num.trees = 200,
                        importance = "impurity")             

modelo11_rf_sub

# Porbabilidades predichas en sub_TEST
test_probs <- predict(modelo11_rf_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

# Umbral que maximice F1
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontrar el F1 más alto
umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)


# Valores F1
print(paste("Optimal Threshold:", umbral_optimo))       # 0.31
print(paste("F1-score at Optimal Threshold:", f1_optimo))  # 0.633

set.seed(3698521)

modelo11_rf <- train(modelo_RF1, 
                    data = TRAIN, 
                    method = "ranger",           
                    metric = "F",               
                    trControl = ctrl,          
                    tuneGrid = grid_rf,    
                    num.trees = 200,
                    importance = "impurity")  
modelo11_rf



# Submission
test_probs <- predict(modelo11_rf, newdata = TEST, type = "prob")[, "Yes"]

# Usamos el umbral optimo
final_test_preds <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo11_rf, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RFajust_mtry_5_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)



#Modelo 12: Random Forest (todas las variables) ajustando threshold - umbral

# Grid de combinaciones de hiperparámetros

grid_rf <- expand.grid(
  mtry = 6,           
  splitrule = "gini",           
  min.node.size = 20)

#Planteamos un nuevo modelo:

modelo_RF2 <- Pobre ~ Dominio + arrienda + Nper + num_mujeres + num_menores + num_mayores + maxEducLevel + num_ocupados + num_inactivos + Jefe_H_mujer + Jefe_H_edad + Jefe_regimen_salud + Jefe_EducLevel + Jefe_Tipo_primer_empleo

# Entrenamos Random forest en el sub_TRAIN

modelo12_rf_sub <- train(modelo_RF2, 
                        data = sub_TRAIN, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl,          
                        tuneGrid = grid_rf,    
                        num.trees = 200,
                        importance = "impurity")             

modelo12_rf_sub

# Porbabilidades predichas en sub_TEST
test_probs <- predict(modelo12_rf_sub, newdata = sub_TEST, type = "prob")[, "Yes"]

# Umbral que maximice F1
umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_TEST$Pobre, predicted_probs = test_probs)

# Encontrar el F1 más alto
umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)


# Valores F1
print(paste("Optimal Threshold:", umbral_optimo))       # 0.35
print(paste("F1-score at Optimal Threshold:", f1_optimo))  # 0.6398

set.seed(3698521)

modelo12_rf <- train(modelo_RF2, 
                    data = TRAIN, 
                    method = "ranger",           
                    metric = "F",               
                    trControl = ctrl,          
                    tuneGrid = grid_rf,    
                    num.trees = 200,
                    importance = "impurity")  
modelo12_rf



# Submission
test_probs <- predict(modelo12_rf, newdata = TEST, type = "prob")[, "Yes"]

# Usamos el umbral optimo
final_test_preds <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

predictSample <- TEST %>% 
  mutate(pobre_lab = predict(modelo12_rf, newdata = TEST, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

name <- paste0("C:/Users/claud/OneDrive/Documents/OneDrive - Universidad de los andes/Universidad Los andes/Noveno semestre/Big data/taller 2/Data/submissions/RF2ajust_mtry_6_n500.csv") 
write.csv(predictSample, name, row.names = FALSE)
