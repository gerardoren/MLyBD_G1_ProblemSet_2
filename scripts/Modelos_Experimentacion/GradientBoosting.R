## ---------------------------
##
## Script name: AdaBoostM1_SMOTE.R
##
## Purpose of script:
##
## Author: Nicolas Lozano Huertas
##
## Date Created: 2025-04-08
##
## Email: n.lozanoh@uniandes.edu.co
##
## ---------------------------
##
## Notes:
##
## ---------------------------

## ---- Limpiamos el Entorno ----
rm(list = ls())

## ---- Cargamos paquetes ----

pckg <- c("adabag",
          "caret",
          "dplyr",
          "Metrics",
          "gbm")

invisible(lapply(pckg, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)){ 
    install.packages(pkg)}
  library(pkg, character.only = TRUE)}))

## ---- Cargamos datos ----
train <- readRDS("stores\\work\\train.rds")
test <- readRDS("stores\\work\\test.rds")

## ---- Revisamos balanceo ----
table(train$Pobre)
prop.table(table(train$Pobre))

## ---- Partimos la data para cv antes de enviar ----
inTRAIN <- createDataPartition(
  y = train$Pobre,  
  p = 0.7,          # 70% of the data for training
  list = FALSE      
)

sub_train <- train[inTRAIN, ]
sub_test  <- train[-inTRAIN, ]
sub_train$Pobre <- factor(sub_train$Pobre, levels = c("No", "Yes"))

## ---- Definimos control ----

ctrl <- trainControl(method = "cv",
                     number = 5,                  
                     classProbs = TRUE,           
                     summaryFunction = prSummary,
                     savePredictions = TRUE,      
                     verboseIter = T,
                     sampling = "smote")

## ---- Grilla para los parametros ----

grid <-  expand.grid(
  n.trees= c(100,150,300),
  interaction.depth=c(1,2,5),
  shrinkage=c(0.01),
  n.minobsinnode=c(5, 10))


## ---- Definimos el modelo ----


## ---- Entrenamos el modelo con los datos sub ----
set.seed(91519)
modelo_sub <- train(Pobre~Nper+num_ocupados+arrienda+num_menores+num_adulto_mayor+
                      maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
                      Jefe_segundo_empleo+Jefe_edad, 
                    data = sub_train, 
                    method = "gbm",        
                    metric = "F",               
                    trControl = ctrl,       
                    tuneGrid = grid) 

modelo_sub

## ---- Predecimos ----
test_probs <- predict(modelo_sub, newdata = sub_test, type = "prob")[, "Yes"]

## ---- Funcion para calcular F1 ----

calculate_f1_manual <- function(threshold, true_labels, predicted_probs) {
  preds <- factor(ifelse(predicted_probs >= threshold, "Yes", "No"), levels = c("No", "Yes"))
  
  confusion <- confusionMatrix(as.factor(preds), as.factor(true_labels))$table
  
  TP <- confusion[2,2]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(f1)
}

## ---- buscamos el umbral que maximiza F1 ----

umbrales <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(umbrales, calculate_f1_manual, true_labels = sub_test$Pobre, predicted_probs = test_probs)

f1_scores <- f1_scores[!is.na(f1_scores)] 

umbral_optimo <- umbrales[which.max(f1_scores)]
f1_optimo <- max(f1_scores)

print(paste("Umbral Optimo:", umbral_optimo))     
print(paste("F1 optimo de acuerdo a umbral:", f1_optimo))

## ---- Entrenamos modelo con toda la muestra ----
set.seed(91519)
modelo <- train(Pobre~Nper+num_ocupados+arrienda+num_menores+num_adulto_mayor+
                      maxEducLevel+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
                      Jefe_segundo_empleo+Jefe_edad, 
                    data = train, 
                    method = "gbm",        
                    metric = "F",               
                    trControl = ctrl,       
                    tuneGrid = grid)

## ---- Ajustamos probabilidades ----

test_probs <- predict(modelo, newdata = test, type = "prob")[, "Yes"]

test_preds_fin <- ifelse(test_probs >= umbral_optimo, "Yes", "No")

#Ahora la submission

predictSample <- test %>%
  mutate(pobre_lab = test_preds_fin) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre_lab = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre_lab)

head(predictSample)

write.csv(predictSample, "stores\\sub\\gradient_boosting.csv", row.names = FALSE)
