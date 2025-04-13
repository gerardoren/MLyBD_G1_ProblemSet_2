
# ARBOLES DE DECISION usando CART:

form_modelo_CART1=form_modelo_logit1=Pobre~Dominio+arrienda + Nper + pers_por_cuarto + num_mujeres+ num_menores+
  num_adulto_mayor+ maxEducLevel+ num_ocupados + num_inactivos+ 
  num_con_segundo_empleo + num_recibieron_hrextra+ Jefe_H_mujer+
  Jefe_regimen_salud+ Jefe_EducLevel+ Jefe_desocupado+ 
  Jefe_Tipo_primer_empleo+ Jefe_edad+Jefe_adulto_mayor

CART_modelo_1 <- rpart(form_modelo_CART1,
                       data=sub_TRAIN,
                       method = "class",
                       parms = list(split = "Gini"))
CART_modelo_1

prp(CART_modelo_1, 
    under = TRUE, #Muestra información extra debajo del nodo, como por ejemplo el valor predicho o número de observaciones.
    branch.lty = 2,    #Estilo de línea de las ramas
    yesno = 2, #Agrega "sí" o "no" ; 2 coloca los "sí/no" cerca del nodo padre.
    faclen = 0, #Muestra los nombres completos de los niveles de factores
    varlen=15, #Largo máximo del nombre de cada variable
    box.palette = "-RdYlGn") #Paleta de colores para las cajas de los nodos.


# Error de test del modelo final
predicciones_rpart_CART_modelo_1 <- data.frame(
  obs = sub_TEST$Pobre, # Observados
  pred = predict(CART_modelo_1, newdata = sub_TEST, type = "class")    ## Predichos
)


table(predicciones_rpart_CART_modelo_1$obs, predicciones_rpart_CART_modelo_1$pred)

# F1 score fuera de muestra
f1_score_CART_modelo_1 <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = predicciones_rpart_CART_modelo_1$pred)
print(paste("F1-score CART modelo 1:", f1_score_CART_modelo_1))  #0.4504


##------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##


# Arboles de desicion evitando sobreajuste usando Cost complexity pruning:


# Modelo CART poda 1, con smote y sin optimizacion de umbral:
# arbol con condicion min obs por hojas
minbucket_tree <- rpart::rpart(
  form_modelo_CART1, 
  data = sub_TRAIN, 
  method = "class", 
  minbucket = 1, # Numero minimo de obs en hojas
  cp = 0 # alpha para podar el arbol
) 
minbucket_tree
prp(minbucket_tree, 
    under = TRUE, 
    branch.lty = 2,
    yesno = 2, 
    faclen = 0,
    varlen=15,
    box.palette = "-RdYlGn")

# Empiezo a podar
fiveStats <- function(...) {
  c(
    twoClassSummary(...),
    defaultSummary(...)
  )
}

# Configuración de validación cruzada
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     sampling = "smote",
                     verbose = FALSE,
                     savePredictions = TRUE)

# Grilla de valores de cp para probar
grid <- expand.grid(cp = c(
  seq(0.0005, 0.02, by = 0.0005),  
  seq(0.025, 0.1, by = 0.005)      
))
# Entrenamiento con CV para encontrar mejor cp
cv_tree <- train(form_modelo_CART1,
                 data = sub_TRAIN,
                 method = "rpart",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric = "F1")

# Mejor valor de cp encontrado
best_cp <- cv_tree$bestTune$cp
print(paste("Mejor cp encontrado:", best_cp))

# Poda del árbol original con el mejor cp
minbucket_tree_podado <- prune(minbucket_tree, cp = best_cp)

# Visualizar el árbol podado
prp(minbucket_tree_podado,
    under = TRUE, 
    branch.lty = 2, 
    yesno = 2, 
    faclen = 0, 
    varlen = 15,
    box.palette = "-RdYlGn")

# Predicciones fuera de muestra
predicciones_podado <- data.frame(
  obs = sub_TEST$Pobre,
  pred = predict(minbucket_tree_podado, newdata = sub_TEST, type = "class")
)

# Matriz de confusión
table(predicciones_podado$obs, predicciones_podado$pred)

# F1-score fuera de muestra
f1_score_podado <- calculate_f1(true_labels = sub_TEST$Pobre, predicted_labels = predicciones_podado$pred)
print(paste("F1-score árbol podado smote sin opt de umbral:", f1_score_podado))


#sin smote 0.576918597880517
# con smote: 0.576918597880517"



##------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

#MODELO CART PODA 2 con smote y optimizacion de umbral.

# arbol con condicion min obs por hojas
minbucket_tree <- rpart::rpart(
  form_modelo_CART1, 
  data = sub_TRAIN, 
  method = "class", 
  minbucket = 1, # Numero minimo de obs en hojas
  cp = 0 # alpha para podar el arbol
) 
minbucket_tree

# Empiezo a podar
fiveStats <- function(...) {
  c(
    twoClassSummary(...),
    defaultSummary(...)
  )
}

# Configuración de validación cruzada
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     sampling = "smote",
                     verbose = FALSE,
                     savePredictions = TRUE)

# Grilla de valores de cp para probar
grid <- expand.grid(cp = c(
  seq(0.0005, 0.02, by = 0.0005), 
  seq(0.025, 0.1, by = 0.005)     
))
# Entrenamiento con CV para encontrar mejor cp
cv_tree <- train(form_modelo_CART1,
                 data = sub_TRAIN,
                 method = "rpart",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric = "F1")

# Mejor valor de cp encontrado
best_cp <- cv_tree$bestTune$cp
print(paste("Mejor cp encontrado:", best_cp))

# Poda del árbol original con el mejor cp
minbucket_tree_podado <- prune(minbucket_tree, cp = best_cp)

# Visualizar el árbol podado
prp(minbucket_tree_podado,
    under = TRUE, 
    branch.lty = 2, 
    yesno = 2, 
    faclen = 0, 
    varlen = 15,
    box.palette = "-RdYlGn")

# Predicciones fuera de muestra
# Paso 1: Predecimos probabilidades (NO clase)
probs <- predict(minbucket_tree_podado, newdata = sub_TEST, type = "prob")[, "Yes"]  # Asumiendo que "Yes" es la clase positiva

# Paso 2: Buscamos el mejor threshold que maximiza el F1-score
library(MLmetrics)

thresholds <- seq(0.1, 0.9, by = 0.01)  # Umbrales posibles
f1_scores <- sapply(thresholds, function(t) {
  pred_labels <- ifelse(probs > t, "Yes", "No")
  F1_Score(y_pred = as.factor(pred_labels), y_true = sub_TEST$Pobre, positive = "Yes")
})

# Paso 3: Umbral óptimo
best_threshold <- thresholds[which.max(f1_scores)]
cat("Mejor umbral según F1:", best_threshold, "\n")

# Paso 4: Predicciones con el mejor threshold
final_preds <- ifelse(probs > best_threshold, "Yes", "No")

# Paso 5: Nueva matriz de confusión y nuevo F1
cat("Matriz de confusión con umbral optimizado:\n")
print(table(Real = sub_TEST$Pobre, Predicha = final_preds))

# Nuevo F1-score optimizado
f1_final <- F1_Score(y_pred = as.factor(final_preds), y_true = sub_TEST$Pobre, positive = "Yes")
cat("F1-score arbol con poda, smote y con umbral optimizado:", round(f1_final, 4), "\n")

#sin smote 0.576918597880517
# con smote y PC recall curve 0.5982 

##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##




# Bagging forest:

# === 1. Definir la grilla de hiperparámetros ===
grid <- expand.grid(
  mtry = 19,
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
  Bagging <- ranger::ranger(
    formula = Pobre ~ ., 
    data = sub_TRAIN, 
    num.trees = 500,
    mtry = mtry_val,
    min.node.size = min_node_val,
    importance = "impurity",
    probability = TRUE,
    seed = 123
  )
  
  # 3. Predicciones y optimización de umbral (igual que antes)
  Bagging_probs <- predict(Bagging, data = sub_TEST)$predictions[, "Yes"]
  
  thresholds <- seq(0.1, 0.9, by = 0.01)
  f1_scores <- sapply(thresholds, function(t) {
    pred_labels <- ifelse(Bagging_probs > t, "Yes", "No")
    F1_Score(y_pred = as.factor(pred_labels), y_true = sub_TEST$Pobre, positive = "Yes")
  })
  
  best_threshold <- thresholds[which.max(f1_scores)]
  cat("Mejor umbral según F1:", best_threshold, "\n")
  
  final_preds <- ifelse(Bagging_probs > best_threshold, "Yes", "No")
  cat("Matriz de confusión con ranger:\n")
  print(table(Real = sub_TEST$Pobre, Predicha = final_preds))
  
  f1_final <- F1_Score(y_pred = as.factor(final_preds), y_true = sub_TEST$Pobre, positive = "Yes")
  cat("F1-score optimizado con ranger:", round(f1_final, 4), "\n") # F1 =  
  
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
#   19             1            0.4 0.5530288

umbral_optimo <- mejor_modelo$best_threshold

# vuelvo a entrenar el modelo pero con los mejores hiperparametrosy 
#con TRAIN total para luego hacerlo con TEST:
Bagging_optimo <- ranger::ranger(
  formula = Pobre ~ ., 
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
    pobre_lab = predict(Bagging_optimo, data = TEST, type = "response")$predictions[, "Yes"],
    
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
  "Bagging_mtry_", mejor_modelo$mtry,
  "_min.node.size_", mejor_modelo$min.node.size,
  "_best_threshold_", round(umbral_optimo, 2), ".csv") 

write.csv(predictSample,name, row.names = FALSE)



##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
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
    formula = Pobre ~ ., 
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
    F1_Score(y_pred = as.factor(pred_labels), y_true = sub_TEST$Pobre, positive = "Yes")
  })
  
  best_threshold <- thresholds[which.max(f1_scores)]
  cat("Mejor umbral según F1:", best_threshold, "\n")
  
  final_preds <- ifelse(rf_probs > best_threshold, "Yes", "No")
  cat("Matriz de confusión con ranger:\n")
  print(table(Real = sub_TEST$Pobre, Predicha = final_preds))
  
  f1_final <- F1_Score(y_pred = as.factor(final_preds), y_true = sub_TEST$Pobre, positive = "Yes")
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
  formula = Pobre ~ ., 
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
  "Random_forest_mtry_", mejor_modelo$mtry,
  "_min.node.size_", mejor_modelo$min.node.size,
  "_best_threshold_", round(umbral_optimo, 2), ".csv") 

write.csv(predictSample,name, row.names = FALSE)
##-----------------------------------------------------------------------------##




