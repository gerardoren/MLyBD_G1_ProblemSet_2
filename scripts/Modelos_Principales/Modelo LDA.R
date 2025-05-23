#Modelo para predecir

modelo_lda1=pobre~num_personas+num_ocupados+arrienda+num_menores+num_mayores+maxEducLevel+Jefe_H_mujer+Jefe_desocupado+Jefe_regimen_salud+Jefe_Tipo_primer_empleo+Jefe_segundo_empleo+Jefe_H_edad


modelo2_lda_sub <- train(modelo_lda1, 
                        data = sub_TRAIN, 
                        method = "lda",          
                        metric = "F",             
                        trControl = ctrl)

modelo2_lda_sub

# Predecir las propabilidades en el sub_TEST 3

TEST_preds <- predict(modelo2_lda_sub, newdata = sub_TEST, type = "raw")

# F1 score fuera de muestra

f1_score <- calculate_f1(true_labels = sub_TEST$pobre, predicted_labels = TEST_preds)
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

name<- paste0("stores\\sub\\02_LDA.csv") 
write.csv(predictSample,name, row.names = FALSE)

