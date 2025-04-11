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
              "pROC",
              "rpart",
              "rpart.plot",
              "ranger",
              "randomForest",
              "parallel",
              "doParallel",
              "adabag",
              "themis",
              "rattle",
              "stargazer")

invisible(lapply(Packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)){ 
    install.packages(pkg)}
  library(pkg, character.only = TRUE)}))

# Recolección de los datos:
train_hogares<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\train_hogares.csv")
train_personas<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\train_personas.csv")
test_hogares<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\test_hogares.csv")
test_personas<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\test_personas.csv")

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

##-------------------------------------------------------------------------------##

# Particion del set de entrenamiento:

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





##-------------------------------------------------------------------------------##

##-------------------------------------------------------------------------------##


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






