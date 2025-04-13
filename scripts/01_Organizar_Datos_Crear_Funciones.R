## ---- Limpiar Entorno ----
rm(list=ls())

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

## ---- Cargue de Datos ----
train_hogares<-read.csv("stores\\raw\\train_hogares.csv")
train_personas<-read.csv("stores\\raw\\train_personas.csv")
test_hogares<-read.csv("stores\\raw\\test_hogares.csv")
test_personas<-read.csv("stores\\raw\\test_personas.csv")

## ---- Renombramos variables ----
rename_data_hogares <- function(data, train){
  data <- data %>% rename(
    dominio=Dominio,
    clase=Clase,
    cuartos=P5000,
    dormitorios=P5010,
    tipo_vivienda=P5090,
    cuota=P5100,
    arriendo_proyectado=P5130,
    arriendo=P5140,
    no_personas=Nper,
    no_personas_ug=Npersug,
    linea_indigencia=Li,
    linea_pobreza=Lp,
    factor_expansion=Fex_c,
    departamento=Depto,
    factor_expansion_dpto=Fex_dpto
  )
  
  if (train) {
    data <- data %>% rename(
      ingresos_unidad_gasto=Ingtotug,
      ingresos_arriendo_ug=Ingtotugarr,
      ingresos_pc_arriendo=Ingpcug,
      pobre=Pobre,
      indigente=Indigente,
      numero_pobres=Npobres,
      numero_indigentes=Nindigentes
    )
  }
  return(data)
}

test_hogares <- rename_data_hogares(test_hogares, F)
train_hogares <- rename_data_hogares(train_hogares, T)

rename_data_personas <- function(data, train){
  data <- data %>% rename(
    orden=Orden,
    clase=Clase,
    dominio=Dominio,
    sexo=P6020,
    anhos=P6040,
    parentesco_con_jefe=P6050,
    afiliado_salud=P6090,
    regimen_ss=P6100,
    max_educ=P6210,
    grado_aprobado=P6210s1,
    actividad_semana_pasada=P6240,
    oficio =Oficio,
    tiempo_trabajo=P6426,
    ocupacion_trabajo=P6430,
    horas_extra=P6510,
    primas=P6545,
    bonificaciones_month=P6580,
    valor_bonificaciones=P6585s1,
    incluyo_bonificaciones=P6585s2,
    subsidio_familiar=P6585s3,
    subsidio_educativo=P6585s4,
    ingreso_alimentos=P6590,
    recibe_vivienda=P6600,
    transporte_empresa=P6610,
    ingresos_especie=P6620,
    prima_servicios=P6630s1,
    prima_navidad=P6630s2,
    prima_vacaciones=P6630s3,
    viaticos=P6630s4,
    bonificaciones_year=P6630s6,
    horas_semana=P6800,
    personal_empresa=P6870,
    cotiza_pension=P6920,
    otro_trabajo=P7040,
    horas_segundo_puesto=P7045,
    puesto_segundo_trabajo=P7050,
    quiere_mas_horas=P7090,
    dilig_mas_horas=P7110,
    disp_mas_horas=P7120,
    cambio_trabajo=P7150,
    empezar_nuevo_puesto=P7160,
    puesto_ultimo_trabajo_deso=P7350,
    ingresos_trabajo_deso=P7422,
    ingresos_trabajo_inac=P7472,
    arriendos_pensiones=P7495,
    pension_vejez=P7500s2,
    pension_divorcio=P7500s3,
    otros_ingresos=P7505,
    remesas_nac=P7510s1,
    remesas_int=P7510s2,
    subsidios=P7510s3,
    intereses_inversiones=P7510s5,
    cesantias=P7510s6,
    otras_fuentes=P7510s7,
    edad_trabajar=Pet,
    ocupado=Oc,
    desocupado=Des,
    inactivo=Ina,
    factor_expansion=Fex_c,
    factor_expansion_dpto=Fex_dpto
  )
  
  if (train) {
    
  }
  return(data)
}

test_personas <- rename_data_personas(test_personas, F)
train_personas <- rename_data_personas(train_personas, T)


preproces_personas <- function(data,...){
  data<-data %>% mutate(
    mujer = ifelse(sexo==2,1,0),
    menor_de_edad = ifelse(anhos <= 16, 1, 0),
    adulto_mayor = ifelse(anhos >= 60, 1, 0),
    regimen_salud = ifelse(regimen_ss==4,3,regimen_ss),
    regimen_salud = ifelse(is.na(regimen_ss), 3, regimen_ss),
    EducLevel = ifelse(max_educ==9,0,max_educ),
    Jefe_H = ifelse(parentesco_con_jefe==1,1,0),
    ocupado = ifelse(is.na(ocupado),0,1),
    desocupado = ifelse(is.na(desocupado),0,1),
    Inactivo = ifelse(is.na(inactivo),0,1),
    Tipo_primer_empleo = ifelse(ocupacion_trabajo==9,0,ocupacion_trabajo),
    Tipo_primer_empleo = ifelse(is.na(Tipo_primer_empleo),1,Tipo_primer_empleo),
    segundo_empleo = ifelse(otro_trabajo==1,1,0),
    segundo_empleo = ifelse(is.na(segundo_empleo), 0, segundo_empleo),
    Recibio_horasextra = ifelse(horas_extra==1,1,0),
    Recibio_horasextra = ifelse(is.na(Recibio_horasextra), 0, Recibio_horasextra),
    cotiza_pension = ifelse(cotiza_pension == 2, 0, 1),
    subsidios = ifelse(subsidios==1, 1, 0)) %>%
    rename(edad = anhos) %>%
    select(id, mujer, edad, menor_de_edad, adulto_mayor, regimen_salud, EducLevel, Jefe_H, 
           ocupado,desocupado, Inactivo,Tipo_primer_empleo,segundo_empleo,
           Recibio_horasextra, subsidios, cotiza_pension, regimen_salud) 
  return(data)
}

train_personas <- preproces_personas(train_personas)
test_personas <- preproces_personas(test_personas)


## ---- Agregamos Data a Nivel Hogar ----

preproces_personas_agregacion <- function(data,...){
  agregacion <- data %>%
    group_by(id)%>%
    summarise(
      num_personas = n(),
      num_mujeres = sum(mujer,na.rm = TRUE),
      num_menores = sum(menor_de_edad, na.rm = TRUE),
      num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      num_ocupados = sum(ocupado, na.rm=TRUE),
      num_desocupados = sum(desocupado, na.rm = TRUE),
      num_inactivos = sum(Inactivo, na.rm = TRUE),
      num_con_segundo_empleo = sum(segundo_empleo, na.rm = TRUE),
      num_recibieron_hrextra = sum(Recibio_horasextra, na.rm = TRUE),
      
      prop_adulto_mayor = mean(adulto_mayor, na.rm = TRUE),
      prop_mujeres = mean(mujer, na.rm = TRUE),
      prop_menores = mean(menor_de_edad, na.rm = TRUE),
      prop_ocupados = mean(ocupado, na.rm =TRUE),
      prop_desocupado = mean(desocupado, na.rm = TRUE),
      prop_inactivos = mean(Inactivo, na.rm = TRUE),
      prop_subsidio = mean(subsidios, na.rm = TRUE),
      prop_pension = mean(cotiza_pension,na.rm = TRUE),
      prop_ss = mean(regimen_salud,na.rm = TRUE),
      meanEducLevel = mean(EducLevel, na.rm = TRUE)
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

## ---- Variables nivel hogar ----
preproces_hogares <- function(data, ...) {
  data <- data %>%
    mutate(arrienda = ifelse(tipo_vivienda == 3, 1, 0))
  data$dominio <- as.character(data$dominio)
  data$dominio[data$dominio != "RURAL"] <- "urbano"
  data$dominio[data$dominio == "RURAL"] <- "rural"
  data$dominio <- factor(data$dominio)
  names(data)[names(data) == "dormitorios"] <- "pers_por_cuarto"
  data <- data %>%
    select(id, clase, dominio, arrienda, no_personas, pers_por_cuarto, any_of("pobre"))
  
  return(data)
}


train_hogares<-preproces_hogares(train_hogares)
test_hogares<-preproces_hogares(test_hogares)

# Unimos las bases de datos correspondientes a nivel hogar


TRAIN <-merge(train_hogares,train_personas)
TEST <- merge(test_hogares,test_personas)



#convertimos las variables a formatos adecuados
TRAIN<- TRAIN %>% 
  mutate(pobre=factor(pobre,
                      levels=c(0,1),labels=c("No","Yes")),
         dominio=factor(dominio),
         clase=factor(clase),
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
  mutate(dominio=factor(dominio),
         clase=factor(clase),
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
  "id", "pobre", "dominio", "arrienda", "num_personas", "pers_por_cuarto",
  "num_mujeres", "num_menores", "num_adulto_mayor", "maxEducLevel",
  "num_ocupados", "num_inactivos", "num_con_segundo_empleo",
  "num_recibieron_hrextra", "Jefe_H_mujer", "Jefe_regimen_salud",
  "Jefe_EducLevel", "Jefe_desocupado", "Jefe_Tipo_primer_empleo",
  "Jefe_edad", "Jefe_adulto_mayor","Jefe_segundo_empleo", "meanEducLevel", 
  "prop_mujeres", "prop_menores","prop_ocupados","prop_desocupado", "prop_adulto_mayor",
  "prop_inactivos","prop_subsidio","prop_ss","prop_pension"
)

TRAIN <- TRAIN[, vars_deseadas_TRAIN]

vars_deseadas_TEST<- c(
  "id", "dominio", "arrienda", "num_personas", "pers_por_cuarto",
  "num_mujeres", "num_menores", "num_adulto_mayor", "maxEducLevel",
  "num_ocupados", "num_inactivos", "num_con_segundo_empleo",
  "num_recibieron_hrextra", "Jefe_H_mujer", "Jefe_regimen_salud",
  "Jefe_EducLevel", "Jefe_desocupado", "Jefe_Tipo_primer_empleo",
  "Jefe_edad", "Jefe_adulto_mayor", "Jefe_segundo_empleo", "meanEducLevel", 
  "prop_mujeres", "prop_menores","prop_ocupados","prop_desocupado", "prop_adulto_mayor",
  "prop_inactivos","prop_subsidio","prop_ss","prop_pension"
)

TEST <- TEST[, vars_deseadas_TEST]

saveRDS(TRAIN, file = "stores\\work\\trainv2.rds")
saveRDS(TEST, file = "stores\\work\\testv2.rds")

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






