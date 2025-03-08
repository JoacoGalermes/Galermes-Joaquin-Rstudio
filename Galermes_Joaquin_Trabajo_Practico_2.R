# Joaquin Galermes
# DNI: 42.816.297
# Trabajo Práctico Módulo 2 - Curso de R - UTN

# Consignas

# La idea principal es que puedan realizar un análisis de un dataset a elección, mediante el tratamiento de los datos 
#y algunas visualizaciones con el fin de poder responder las hipótesis planteadas.

# 1) Deberán seleccionar un dataset (puede ser público o de sus trabajos) que contenga como
# mínimo 50.000 filas y 10 columnas con variedad en sus tipos, por ej: numéricas, fecha, categóricas, etc.

# 2) Una vez obtenido el dataset, la idea es que planteen 4 o 5 preguntas de interés sobre el mismo
# para poder responderlas posteriormente.

# 3) Sera necesario que carguen el archivo y realicen un chequeo/limpieza de los datos, es decir,
# verificar la integridad de los mismos.

# 4) Realizar un análisis exploratorio de datos considerando cuestiones generales como, por
# ejemplo: tipos de datos, distribuciones, valores atípicos, correlación entre las variables, etc.

# 5) Responder las preguntas planteadas en el punto 2 mediante el uso de tablas de contingencia y
# gráficos relevantes. (Serán necesarios como mínimo 5 gráficos, 3 de ellos distintos).

# Resolución de Trabajo Práctico

install_if_not_installed <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
}

install_if_not_installed("ggplot2")
install_if_not_installed("dplyr")
install_if_not_installed("corrplot")
install_if_not_installed("tidyr")
install_if_not_installed("lubridate")
install_if_not_installed("gmodels")
install_if_not_installed("ggpubr")


library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(lubridate)
library(gmodels)
library(ggpubr)


# Ejercicio 1

# Cargar el dataset
crime_data <- read.csv("C:/Users/joaquin.galermes/OneDrive - estremar.com/Escritorio/Curso de R/Unidad 2/Crime_Data_from_2020_to_Present.csv")

# Visualizamos la estructura y los primeros datos
dim(crime_data)
str(crime_data)
head(crime_data)

# Ejercicio 2

# Preguntas posibles para resolver:

# a) ¿Cuales son los tipos de crimenes más comunes?
# b) Distribución de crímenes por día de la semana
# c) ¿En donde se tiende a reportar más crimenes?
# d) ¿Hay una relación entre el sexo y el tipo de crimen? 
# e) ¿Cómo se distribuyen las edades de los crimenes?

# Ejercicio 3

# Visualizando los datos y para trabajar correctamente, "Date.Rptd" y "DATE.OCC" está en formato de texto, entonces lo convertimos en formato fecha 

crime_data$Date.Rptd <- as.POSIXct(crime_data$Date.Rptd, format = "%m/%d/%Y %I:%M:%S %p")
crime_data$DATE.OCC <- as.POSIXct(crime_data$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")
crime_data$YEAR.OCC <- year(crime_data$DATE.OCC)
crime_data$MONTH.OCC <- month(crime_data$DATE.OCC, label = TRUE)
crime_data$Day.Week <- weekdays(crime_data$DATE.OCC)
crime_data$Month.Year <- format(crime_data$DATE.OCC, "%Y-%m")


# A su vez, para trabajar mejor, decido convertir columnas categóricas a factor
crime_data$AREA.NAME <- as.factor(crime_data$AREA.NAME)
crime_data$Vict.Sex <- as.factor(crime_data$Vict.Sex)
crime_data$Vict.Descent <- as.factor(crime_data$Vict.Descent)
crime_data$Premis.Desc <- as.factor(crime_data$Premis.Desc)
crime_data$Status.Desc <- as.factor(crime_data$Status.Desc)

# Luego de las conversiones, vemos los valores nulos
colSums(is.na(crime_data))

# Como al visualizar los datos, ve que hay bastantes valores nulos, decido ver el porcentaje de valores faltantes
# Si la cantidad de valores faltantes son mayor del 50%, decido eliminar la columna, en caso contrario para
# las variables númericas utilizo la mediana, y para las categóricas utilizo la moda

# Porcentaje de valores faltantes
missing_percentage <- colSums(is.na(crime_data)) / nrow(crime_data) * 100
print(missing_percentage)

# Si cumple la condición de > 50%, elimino la columna
crime_data <- crime_data[, missing_percentage <= 50]

# Función para calcular la moda
calculo_de_moda <- function(x) {
  moda <- unique(x)
  moda[which.max(tabulate(match(x, moda)))]
}

# Para los valores faltantes utilizamos la moda para las categóricas y mediana para las númericas
for (col in colnames(crime_data)) {
  if (any(is.na(crime_data[[col]]))) {
    if (is.numeric(crime_data[[col]])) {
      crime_data[[col]][is.na(crime_data[[col]])] <- median(crime_data[[col]], na.rm = TRUE)
    } else if (is.factor(crime_data[[col]]) || is.character(crime_data[[col]])) {
      crime_data[[col]][is.na(crime_data[[col]])] <- calculo_de_moda(crime_data[[col]])
    }
  }
}

# Chequeamos que no haya valores faltantes
colSums(is.na(crime_data))

# Ejercicio 4

# Paso a ver las dispersión y como están distribuidas las variables relevantes para responder nuestras preguntas

# Gráfico de dispersión entre la edad de las víctimas y el tiempo del crimen
plot(crime_data$Vict.Age, crime_data$TIME.OCC, 
     main = "Correlación entre edad de las víctimas y tiempo del crimen",
     xlab = "Edad de las víctimas",
     ylab = "Tiempo del crimen",
     col = "blue",
     pch = 16)

# Histograma de la distribución de edades de las víctimas
hist(crime_data$Vict.Age, 
     main = "Distribución de edades de las víctimas",
     xlab = "Edad",
     ylab = "Frecuencia",
     col = "lightblue",
     breaks = 30)

# Boxplot de la edad de las víctimas
boxplot(crime_data$Vict.Age, 
        main = "Boxplot de la edad de las víctimas",
        ylab = "Edad",
        col = "lightblue")

# Para definir y ver los valores atípicos, utilizo Rango intercuartílico (IQR)
Q1 <- quantile(crime_data$Vict.Age, 0.25, na.rm = TRUE)
Q3 <- quantile(crime_data$Vict.Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definir los límites inferior y superior
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar valores atípicos
outliers <- crime_data$Vict.Age[crime_data$Vict.Age < limite_inferior | crime_data$Vict.Age > limite_superior]
print(outliers)

crime_data_sin_outliers <- crime_data[crime_data$Vict.Age >= limite_inferior & crime_data$Vict.Age <= limite_superior, ]

crime_data <- crime_data_sin_outliers

# Histograma del tiempo del crimen
hist(crime_data$TIME.OCC, 
     main = "Distribución del tiempo del crimen",
     xlab = "Tiempo del crimen (HHMM)",
     ylab = "Frecuencia",
     col = "lightgreen",
     breaks = 24)  # 24 horas

# Boxplot del tiempo del crimen
boxplot(crime_data$TIME.OCC, 
        main = "Boxplot del tiempo del crimen",
        ylab = "Tiempo del crimen (HHMM)",
        col = "lightgreen")

# Matriz de correlación

# Selecciono las columnas numéricas
numeric_columns <- crime_data[, sapply(crime_data, is.numeric)]

# Calculo de la matriz de correlación
correlation_matrix <- cor(numeric_columns, use = "complete.obs")
print(correlation_matrix)

# Gráfico de la matriz de correlación
corrplot(correlation_matrix, method = "color")

# Ejercicio 5

# a) ¿Cuales son los tipos de crimenes más comunes?

# Tabla de Contingencia
contador_de_crimines <- table(crime_data$Crm.Cd.Desc)
print(contador_de_crimines)

# Gráfico de barras
barplot(sort(contador_de_crimines, decreasing = TRUE)[1:10], 
        main = "Top 10 tipos de crimen más comunes",
        xlab = "Tipo de crimen",
        ylab = "Frecuencia",
        col = "lightblue"
        )

# b) Distribución de crímenes por día de la semana
colnames(crime_data)

# ggplot(crime_data, aes(x = factor(Day.Week, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")))) +
#  geom_bar(fill = "blue") +
#  labs(title = "Distribución de crímenes por día de la semana", x = "Día de la semana", y = "Frecuencia")

# No se puede responder ya que poseía demasiados valores nulos

# c) ¿En donde se tiende a reportar más crimenes?

# Tabla de Contingencia

contador_de_areas <- table(crime_data$AREA.NAME)
print(contador_de_areas)

# Gráfico de barras
barplot(sort(contador_de_areas, decreasing = TRUE)[1:10], 
        main = "Top 10 áreas con más crímenes",
        xlab = "Área",
        ylab = "Frecuencia",
        col = "lightgreen"
        )

# d) ¿Hay una relación entre el tipo de crimen y el uso de armas? 

# Seleccionar los 5 tipos de armas más comunes
top_weapon_crimes <- names(sort(table(crime_data$Weapon.Desc), decreasing = TRUE)[1:5])

# Filtrar el dataset para incluir solo los tipos de armas más comunes y eliminar valores no válidos
crime_data_top <- crime_data %>%
  filter(Weapon.Desc %in% top_weapon_crimes) %>%
  filter(!is.na(Weapon.Desc) & Weapon.Desc != "") %>%
  filter(!is.na(Crm.Cd.Desc))

ggplot(crime_data_top, aes(x = as.factor(Crm.Cd), fill = Weapon.Desc)) +
  geom_bar(position = "stack", aes(y = after_stat(count) / 1000)) +  # Dividir por 1,000
  labs(title = "Relación entre tipo de crimen y uso de armas", 
       x = NULL, 
       y = "Frecuencia (en miles)",
       fill = "Tipo de arma") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())  

# e) ¿Cómo se distribuyen las edades de los crimenes?

# Histograma de la distribución de edades de las víctimas
hist(crime_data$Vict.Age, 
     main = "Distribución de edades de las víctimas",
     xlab = "Edad",
     ylab = "Frecuencia",
     col = "lightblue",
     breaks = 30)

# Guardado de trabajo en entorno R.Data
save.image("C:/Users/joaquin.galermes/OneDrive - estremar.com/Escritorio/Curso de R/Unidad 2/TP_Modulo_2_Galermes_Joaquin.RData")


