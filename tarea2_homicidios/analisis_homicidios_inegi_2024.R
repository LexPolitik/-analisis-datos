####################################################################################################
# Área de trabajo de Víctor Castellanos
#
# Prueba técnica – Analista de Datos
#
# Tarea 2 – Análisis exploratorio de homicidios en México
#
# Descripción:
# Este script tiene como objetivo leer y explorar los microdatos de defunciones registradas
# por homicidio en México, a partir de los microdatos oficiales del INEGI (Estadísticas de
# Defunciones Registradas). El análisis se enfoca en identificar patrones relevantes de
# violencia letal con perspectiva territorial y de género, con fines descriptivos y
# exploratorios.
#
# Periodo de análisis:
# Año 2024
#
# Fuente de datos:
# Instituto Nacional de Estadística y Geografía (INEGI)
# Estadísticas de Defunciones Registradas, microdatos 2024
#
# Notas sobre los datos:
# Los microdatos originales no se incluyen en el repositorio por restricciones de tamaño.
# Se asume que los archivos .dbf se encuentran almacenados localmente en la carpeta:
# tarea2_homicidios/data_raw/
#
# Autor:
# Víctor Castellanos
#
# Fecha:
# Diciembre de 2025
####################################################################################################
# Limpiar el entorno de trabajo
rm(list = ls())

# Definir directorio de trabajo
setwd("~/Documents/-analisis-datos/tarea2_homicidios")

# Cargar librería para leer archivos DBF
library(foreign)

# Leer microdatos de defunciones (INEGI 2024)
defunciones <- read.dbf("data_raw/DEFUN24.dbf", as.is = TRUE)

dim(defunciones)
names(defunciones)
table(defunciones$TIPO_DEFUN)


#Estadística descriptiva#

# Quitar notación científica
options(scipen = 999)

# --------------------------------------------------
# 1. Tamaño del fenómeno
# --------------------------------------------------
total_homicidios <- nrow(homicidios)
total_homicidios


# --------------------------------------------------
# 2. Distribución por sexo
# --------------------------------------------------
sexo_abs <- table(homicidios$SEXO)
sexo_abs

sexo_pct <- prop.table(sexo_abs) * 100
sexo_pct


# --------------------------------------------------
# 3. Distribución por grupo de edad
# --------------------------------------------------
edad_abs <- table(homicidios$EDAD_AGRU)
edad_abs

edad_pct <- prop.table(edad_abs) * 100
edad_pct


# --------------------------------------------------
# 4. Cruce sexo × grupo de edad
# --------------------------------------------------
sexo_edad_abs <- table(homicidios$SEXO, homicidios$EDAD_AGRU)
sexo_edad_abs

sexo_edad_pct <- prop.table(sexo_edad_abs, margin = 1) * 100
sexo_edad_pct


# --------------------------------------------------
# 5. Distribución territorial: entidades federativas
# --------------------------------------------------
entidad_abs <- table(homicidios$ENT_OCURR)
entidad_abs

# Top 10 entidades con más homicidios
entidad_top10 <- sort(entidad_abs, decreasing = TRUE)[1:10]
entidad_top10

# Porcentaje que representan
entidad_top10_pct <- prop.table(entidad_top10) * 100
entidad_top10_pct


# --------------------------------------------------
# 6. Distribución territorial: municipios (opcional)
# --------------------------------------------------
mun_top10 <- sort(table(homicidios$MUN_OCURR), decreasing = TRUE)[1:10]
mun_top10


# --------------------------------------------------
# 7. Resumen descriptivo básico (objetos finales)
# --------------------------------------------------
list(
  total_homicidios = total_homicidios,
  sexo_absoluto = sexo_abs,
  sexo_porcentaje = sexo_pct,
  edad_absoluto = edad_abs,
  edad_porcentaje = edad_pct,
  top10_entidades = entidad_top10,
  top10_entidades_pct = entidad_top10_pct
)




# Defunciones por tipo de defunción 


# Quitar notación científica
options(scipen = 999)

# Convertir a porcentaje
tabla_tipo_pct <- prop.table(tabla_tipo) * 100

# Abrir ventana gráfica (Mac)
quartz()

# Ajustar márgenes
par(mar = c(10, 5, 4, 2))

# Gráfica de barras en porcentaje
barplot(
  tabla_tipo_pct,
  main = "Distribución porcentual de defunciones por tipo (México, 2024)",
  ylab = "Porcentaje del total de defunciones",
  col = "gray",
  las = 2,
  cex.names = 1.2
)


# (Opcional) Restablecer notación científica
#options(scipen = 0)


###Filtrar homicidios ####
homicidios <- defunciones[defunciones$TIPO_DEFUN == 2, ]
dim(homicidios)


##Análisis por sexo###

table(homicidios$SEXO)

###Análisis por edad ###

summary(homicidios$EDAD)
table(homicidios$EDAD_AGRU)


###Análisis sexo y grupo de edad ####
table(homicidios$SEXO, homicidios$EDAD_AGRU)
prop.table(table(homicidios$SEXO, homicidios$EDAD_AGRU), margin = 1) * 100

# Tabla de porcentajes por sexo (sin sexo no especificado)
tabla_pct <- prop.table(
  table(homicidios$SEXO, homicidios$EDAD_AGRU),
  margin = 1
)[c("1", "2"), ] * 100

tabla_pct
View(tabla_pct)

# Gráfica simple
barplot(
  tabla_pct,
  beside = TRUE,
  legend.text = c("Hombres", "Mujeres"),
  main = "Distribución porcentual de homicidios por sexo y grupo de edad (2024)",
  xlab = "Grupo de edad (EDAD_AGRU)",
  ylab = "Porcentaje"
)



# Distribución de homicidios por entidad federativa y cruce por sexo
# Quitar notación científica
options(scipen = 999)

# --------------------------------------------------
# 1. Catálogo de entidades federativas (INEGI)
# --------------------------------------------------
catalogo_entidades <- c(
  "01" = "Aguascalientes",
  "02" = "Baja California",
  "03" = "Baja California Sur",
  "04" = "Campeche",
  "05" = "Coahuila",
  "06" = "Colima",
  "07" = "Chiapas",
  "08" = "Chihuahua",
  "09" = "Ciudad de México",
  "10" = "Durango",
  "11" = "Guanajuato",
  "12" = "Guerrero",
  "13" = "Hidalgo",
  "14" = "Jalisco",
  "15" = "Estado de México",
  "16" = "Michoacán",
  "17" = "Morelos",
  "18" = "Nayarit",
  "19" = "Nuevo León",
  "20" = "Oaxaca",
  "21" = "Puebla",
  "22" = "Querétaro",
  "23" = "Quintana Roo",
  "24" = "San Luis Potosí",
  "25" = "Sinaloa",
  "26" = "Sonora",
  "27" = "Tabasco",
  "28" = "Tamaulipas",
  "29" = "Tlaxcala",
  "30" = "Veracruz",
  "31" = "Yucatán",
  "32" = "Zacatecas",
  "99" = "No especificado"
)

# --------------------------------------------------
# 2. Homicidios por entidad de ocurrencia (absolutos)
# --------------------------------------------------
homicidios_entidad_abs <- table(homicidios$ENT_OCURR)

homicidios_entidad_df <- data.frame(
  entidad_clave = names(homicidios_entidad_abs),
  homicidios = as.numeric(homicidios_entidad_abs),
  entidad = catalogo_entidades[names(homicidios_entidad_abs)]
)

# Ordenar de mayor a menor
homicidios_entidad_df <- homicidios_entidad_df[
  order(-homicidios_entidad_df$homicidios),
]

homicidios_entidad_df


# --------------------------------------------------
# 3. Top 10 entidades con más homicidios
# --------------------------------------------------
entidad_top10 <- homicidios_entidad_df[1:10, ]
entidad_top10

# Porcentaje que representan sobre el total
entidad_top10$porcentaje <- entidad_top10$homicidios / sum(homicidios$ENT_OCURR != "") * 100
entidad_top10


# --------------------------------------------------
# 4. Cruce homicidios por entidad × sexo
# --------------------------------------------------
entidad_sexo_abs <- table(homicidios$ENT_OCURR, homicidios$SEXO)

entidad_sexo_df <- as.data.frame(entidad_sexo_abs)
colnames(entidad_sexo_df) <- c("entidad_clave", "sexo", "homicidios")

# Etiquetas de sexo
entidad_sexo_df$sexo <- factor(
  entidad_sexo_df$sexo,
  levels = c(1, 2, 9),
  labels = c("Hombres", "Mujeres", "No especificado")
)

# Agregar nombre de la entidad
entidad_sexo_df$entidad <- catalogo_entidades[entidad_sexo_df$entidad_clave]

# Ordenar
entidad_sexo_df <- entidad_sexo_df[
  order(entidad_sexo_df$entidad, entidad_sexo_df$sexo),
]

entidad_sexo_df


# --------------------------------------------------
# 5. Resumen final (objetos clave)
# --------------------------------------------------
list(
  homicidios_por_entidad = homicidios_entidad_df,
  top10_entidades = entidad_top10,
  homicidios_entidad_sexo = entidad_sexo_df
)


# Abrir ventana gráfica (Mac)
quartz()

# Top 10 entidades (ya ordenadas)
top10 <- homicidios_entidad_df[1:10, ]

# Ajustar márgenes
par(mar = c(10, 5, 4, 2))

# Gráfica de barras
barplot(
  top10$homicidios,
  names.arg = top10$entidad,
  las = 2,
  col = "gray",
  main = "Homicidios por entidad federativa (Top 10, México 2024)",
  ylab = "Número de homicidios"
)

# Filtrar solo Top 10 entidades
entidades_top10 <- top10$entidad

datos_sexo_top10 <- entidad_sexo_df[
  entidad_sexo_df$entidad %in% entidades_top10 &
    entidad_sexo_df$sexo %in% c("Hombres", "Mujeres"),
]

# Reordenar entidades
datos_sexo_top10$entidad <- factor(
  datos_sexo_top10$entidad,
  levels = entidades_top10
)



# Abrir ventana gráfica (Mac)
quartz()

# Ajustar márgenes
par(mar = c(10, 5, 4, 2))

# Matriz entidad × sexo (Top 10)
matriz_sexo <- tapply(
  datos_sexo_top10$homicidios,
  list(datos_sexo_top10$sexo, datos_sexo_top10$entidad),
  sum
)

# Gráfica de barras agrupadas con colores diferenciados
barplot(
  matriz_sexo,
  beside = TRUE,
  las = 2,
  col = c("gray30", "gray80", "steelblue"),
  legend.text = rownames(matriz_sexo),
  args.legend = list(
    x = "topright",
    bty = "n"
  ),
  main = "Homicidios por entidad y sexo (Top 10, México 2024)",
  ylab = "Número de homicidios"
)

