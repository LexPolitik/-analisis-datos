############################################################
# Área de trabajo de Víctor Castellanos
# Análisis de datos del Registro Nacional de Personas
# Desaparecidas y No Localizadas (RNPDNO)
#
# Prueba técnica – Analista de Datos
#
# Descripción:
# Este script tiene como objetivo leer los archivos .json
# descargados desde la Versión Pública del RNPDNO utilizando
# el repositorio reqrnpdno, construir un conjunto de datos
# consolidado por entidad federativa, sexo y grupos de edad,
# y exportar los resultados a un archivo .csv para su análisis.
#
# Periodo de análisis:
# 1 de enero de 2024 – 1 de enero de 2025
#
# Fuente de datos:
# Registro Nacional de Personas Desaparecidas y No Localizadas
# (Versión Pública RNPDNO – Comisión Nacional de Búsqueda)
#
# Autor:
# Víctor Castellanos
#
# Fecha:
# Diciembre 2025
############################################################
############################################################
# 1. Limpieza del entorno y configuración inicial
############################################################

# Limpiar todos los objetos del Environment
rm(list = ls())

# Definir el directorio de trabajo del proyecto
setwd("~/Documents/reqrnpdno")

############################################################
# 2. Verificación de archivos del proyecto
############################################################

# Ver archivos en el directorio principal
list.files()

# Ver archivos JSON descargados por entidad federativa
list.files("salida/estados")

############################################################
# 3. Lectura de un archivo JSON de ejemplo
# (Aguascalientes: id_estado = 1)
############################################################

# Cargar librería para leer archivos JSON
library(jsonlite)

# Leer un archivo JSON de ejemplo
ejemplo <- fromJSON("salida/estados/1.json")

# Ver las secciones principales del archivo
names(ejemplo)

############################################################
# 4. Exploración de la estructura por edad
############################################################

# Ver categorías de sexo dentro de la sección por edad
names(ejemplo$por_edad)

# Ver algunas edades y conteos para hombres
head(ejemplo$por_edad$Hombres)

############################################################
# 5. Conversión de la información de edades a una tabla
############################################################

# Convertir la lista de edades de hombres en un data frame

as.integer(names(ejemplo$por_edad$Hombres))

names(ejemplo$por_edad$Hombres)

edades_hombres <- data.frame(
  edad  = as.integer(names(ejemplo$por_edad$Hombres)),
  total = as.integer(ejemplo$por_edad$Hombres)
)

edades_hombres <- edades_hombres[!is.na(edades_hombres$edad), ]

head(edades_hombres)


edades_hombres$grupo_edad <- cut(
  edades_hombres$edad,
  breaks = c(-Inf, 9, 19, 35, 59, Inf),
  labels = c("0-9", "10-19", "20-35", "36-59", "60+")
)


head(edades_hombres)


hombres_por_grupo <- aggregate(
  total ~ grupo_edad,
  data = edades_hombres,
  sum
)

hombres_por_grupo



# Convertir edades de Mujeres a data frame
edades_mujeres <- data.frame(
  edad  = as.integer(names(ejemplo$por_edad$Mujeres)),
  total = as.integer(ejemplo$por_edad$Mujeres)
)

# Quitar edades no numéricas
edades_mujeres <- edades_mujeres[!is.na(edades_mujeres$edad), ]

# Crear grupos de edad
edades_mujeres$grupo_edad <- cut(
  edades_mujeres$edad,
  breaks = c(-Inf, 9, 19, 35, 59, Inf),
  labels = c("0-9", "10-19", "20-35", "36-59", "60+")
)

# Sumar por grupo de edad
mujeres_por_grupo <- aggregate(
  total ~ grupo_edad,
  data = edades_mujeres,
  sum
)

mujeres_por_grupo


# Agregar columna de sexo
hombres_por_grupo$sexo <- "Hombres"
mujeres_por_grupo$sexo <- "Mujeres"

# Unir ambas tablas
tabla_estado <- rbind(hombres_por_grupo, mujeres_por_grupo)

tabla_estado


ejemplo$parametros
ejemplo$parametros$estado
tabla_estado$id_estado <- 1
tabla_estado


procesar_estado <- function(ruta_json, id_estado) {
  
  # Leer archivo JSON
  datos <- fromJSON(ruta_json)
  
  # ---------- HOMBRES ----------
  edades_hombres <- data.frame(
    edad  = as.integer(names(datos$por_edad$Hombres)),
    total = as.integer(datos$por_edad$Hombres)
  )
  edades_hombres <- edades_hombres[!is.na(edades_hombres$edad), ]
  
  edades_hombres$grupo_edad <- cut(
    edades_hombres$edad,
    breaks = c(-Inf, 9, 19, 35, 59, Inf),
    labels = c("0-9", "10-19", "20-35", "36-59", "60+")
  )
  
  hombres_por_grupo <- aggregate(
    total ~ grupo_edad,
    data = edades_hombres,
    sum
  )
  hombres_por_grupo$sexo <- "Hombres"
  
  # ---------- MUJERES ----------
  edades_mujeres <- data.frame(
    edad  = as.integer(names(datos$por_edad$Mujeres)),
    total = as.integer(datos$por_edad$Mujeres)
  )
  edades_mujeres <- edades_mujeres[!is.na(edades_mujeres$edad), ]
  
  edades_mujeres$grupo_edad <- cut(
    edades_mujeres$edad,
    breaks = c(-Inf, 9, 19, 35, 59, Inf),
    labels = c("0-9", "10-19", "20-35", "36-59", "60+")
  )
  
  mujeres_por_grupo <- aggregate(
    total ~ grupo_edad,
    data = edades_mujeres,
    sum
  )
  mujeres_por_grupo$sexo <- "Mujeres"
  
  # ---------- UNIR Y AGREGAR ID ----------
  tabla_estado <- rbind(hombres_por_grupo, mujeres_por_grupo)
  tabla_estado$id_estado <- id_estado
  
  return(tabla_estado)
}


prueba_estado1 <- procesar_estado("salida/estados/1.json", 1)
prueba_estado1



# Lista de todos los archivos JSON
archivos <- list.files("salida/estados", full.names = TRUE)

# Extraer el id_estado desde el nombre del archivo
ids <- as.integer(gsub(".json", "", basename(archivos)))

# Quedarnos solo con los estados 1 a 32
validos <- ids >= 1 & ids <= 32

archivos_estados <- archivos[validos]
ids_estados <- ids[validos]

length(archivos_estados)

# Procesar todos los estados y unirlos en una sola tabla
tabla_final <- do.call(
  rbind,
  Map(procesar_estado, archivos_estados, ids_estados)
)

# Procesar todos los estados y unirlos en una sola tabla
tabla_final <- do.call(
  rbind,
  Map(procesar_estado, archivos_estados, ids_estados)
)


head(tabla_final)

write.csv(tabla_final, "RNPDNO_2024_por_estado_sexo_grupo_edad.csv", row.names = FALSE)

# Crear columna combinada grupo de edad + sexo
tabla_final$edad_sexo <- paste(tabla_final$grupo_edad, tabla_final$sexo, sep = "_")

# Ver cómo quedó
head(tabla_final)

# Pasar a formato ancho: una fila por estado
tabla_ancha <- reshape(
  tabla_final[, c("id_estado", "edad_sexo", "total")],
  idvar = "id_estado",
  timevar = "edad_sexo",
  direction = "wide"
)

# Ver cómo quedó
head(tabla_ancha)

##Reestructurar tabla####
# 1) Quitar prefijo "total." de los nombres de columnas
names(tabla_ancha) <- gsub("^total\\.", "", names(tabla_ancha))

# 2) Limpiar nombres de fila
rownames(tabla_ancha) <- NULL

# 3) Reemplazar NA por 0
tabla_ancha[is.na(tabla_ancha)] <- 0

# ORDENAR estados del 1 al 32
tabla_ancha <- tabla_ancha[order(tabla_ancha$id_estado), ]

# Ver resultado
head(tabla_ancha)

##Nuevo DF###
write.csv(
  tabla_ancha,
  "RNPDNO_2024_por_estado_edad_sexo.csv",
  row.names = FALSE
)
