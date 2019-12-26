corregir <- 1
revisión <- 0
proyecto <- "Porcicoll"
experimento <- "cromatina"
library(gdata)
ruta_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/"
directorio <- "datos/SCSA Porcicoll"
ruta_general <- paste(ruta_base, "/", directorio, "/", sep = "")

setwd(ruta_general)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

directorios <- list.dirs(recursive = FALSE)
directorios <- directorios[!grepl("informes|programas", directorios, ignore.case = TRUE)]

# Leer todos los datos

datos <- data.frame()
for (directorio in directorios) {
	archivo <- list.files(directorio, pattern = "summary SCSA\\.csv")
	datos <- rbind(datos, read.csv2(paste(directorio, "/", archivo, sep = ""), stringsAsFactors = FALSE))
}

datos <- within(datos, {

})

datos$id[grepl("noID", datos$id)] <- "STD"
datos$id[grepl("STD", datos$id)] <- paste("DS", datos$id[grepl("STD", datos$id)], "S", sep = "-")
ids <- datos$id
ids <- gsub(" ", "-", ids)
ids <- gsub("SLC-", "SLC", ids)
ids <- gsub("SCL", "SLC", ids)
ids <- gsub("DIA", "D", ids)
ids <- strsplit(ids, " |-")
datos$verraco <- sapply(ids, "[[", 3)
datos$día <- sapply(ids, "[[", 1)
datos$trat <- sapply(ids, "[[", 2)

datos$verraco[substr(datos$id, 1, 1) != "D" & !grepl("STD", datos$id)] <- sapply(ids[substr(datos$id, 1, 1) != "D" & !grepl("STD", datos$id)], "[[", 1)
datos$día[substr(datos$id, 1, 1) != "D" & !grepl("STD", datos$id)] <- sapply(ids[substr(datos$id, 1, 1) != "D" & !grepl("STD", datos$id)], "[[", 3)
datos$trat[datos$trat == "C"] <- "CTL"

datos$comentarios <- ""

if (corregir & file.exists("Porcicoll SCSA para omitir.csv")) {
	listaomitir <- read.csv2("Porcicoll SCSA para omitir.csv", stringsAsFactors = FALSE)
	paraomitir <- paste(listaomitir$directorio, listaomitir$archivo)
	datos$comentarios[paste(datos$directorio, datos$archivo) %in% paraomitir] <- "OMITIR"
	datos$comentarios <- trim(datos$comentarios)
}
datos$cia <- "AIM León"

archivo <- paste(proyecto, " ", experimento, " SCSA", sep = "")
save2(datos, paste(ruta_base, "/datos/datos procesados/", archivo, sep = ""))

source(paste(ruta_base, "/programas R/preparar datos/programas_R SCSA porcino/2 AIM procesar Porcicoll.R", sep = ""))