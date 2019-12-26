# Combinar datos 1

juntar_datos <- 1

directorio_datos <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos"

ruta_programas_R <- "preparar datos"
ruta_datos <- "./"
prefijo <- "Porcicoll"
experimento <- "cromatina"

idents <- c("día", "trat", "macho", "semana")

library(xlsx)
options(stringsAsFactors = FALSE)

setwd(directorio_datos)

source(paste("../programas R/", prefijo, " ", experimento, " funciones y opciones.R", sep = ""))
load("datos procesados/Porcicoll cromatina CASA medians.Rdata")
datoscasa <- datos
datos <- data.frame()
identificaciones <- read.xlsx2("Porcicoll_Movilidad_Concentraciones_PesajeTubos_Morfo.xlsx", sheetName = "ID Machos")
names(identificaciones) <- c("semana", "verraco", "idfcm", "idmicro")
recuperaciones <- read.xlsx2("Porcicoll_Movilidad_Concentraciones_PesajeTubos_Morfo.xlsx", sheetName = "Concentraciones (Bürker)")
recuperaciones$trat[recuperaciones$trat == "Control"] <- "C"
recuperaciones$idmicro <- recuperaciones$macho
morfología <- read.xlsx2("Porcicoll_Movilidad_Concentraciones_PesajeTubos_Morfo.xlsx", sheetName = "Morfología")
morfología$trat[morfología$trat == "Control"] <- "C"
morfología$idmicro <- morfología$macho
datos <- merge(identificaciones, recuperaciones, by = c("idmicro", "semana"), all = TRUE)
datos <- merge(datos, morfología, by = c("idmicro", "trat", "semana", "día"), all = TRUE)
names(datoscasa)[2:4] <- c("macho", "semana", "día")
datoscasa$trat <- "C"
stop()
datoscasa <- within(datoscasa, {
	ident <- sub("SCL", "SLC", ident)
	trat <- ifelse(grepl("SLC-1", ident), "SLC-1", ifelse(grepl("SLC-4", ident), "SLC-4", ifelse(grepl("C_", ident), trat <- "C", "")))
	verracoid <- paste(macho, semana, sep = "-")
})
datos$verracoid <- paste(macho, semana, )
datos <- merge(datos, datoscasa, by = idents, all = TRUE)

save2(datos, file = paste("datos procesados/", prefijo, " ", experimento, " datos combinados", sep = ""))