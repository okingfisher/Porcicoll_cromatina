proyecto <- "Porcicoll"
experimento <- "cromatina"

ruta <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento"

directorio_CASA <- paste(ruta, "/datos/datos procesados", sep = "")

variables <- c("VCL", "VSL", "VAP", "LIN", "STR", "WOB", "ALH", "BCF", "DNC", "DNCm")

setwd(directorio_CASA)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

load(file = paste(proyecto, " ", experimento, " CASA rawdata.Rdata", sep = ""))
datos <- subset(datos, !grepl("leche", ident))



cat("\nSalvando... ")
save2(datos, file = paste(proyecto, " ", experimento, " CASA procesados", sep = ""))
cat("Hecho\n")
