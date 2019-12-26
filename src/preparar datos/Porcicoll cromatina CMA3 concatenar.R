proyecto <- "Porcicoll"
experimento <- "cromatina"
ruta_general <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos/CMA3"
setwd(ruta_general)
directorios <- list.files(pattern = "2019.* CMA3")
directorios <- directorios[file.info(directorios)$isdir]

source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

datos_cma3 <- data.frame()
for(dir_trabajo in directorios) {
	dir_analisis <- paste(ruta_general, dir_trabajo, sep = "/")
	setwd(dir_analisis)	
	file_cma3 <- list.files(pattern = "summary.csv")
	losdatos <- read.csv2(file_cma3, stringsAsFactors = FALSE)
	datos_cma3 <- rbind(datos_cma3, losdatos)
}
datos_cma3 <- within(datos_cma3, {
	muestra <- toupper(muestra)
	muestra <- gsub("\\+", " ", muestra)
	muestra <- gsub("-", " ", muestra)
	muestra <- sub("SCL", "SLC", muestra)
	muestra <- sub("SLC ", "SLC", muestra)
	muestra <- sub("DIA ?", "D", muestra)
	muestra <- gsub(" {2,}", " ", muestra)
})

save2(datos_cma3, paste("../../datos procesados/", proyecto, " ", experimento, " cma3", sep = ""))

datos <- subset(datos_cma3, !grepl("STD", muestra), select = -c(cit, réplica, well, observaciones))
ids <- strsplit(datos$muestra, " |-")
datos$verraco <- sapply(ids, "[[", 1)
datos$día <- sub("D", "", sapply(ids, "[[", 3))
datos$trat <- sapply(ids, "[[", 2)
datos$trat[datos$trat == "C"] <- "CTL"

datos <- subset(datos, !(archivo %in% c("RB2019-06-05.0004.fcs", "RB2019-06-05.0005.fcs", "RB2019-06-05.0006.fcs", "RB2019-06-05.0007.fcs", "RB2019-06-05.0008.fcs", "RB2019-06-05.0009.fcs")))

save2(datos, paste("../../datos procesados/", proyecto, " ", experimento, " cma3 procesados", sep = ""))
