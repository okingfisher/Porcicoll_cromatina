proyecto <- "Porcicoll"
experimento <- "cromatina"
ruta_general <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos/mBBr"
setwd(ruta_general)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

directorios <- list.files(pattern = "2019.* mBBr")
directorios <- directorios[file.info(directorios)$isdir]

datos_mbbr <- data.frame()
for(dir_trabajo in directorios) {
	dir_analisis <- paste(ruta_general, dir_trabajo, sep = "/")
	setwd(dir_analisis)	
	file_mbbr <- list.files(pattern = "summary.csv")
	losdatos <- read.csv2(file_mbbr, stringsAsFactors = FALSE)
	datos_mbbr <- rbind(datos_mbbr, losdatos)
}

datos_mbbr <- within(datos_mbbr, {
	muestra <- toupper(muestra)
	muestra <- gsub("\\+", " ", muestra)
	muestra <- gsub("-", " ", muestra)
	muestra <- sub("SCL", "SLC", muestra)
	muestra <- sub("SLC ", "SLC", muestra)
	muestra <- sub("DIA ", "D", muestra)
	muestra <- gsub(" {2,}", " ", muestra)
	tipo <- ifelse(grepl(" DTT", muestra), "T", "F")
	muestra <- sub(" DTT", "", muestra)
})

write.csv2(datos_mbbr, paste("../../datos procesados/", proyecto, " ", experimento, " mBBr.csv", sep = ""), row.names = FALSE)

datos <- subset(datos_mbbr, !grepl("STD", muestra), select = -c(cit, repl, well, observaciones))
datos <- subset(datos, !(archivo %in% c("RB2019-05-03.0004.fcs", "RB2019-05-03.0006.fcs", "RB2019-05-15.0009.fcs", "RB2019-05-15.0010.fcs")))

datos <- reshape(datos, idvar = "muestra", timevar = "tipo", direction = "wide")

datos <- within(datos, {
	verraco <- sapply(strsplit(muestra, " "), "[[", 1)
	trat <- ifelse(grepl("C ", muestra), "CTL", ifelse(grepl("SLC.?4 ", muestra), "SLC4", ifelse(grepl("SLC.?1 ", muestra), "SLC1", "")))
	día <- ifelse(grepl("D0", muestra), 0, ifelse(grepl("D3", muestra), 3, NA))
	dtt <- ifelse(grepl("DTT", muestra), 1, 0)
})

datos <- within(datos, {
#Zubkova, E.V., Wade, M., and Robaire, B. (2005). Changes in spermatozoal chromatin packaging and susceptibility to oxidative challenge during aging. Fertil. Steril. 84 Suppl 2, 1191–1198.
#En 2016 probé a pasarlos a canales pero es peor.

	disulphide_mean <- (meanmbbr.T - meanmbbr.F) / 2 * 100
	disulphide_median <- (medianmbbr.T - medianmbbr.F) / 2 * 100
	disulphide_mode <- (picombbr.T - picombbr.F) / 2 * 100
	
	disulphide_meanA <- (meanmbbrA.T - meanmbbrA.F) / 2 * 100
	disulphide_medianA <- (medianmbbrA.T - medianmbbrA.F) / 2 * 100
	disulphide_modeA <- (picombbrA.T - picombbrA.F) / 2 * 100
})

datos <- datos[ , !(names(datos) %in% c("repl.F", "repl.T"))]

save2(datos, paste("../../datos procesados/", proyecto, " ", experimento, " mBBr procesados", sep = ""))