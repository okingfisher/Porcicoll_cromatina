ruta_general <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto análisis SCSA/análisis 2018"
setwd(ruta_general)
directorios <- list.files(pattern = "Le.+n")
directorios <- directorios[file.info(directorios)$isdir]

datos_scsa <- data.frame()
for(dir_trabajo in directorios)
{
	dir_analisis <- paste(ruta_general, dir_trabajo, sep = "/")
	setwd(dir_analisis)
	dir_informes <- list.files(pattern = "informes")
	dir_datos <- paste(ruta_general, dir_trabajo, dir_informes, sep = "/")
	setwd(dir_datos)
	
	file_scsa <- list.files(pattern = "identificados.Rdata")
	losdatos <- load(file_scsa)
	datos <- datos[ , names(datos) != "fecha salida"]
	datos <- within(datos,
	{
		`fecha muestreo` <- as.Date(`fecha muestreo`)
		`fecha análisis` <- as.Date(`fecha análisis`)
	})
#	print(dir_trabajo)
#	print(str(datos))
#	readline("#")
	datos_scsa <- rbind(datos_scsa, datos)
}

library(xlsx)
write.xlsx2(datos_scsa, file = "../../Cromatina León enero 2018 a febrero 2018.xlsx", row.names = FALSE)