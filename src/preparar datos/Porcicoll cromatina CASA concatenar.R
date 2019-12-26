#recalcular DNC y DNCm

reconcatenar <- 1

directorio_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento"

directorio_CASA <- paste(directorio_base, "/datos/CASA", sep = "")

proyecto <- "Porcicoll"
experimento <- "cromatina"

directorios <- NULL
datos <- NULL

files <- "./"

identnames <- c("ident", "verraco", "muestreo", "día", "trat")
identcasa <- c("Campo", "Nesp", "first", "last", "Npuntos")
variables <- c("VCL", "VSL", "VAP", "LIN", "STR", "WOB", "ALH", "BCF", "DNC", "DNCm")

make.idents <- function(ident, directorio, rawdata) {
	idents <- strsplit(ident, " ")[[1]]
	identsdir <- strsplit(directorio, " ")[[1]]
	cat(idents, "\n", sep = "\t\t")
	rawdata$ident <- paste(idents, collapse  = "_")
	rawdata$verraco <- idents[1]
	rawdata$trat <- idents[2]
#	ifelse(identsdir[2] == "primer", rawdata$muestreo <- 1, ifelse(identsdir[2] == "segundo", rawdata$muestreo <- 2, ifelse(identsdir[2] == "tercer", rawdata$muestreo <- 3, rawdata$muestreo <- 4)))
	rawdata$muestreo <- as.integer(identsdir[3])
	rawdata$día <- as.integer(identsdir[5])
#	rawdata$día <- as.integer(idents[4])
	return(rawdata)
}

library(xlsx)
options(java.parameters = "-Xmx1000m")
#library(gdata)

setwd(directorio_CASA)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

datos <- data.frame()
directorios <- dir(pattern = "muestreo")
for (directorio in directorios) {
	listado <- list.files(directorio, pattern = "\\.xls?")
	dirlistado <- paste(directorio, "/", listado, sep = "")
	nfiles <- length(dirlistado)
	if(nfiles == 0) next()
	
	size_total <- sum(file.info(dirlistado)$size - 12000) #Un archivo sin líneas tiene 12 kB
	#Unas 3 líneas por kB
	lines_total <- round((size_total / 1000) * 3.5)
	long_ident <- length(c(identnames, identcasa))
	long_mov <- length(variables)
	long_total <- long_ident + long_mov - 2 #por DNC y DNCm		
	tipo_columnas <- c(rep("character", length(identnames)), rep("integer", length(identcasa)), rep("numeric", long_mov - 2)) #no consideramos DNC ni DNCm
	vectores <- function(modo, longitud) vector(mode = modo, length = longitud)	
	lista_vectores <- lapply(tipo_columnas, vectores, longitud = lines_total)	
	names(lista_vectores) <- paste("V", 1:long_total, sep = "")		
	datosfile <- as.data.frame(lista_vectores, stringsAsFactors = FALSE)	
	names(datosfile) <- c(identnames, identcasa, variables[variables != c("DNC", "DNCm")])
	fila <- 1
	orden <- 1	
	for(archivo in listado) {
		cat("Añadiendo archivo ", archivo, ", ", orden, " de ", nfiles, sep = "")
		rawdata <- read.xlsx2(paste(directorio, "/", archivo, sep = ""), sheetIndex = 1, startRow = 10, stringsAsFactors = FALSE)
#		tmpfil <- xls2csv(archivo)
#		rawdata <- read.csv(summary(tmpfil)$description, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE, skip = 1)
#		file.remove(summary(tmpfil)$description)
	#	rawdata <- read.csv2(archivo, skip = 10, stringsAsFactors = FALSE, header = FALSE)
		rawdata <- rawdata[ , -c(6, 15:22)]
		names(rawdata) <- c(identcasa, variables[variables != c("DNC", "DNCm")]) #sin DNC ni DNCm
		nfilas <- nrow(rawdata)
		cat(", con ", nfilas, " filas... ", sep = "")
		cat("\n")
		ident <- sub("\\.xls?", "", archivo)
		ident <- sub("SCL", "SLC", ident)
		
		
		rawdata <- make.idents(ident, directorio, rawdata)
		if (unique(rawdata$ident) == "") stop()	
	#	si se importa desde Excel	
	#	rawdata[ , c("LIN", "STR", "WOB")] <- data.frame(apply(rawdata[ , c("LIN", "STR", "WOB")], c(1, 2), function(x) unlist(strsplit(x, "%"))[1]), stringsAsFactors = FALSE) 
		rawdata <- rawdata[ , c(identnames, identcasa, variables[variables != c("DNC", "DNCm")])]
		if (directorio %in% c("20190129 muestreo 4 día 0", "20190201 muestreo 4 día 3")) {
			# Equivalencias de MEMESEM concentraciones mel.
			rawdata <- within(rawdata, {
				VCL <- as.numeric(VCL) * 1.592908
				VAP <- as.numeric(VAP) * 1.188908
				BCF <- as.numeric(BCF) * 3.035417
				Npuntos <- round(as.integer(Npuntos) / 25 * 53)
				LIN <- as.numeric(VSL) / VCL	
				STR <-  as.numeric(VSL) / VAP		
				WOB <- VAP / VCL
			})
		}
		
		filafin <- fila + nfilas - 1
		datosfile[fila:filafin, ] <- rawdata
		cat("hecho.\n")
		fila <- fila + nfilas
		orden <- orden + 1
	}
	datosfile <- subset(datosfile, ident != "")
	analizadosdir <- unique(datosfile$ident)
	if (nrow(datos) > 0) datos <- subset(datos, !(ident %in% analizadosdir)) # eliminamos los repetidos en informes movilidad porcicoll revisado, debe estar en última posición
	datos <- rbind(datos, datosfile)
}
datos <- subset(datos, ident != "" & !is.na(Campo))

datos[ , identcasa] <- apply(datos[ , identcasa], 1:2, as.integer)
datos[ , variables[variables != c("DNC", "DNCm")]] <- apply(datos[ , variables[variables != c("DNC", "DNCm")]], 1:2, as.numeric)

#datos[!is.na(datos$VCL) & (datos$VCL == 0 | datos$VAP == 0), variables[1:8]] <- NA

datos <- within(datos, {
	LIN <- LIN * 100
	STR <- STR * 100
	WOB <- WOB * 100
	DNC <- round(VCL * ALH, 1)
	DNCm <- round(ALH / (LIN / 100), 1) #ALH * VCL / VSL
	DNCm[is.infinite(DNCm)] <- NA
})

#datos <- subset(datos, is.na(VCL) | (VCL >= VAP & VAP >= VSL & Npuntos > max(Npuntos, na.rm = TRUE) * 0.3))

loscampos <- aggregate(Nesp ~ Campo + ident, datos, length)
lassd <- aggregate(Nesp ~ ident, loscampos, sd)
loscampos <- merge(loscampos, lassd, by = "ident", all = TRUE)
names(loscampos) <- c("ident", "Campo", "Nesp", "SD")
loscampos$SD <- round(loscampos$SD)
print(loscampos[loscampos$SD > 50 & !is.na(loscampos$SD), ])

cat("\nSalvando... ")
save2(datos, file = paste("../datos procesados/", proyecto, " ", experimento, " CASA rawdata", sep = ""))
cat("Hecho\n")

source("../../programas R/preparar datos/Porcicoll cromatina CASA procesar concatenados.R")
