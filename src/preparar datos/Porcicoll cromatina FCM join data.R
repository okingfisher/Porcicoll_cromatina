# Convertir salida de Weasel en datos ordenados

juntar_datos <- 1

directorio_FCM <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos/FCM"

ruta_programas_R <- "preparar datos"
ruta_datos <- "./"
prefijo <- "Porcicoll"
experimento <- "cromatina"

cuadrantes <- c("ul", "ur", "ll", "lr")
tratamientos <- list()

library(gdata)
library(xlsx)
options(stringsAsFactors = FALSE)

setwd(directorio_FCM)

source(paste("../../programas R/", prefijo, " ", experimento, " funciones y opciones.R", sep = ""))

thesheets <- c("hympp", "hcmm")

if (juntar_datos) {
	hypmcpipna <- data.frame()
	hcfdamsoxmt <- data.frame()
	for (archivo in list.files(pattern = "\\.xls")) {
		wb <- loadWorkbook(archivo)
		sheets <- names(getSheets(wb)) # nombres de las hojas. En general, el código del macho con un código para cada tinción.
		sheets <- gsub(" ", "-", sheets)
		for (hojai in 1:length(sheets))	{
			hoja <- read.xlsx2(archivo, sheetIndex = hojai, colIndex = 1:31)
			hoja <- hoja[ , !apply(hoja, 2, function(x) all(x == ""))]
			names(hoja) <- paste("X", 1:ncol(hoja), sep = ".")
			hoja$X.3[hoja$X.3 != "" & !grepl("CELLS", hoja$X.3)] <- paste(hoja$X.3[hoja$X.3 != "" & !grepl("CELLS", hoja$X.3)], " s", substr(archivo, nchar(archivo) - 5, nchar(archivo) - 5), sep = "")
			if (grepl("hympp", sheets[hojai])) {
				hypmcpipna <- rbind(hypmcpipna, hoja)
			} else if (grepl("hcmm", sheets[hojai])) {
				hcfdamsoxmt <- rbind(hcfdamsoxmt, hoja)
			}
		}
	}

	hypmcpipna <- within(hypmcpipna, {
		X.1 <- sub("_(\\d\\.fcs)", "_0\\1", X.1)
		X.1 <- sub("_(\\d\\d\\.fcs)", "_0\\1", X.1)
		X.3 <- sub("D.a ?", "d", X.3, ignore.case = TRUE)
		X.3 <- sub(" {2,}", " ", X.3)
		X.3 <- sub("C(\\d)", "C \\1", X.3)
		X.3 <- sub("SCL", "SLC", X.3)
		X.3 <- sub("SLC ", "SLC-", X.3)
	})	

	hcfdamsoxmt <- within(hcfdamsoxmt, {
		X.1 <- sub("_(\\d\\.fcs)", "_0\\1", X.1)
		X.1 <- sub("_(\\d\\d\\.fcs)", "_0\\1", X.1)
		X.3 <- sub("D.a ?", "d", X.3, ignore.case = TRUE)
		X.3 <- sub(" {2,}", " ", X.3)
		X.3 <- sub("C(\\d)", "C \\1", X.3)
		X.3 <- sub("SCL", "SLC", X.3)
	})
	write.csv2(hypmcpipna, "datos unidos hypmcpipna.csv", row.names = FALSE)
	write.csv2(hcfdamsoxmt, "datos unidos hcfdamsoxmt.csv", row.names = FALSE)
	datos2o <- hypmcpipna
	datos3o <- hcfdamsoxmt
} else {
	datos2o <- read.csv2("datos unidos hypmcpipna.csv")
	datos3o <- read.csv2("datos unidos hcfdamsoxmt.csv")
}

#########################
# Procesamos hypmcpipna	#
#########################

datos2o <- as.data.frame(apply(datos2o, 1:2, trim))

datos2x <- subset(datos2o, X.4 != "YO-PRO-" & grepl("R0", X.2, ignore.case = TRUE))
datos2x <- datos2x[ , apply(datos2x, 2, function(x) !all(x == ""))]
datos2x <- datos2x[ , c(1, 3, 5:7, 10, 12, 14, 16)]
datos2x[ , 6:9] <- apply(datos2x[ , 6:9], 1:2, as.numeric)
datos2pi <- subset(datos2x, grepl("PI", X.6, ignore.case = TRUE))[ , -c(3:5)]
datos2mc <- subset(datos2x, grepl("M540", X.5, ignore.case = TRUE))[ , -c(3:5)]
datos2pna <- subset(datos2x, grepl("PNA", X.7, ignore.case = TRUE))[ , -c(3:5)]
names(datos2pi) <- c("file", "sampleid", paste("yppi", cuadrantes, sep = ""))
names(datos2mc) <- c("file", "sampleid", paste("ypmc", cuadrantes, sep = ""))
names(datos2pna) <- c("file", "sampleid", paste("yppna", cuadrantes, sep = ""))
hymmfull <- merge(datos2pi, datos2mc, by = c("file", "sampleid"), all = TRUE)
hymmfull <- merge(hymmfull, datos2pna, by = c("file", "sampleid"), all = TRUE)

write.csv2(hymmfull, "datos procesados hypmcpipna.csv", row.names = FALSE)

##########################
# Procesamos hcfdamsoxmt #
##########################

datos3o <- as.data.frame(apply(datos3o, 1:2, trim))

datos3h <- subset(datos3o, grepl("R0ANDR2", X.2, ignore.case = TRUE) & grepl("CFDA", X.4, ignore.case = TRUE))
datos3h <- datos3h[ , apply(datos3h, 2, function(x) !all(x == ""))]
datos3h <- datos3h[ , c(1, 3, 5, 8:11)]
datos3h[ , 4:7] <- apply(datos3h[ , 4:7], 1:2, as.numeric)
names(datos3h) <- c("file", "sampleid", "voltaje", "cfmediana", "cfmedia", "cfmoda", "cfcv")
datos3c <- subset(datos3o, grepl("R0ANDR2ANDNOTR3", X.2, ignore.case = TRUE) & !grepl("CFDA", X.4, ignore.case = TRUE))
datos3c <- datos3c[ , apply(datos3c, 2, function(x) !all(x == ""))]
datos3c <- datos3c[ , c(1, 3, 5:7, 10, 12, 14, 16)]
datos3c[ , 6:9] <- apply(datos3c[ , 6:9], 1:2, as.numeric)
datos3cf <- subset(datos3c, grepl("CFDA", X.5, ignore.case = TRUE))[ , -c(3:5)]
datos3mx <- subset(datos3c, grepl("MitoSOX", X.6, ignore.case = TRUE))[ , -c(3:5)]
datos3mt <- subset(datos3c, grepl("Mtdr", X.7, ignore.case = TRUE))[ , -c(3:5)]

names(datos3cf) <- c("file", "sampleid", paste("h8cf", cuadrantes, sep = ""))
names(datos3mx) <- c("file", "sampleid", paste("h8mx", cuadrantes, sep = ""))
names(datos3mt) <- c("file", "sampleid", paste("h8mt", cuadrantes, sep = ""))

hcmmfull <- merge(datos3h, datos3cf, by = c("file", "sampleid"))
hcmmfull <- merge(hcmmfull, datos3mx, by = c("file", "sampleid"))
hcmmfull <- merge(hcmmfull, datos3mt, by = c("file", "sampleid"))

write.csv2(hcmmfull, "datos procesados hcfdamsoxmt.csv", row.names = FALSE)

generate.ids <- function(datos, idcol) {
	columnas <- names(datos)
	theids <- strsplit(datos[ , idcol], " ")
	datos$verraco <- sapply(theids, "[[", 3)
	datos$trat <- sapply(theids, "[[", 2)
	datos$semana <- sub("s", "", sapply(theids, "[[", 5))
	datos$día <- sub("d", "", sapply(theids, "[[", 4))
	datos <- datos[ , c("verraco", "trat", "semana", "día", columnas)]
	return(datos)
}

hymm <- generate.ids(hymmfull, "sampleid")
names(hymm)[names(hymm) == "file"] <- "file_hymm"
names(hymm)[names(hymm) == "sampleid"] <- "sampleid_hymm"
hcmm <- generate.ids(hcmmfull, "sampleid")
names(hcmm)[names(hcmm) == "file"] <- "file_hcmm"
names(hcmm)[names(hcmm) == "sampleid"] <- "sampleid_hcmm"

datos_fcm <- merge(hymm, hcmm, by = c("verraco", "trat", "semana", "día"), all = TRUE)

save(datos_fcm, file = paste(prefijo, " ", experimento, " FCM.Rdata", sep = ""))
write.csv2(datos_fcm, file = paste(prefijo, " ", experimento, " FCM.csv", sep = ""), row.names = FALSE, na = "")
