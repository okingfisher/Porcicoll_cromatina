# Crear variables y separar datos no ionóforo/LPC a otro archivo

juntar_datos <- 1

directorio_FCM <- "~/Documents/Trabajo/Proyectos/MEMESEM AGL2013-43328P Melatonina/experimentos/melatonina concentraciones/datos/FCM conc mel"

ruta_programas_R <- "preparar datos"
ruta_datos <- "./"
prefijo <- "MEMESEM"
experimento <- "conc mel"

library(gdata)
library(xlsx)
options(stringsAsFactors = FALSE)

setwd(directorio_FCM)

source(paste("../../programas R/", prefijo, " ", experimento, " funciones y opciones.R", sep = ""))

load("MEMESEM conc mel FCM.RData")
datos <- datos_fcm

datos <- within(datos,
{
	macho <- trim(macho)
	macho <- ifelse(nchar(macho) == 1, paste("0", macho, sep = ""), macho)
	challenge <- ifelse(lpc, "lpc", ifelse(ion, "ion", "std"))
	mxposr <- round(ypmxlr / (ypmxll + ypmxlr) * 100, 2)
	mxpost <- ypmxlr + ypmxur
	mxpos <- ypmxlr
	cfr <- cfpiur + cfpilr
	mtposr <- round(ypmtlr / (ypmtll + ypmtlr) * 100, 2)
	mtpos <- ypmtlr
	mcposr <- round(mcpilr / (mcpill + mcpilr) * 100, 2)
	mcpos <- mcpilr
	acrd <- pipnaur + pipnalr
	acrr <- round(pipnalr / (pipnall + pipnalr) * 100, 2)
	viabyp <- ypmxll + ypmxlr
	viabpi <- pipnall + pipnalr
})

datos <- datos[ , c(1:8, 64, 11:17, 38:43, 52:63)]

datoss <- subset(datos, !(ion | lpc), select = -c(challenge, lpc, ion))

datosc <- datos[ , c(1, 2, 4, 5, 6, 9:16, 23, 25:28)]

variablesc <- names(datosc)[7:18]

datoscw <- reshape(datosc, v.names = variablesc, idvar = c("especie", "macho", "trat", "dosis", "hep"), timevar = "challenge", direction = "wide")

for (variable in variablesc[c(1, 2, 5:7, 8:12)])
{
	for (tipo in c("ion", "lpc"))
	{
		datoscw[ , paste(variable, ".", tipo, "r", sep = "")] <- round(datoscw[ , paste(variable, tipo, sep = ".")] / datoscw[ , paste(variable, "std", sep = ".")] * 100, 2)
		datoscw[ , paste(variable, ".", tipo, "d", sep = "")] <- datoscw[ , paste(variable, tipo, sep = ".")] - datoscw[ , paste(variable, "std", sep = ".")]
	}	
}

datoscw <- subset(datoscw, select = !grepl("\\.std", names(datoscw)))

datosfinal <- merge(datoss, datoscw, by = c("especie", "macho", "trat", "dosis", "hep"), all = TRUE)

save2(datosfinal, file = paste("../datos procesados/", prefijo, " ", experimento, " FCM final", sep = ""))
