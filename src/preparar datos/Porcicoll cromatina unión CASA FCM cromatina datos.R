directorio_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/"
directorio_datos <- "datos/datos procesados"
directorio_trabajo <- paste(directorio_base, directorio_datos, sep = "/")
ruta_programas_R <- "../../programas R/preparar datos"
ruta_datos <- "./"
proyecto <- "Porcicoll"
experimento <- "cromatina"

variables_casa <- c("mot", "prog", "vcl", "vsl", "vap", "lin", "str", "wob", "alh", "bcf", "dnc", "dncm")
variables_fcm <- c("viabpi", "viabyp", "acrr", "acrd", "mcpos", "mcposr", "mtpos", "mtposr", "cfmedia", "cfmediana", "mxpos", "mxpost", "mxposr")


factores <- c("verraco", "semana", "trat", "día")

setwd(directorio_trabajo)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

datos_fcm <- arreglar.datos(paste(proyecto, " ", experimento, " FCM.Rdata", sep = ""))

datos_fcm <- within(datos_fcm, {
	mxposr <- round(h8mxlr / (h8mxll + h8mxlr) * 100, 2)
	mxpost <- h8mxlr + h8mxur
	mxpos <- h8mxlr
	mtposr <- round(h8mtlr / (h8mtll + h8mtlr) * 100, 2)
	mtpos <- h8mtlr
	mcposr <- round(ypmclr / (ypmcll + ypmclr) * 100, 2)
	mcpos <- ypmclr
	acrd <- yppnaur + yppnalr
	acrr <- round(yppnaur / (yppnaur + yppnalr) * 100, 2)
	viabyp <- yppill + yppilr
	viabpi <- yppill + yppilr
})

datos_casa <- arreglar.datos(paste(proyecto, " ", experimento, " CASA medianas y clusters.Rdata", sep = ""))
datos_casa$ident <- toupper(datos_casa$ident)
variables_cl <- names(datos_casa)[grepl("prop", names(datos_casa))]
if (any(apply(datos_casa[ , variables_cl], 1, function(x) all(is.na(x))))) stop("Muestras sin subpoblaciones")
datos_casa[, variables_cl] <- apply(datos_casa[, variables_cl], 1:2, function(x) ifelse(is.na(x), x <- 0, x))
stop()
datos_scsa <- arreglar.datos(paste(proyecto, " ", experimento, " datos SCSA identificados.Rdata", sep = ""))
datos_scsa <- subset(datos_scsa, !grepl("OMITIR", comentarios))
datos_scsa$repl <- 1
variables_scsa <- c("sddfi", "tdfi", "hds")

datos_mbbr <- arreglar.datos(paste(proyecto, " ", experimento, " datos mBBr arreglados.Rdata", sep = ""))
datos_mbbr$repl <- 1
variables_mbbr <- c("freethiols_mean", "freethiols_median","freethiols_mode", "freethiols_meanA", "freethiols_medianA", "freethiols_modeA","lowmbbr.p.C", "medmbbr.p.C", "highmbbr.p.C")

datos_cma3 <- arreglar.datos(paste(proyecto, " ", experimento, " datos CMA3 arreglados.Rdata", sep = ""))
variables_cma3 <- c("")

datos <- NULL
datos <- merge(datos_casa, datos_fcm, by = factores, all = TRUE, suffixes = c("_casa","_fcm"))
datos <- merge(datos, datos_scsa, by = factores, all = TRUE, suffixes = c("_fcm","_scsa"))
datos <- merge(datos, datos_mbbr, by = factores, all = TRUE, suffixes = c("_scsa","_mbbr"))
#datos <- merge(datos, datos_cma3, by = factores, all = TRUE, suffixes = c("_mbbr","_cma3"))

datos <- datos[ , c(factores, variables_casa, variables_cl, variables_fcm, variables_scsa, variables_mbbr)]

archivo <- paste(proyecto, " ", experimento, " full", sep = "")
save2(datos, filename = paste("../", archivo, sep = ""))
