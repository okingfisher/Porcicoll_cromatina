directorio_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/"
directorio_datos <- "datos/datos procesados"
directorio_trabajo <- paste(directorio_base, directorio_datos, sep = "/")
ruta_programas_R <- "../../programas R/preparar datos"
ruta_datos <- "./"
proyecto <- "Porcicoll"
experimento <- "cromatina"

factores <- c("verraco", "trat", "día")

setwd(directorio_trabajo)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

datos_scsa <- arreglar.datos(paste(proyecto, " ", experimento, " SCSA procesados.Rdata", sep = ""))
#datos_scsa <- subset(datos_scsa, !grepl("OMITIR", comentarios))
datos_scsa$repl <- 1
variables_scsa <- c("sddfi", "tdfi", "hds")

datos_mbbr <- arreglar.datos(paste(proyecto, " ", experimento, " mBBr procesados.Rdata", sep = ""))
datos_mbbr$repl <- 1
variables_mbbr <- c("disulphide_mean", "disulphide_median","disulphide_mode", "lowmbbr.p.F", "medmbbr.p.F", "highmbbr.p.F")

datos_cma3 <- arreglar.datos(paste(proyecto, " ", experimento, " cma3 procesados.Rdata", sep = ""))
variables_cma3 <- c("lowcma3.p", "medcma3.p", "highcma3.p", "meancma3", "mediancma3", "picocma3")

datos <- NULL
datos <- merge(datos_scsa, datos_mbbr, by = factores, all = TRUE, suffixes = c("_scsa","_mbbr"))
datos <- merge(datos, datos_cma3, by = factores, all = TRUE, suffixes = c("_mbbr","_cma3"))

idents <- read.csv2("../Porcicoll cromatina identificaciones verracos.csv", stringsAsFactors = FALSE)
idents <- idents[ , -4]
names(datos)[1] <- "idFCM"
datos <- merge(datos, idents, by = "idFCM")

factores <- c("verraco", "semana", "trat", "día", "idFCM")

datos <- datos[ , c(factores, variables_scsa, variables_mbbr, variables_cma3)]
names(datos) <- gsub("\\.", "_", names(datos))

datos <- within(datos, {
	día <- as.integer(día)
	idFCM <- as.integer(idFCM)
})

datos <- datos[order(datos$idFCM, datos$semana, datos$día, datos$trat), ]

archivo <- paste(proyecto, " ", experimento, " sólo cromatina", sep = "")
save2(datos, filename = archivo)
