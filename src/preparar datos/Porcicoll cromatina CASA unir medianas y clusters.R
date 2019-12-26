
directorio_CASA <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos/"

ruta_programas_R <- "programas R"
ruta_datos <- "./datos procesados"
proyecto <- "Porcicoll"
experimento <- "cromatina"

directorios <- NULL
datos <- NULL

files <- "./"

setwd(directorio_CASA)
source(paste("../", ruta_programas_R, "/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

nombre <- load(paste(ruta_datos, "/", proyecto, " ", experimento, " CASA medians.Rdata", sep = ""))
datos <- subset(datos, !grepl("leche", ident))
idents <- names(datos)[1:5]
datos <- subset(datos, select = -c(fps, totalspz, totalmot, totalprog))
datos_medianas <- datos

nombre <- load(paste(ruta_datos, "/", proyecto, " ", experimento, "-clusters for analysis-final-agnes-agnes.Rdata", sep = ""))
datos_cl <- subset(datos, !grepl("leche", ident))
datos_cl <- datos_cl[ , c(idents, "cluster", "prop", "prop_mt")]
datos_cl <- reshape(datos_cl, v.names = c("prop", "prop_mt"), idvar = idents, timevar = "cluster", direction = "wide")
datos_cl[ , grepl("prop", names(datos_cl))] <- apply(datos_cl[  , grepl("prop", names(datos_cl))], 2, function(x) ifelse(is.na(x), 0, x))

datos <- merge(datos_medianas, datos_cl, by = idents, all = TRUE)
datos$observaciones <- ""


cat("\nSalvando... ")
save2(datos, file = paste(ruta_datos, "/", proyecto, " ", experimento, " CASA medianas y clusters", sep = ""))
