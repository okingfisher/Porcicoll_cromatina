proyecto <- "Porcicoll"
experimento <- "cromatina"
ruta_datos <- "./datos procesados"
directorio_CASA <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/datos/"
str_prog <- 60
vcl_prog <- 25
prop_tiempo_en_campo <- 0.3
nspz <- 20
fps <- 53

setwd(directorio_CASA)

source(paste("../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

load(paste(ruta_datos, "/", proyecto, " ", experimento, " CASA procesados.Rdata", sep = ""))

datos <- subset(datos, Npuntos >= floor(fps * prop_tiempo_en_campo))
datosl <- aggregate(VCL ~ ident, datos, function(x) length(x[!is.na(x)]))
datos <- datos[datos$ident %in% datosl$ident[datosl$VCL > nspz], ] #remove samples with too few spermatozoa

names(datos) <- tolower(names(datos))

library(plyr)

medianas <- aggregate(datos[ , tolower(variables)], as.list(datos[ , identnames]), function(x) round(median(x, na.rm = TRUE), 2))

datosmot <- ddply(datos, identnames, summarise, fps = max(npuntos, na.rm = TRUE), totalspz = length(vcl), totalmot = length(vcl[!is.na(vcl)]), totalprog = length(vcl[!is.na(vcl) & vcl > vcl_prog & str > str_prog]), mot = round(totalmot / totalspz * 100, 1), prog = round(totalprog / totalspz * 100, 1))

datosmot <- merge(datosmot, medianas, by = c(identnames), all = TRUE)

datos <- datosmot

save2(datos, filename = paste(ruta_datos, "/", proyecto, " ", experimento, " CASA medians", sep = ""))

