dirdatos <- "datos/datos procesados CASA"
ruta <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento"
setwd(ruta)
proyecto <- "Porcicoll"
experimento <- "cromatina"
programas_R <- "programas R"
source(paste(programas_R, "/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))
identnames <- c("ident", "verraco", "muestreo", "día", "trat")
identcasa <- c("Campo", "Nesp", "first", "last", "Npuntos")
variables <- c("VCL", "VSL", "VAP", "LIN", "STR", "WOB", "ALH", "BCF", "DNC", "DNCm")

ids <- identnames
