gráficos <- 0
guardar <- 0
proyecto <- "Porcicoll"
experimento <- "cromatina"
library(xlsx)
options(xlsx.date.format="dd/MM/yyyy")

ruta_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/"
directorio <- "datos/datos procesados"
ruta_general <- paste(ruta_base, "/", directorio, "/", sep = "")

setwd(ruta_general)
source(paste("../../programas R/", proyecto, " ", experimento, " funciones y opciones.R", sep = ""))

archivo_origen <- paste(proyecto, " ", experimento, " SCSA.Rdata", sep = "")

load(archivo_origen)
datos <- subset(datos, !grepl("EST|AO\\+DET|STD|std", id))

names(datos)[which(names(datos) == "X")] <- "orden análisis"
datos$cia <- sub("AIM ", "", datos$cia)

datos$"fecha análisis" <- substr(datos$directorio, 1, 8)

datos <- datos[ , c("cia", "verraco", "trat", "día", "fecha análisis", "orden análisis", "directorio", "archivo", "id", "n", "corte", "xdfi", "sddfi", "mdfi", "hdfi", "tdfi", "hds", "comentarios")]

datos$"fecha análisis" <- as.Date(as.character(datos$"fecha análisis"), "%Y%m%d")

datos <- datos[order(datos$cia, datos$verraco, datos$día, datos$trat), ]

datos <- within(datos, {
	id <- toupper(id)
	id <- gsub(" REP| BIS", "", id)
	cia <- toupper(cia)
	verraco <- sub("id", "", verraco)
})

datos$cia[datos$cia == "LEON"] <- "LEÓN"

datoscsv <- datos

datos$día <- sub("D", "", datos$día)
archivo <- paste(proyecto, " ", experimento, " SCSA identificados", sep = "")
save2(datos, filename = archivo)

write.xlsx2(datos, paste(archivo, ".xlsx", sep = ""), row.names = FALSE)

datosrev <- subset(datos, !grepl("OMITIR", comentarios))
if (gráficos) {
	library(ggplot2)
	g <- ggplot(datosrev, aes(x = `orden análisis`, y = tdfi, group = cia)) +
		geom_boxplot(alpha = 0, outlier.shape = NA) +
		geom_point(aes(col = `fecha análisis`)) +
		facet_grid( ~ `fecha análisis`) +
		theme(legend.position = "none") +
			scale_y_continuous(limits = c(0, 4))
	plot(g)
	ggsave("Porcicoll DFI.pdf")
	g <- ggplot(datosrev, aes(x = `orden análisis`, y = hds, group = cia)) +
		geom_boxplot(alpha = 0, outlier.shape = NA) +
		geom_point(aes(col = `fecha análisis`)) +
		facet_grid( ~ `fecha análisis`) +
		theme(legend.position = "none") +
		scale_y_continuous(limits = c(0, 12))
	plot(g)
	ggsave("Porcicoll HDS.pdf")
}

medias <- aggregate(cbind(n, corte, xdfi, sddfi, mdfi, hdfi, tdfi, hds) ~ cia + verraco + día + trat, datosrev, function(x) round(mean(x, na.rm = TRUE), 2))
medias[ , c("n", "corte")] <- round(medias[ , c("n", "corte")])

archivo <- paste(proyecto, " ", experimento, " SCSA procesados", sep = "")
save2(medias, filename = archivo)

desvest <- aggregate(cbind(n, corte, xdfi, sddfi, mdfi, hdfi, tdfi, hds) ~ cia + verraco + día + trat, datosrev, function(x) round(sd(x, na.rm = TRUE), 2))

diffs <- aggregate(cbind(n, corte, xdfi, sddfi, mdfi, hdfi, tdfi, hds) ~ cia + verraco + día + trat, datosrev, function(x) round(abs(diff(x, na.rm = TRUE)), 2))

diffs[ , 5:ncol(diffs)] <- apply(diffs[ , 5:ncol(diffs)], 2, function(x) sapply(x, function(x) ifelse(length(x) == 0, NA, x)))

if (any(is.na(desvest$n))) {
	cat("\nPosibles errores en identificaciones (sólo un tubo analizado)\n")
	print(desvest[is.na(desvest$n), ])
}

variables <- c("xdfi", "sddfi", "mdfi", "hdfi", "tdfi", "hds")
coefvar <- desvest
coefvar[ , variables] <- round(coefvar[ , variables] / medias[ , variables] * 100)

if (any(diffs$tdfi > 1)) {
	cat("\nPosibles errores en replicados, tdfi\n")
	print(diffs[diffs$tdfi > 1 & !is.na(diffs$tdfi), ])
}
if (any(diffs$hds > 1)) {
	cat("\nPosibles errores en replicados, hds\n")
	print(diffs[diffs$hds > 3 & !is.na(diffs$tdfi), ])
}