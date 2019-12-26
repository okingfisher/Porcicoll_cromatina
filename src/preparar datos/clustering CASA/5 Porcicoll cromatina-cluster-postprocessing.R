#Calculate cluster medians (per male, treatment and time)
library(gmodels)
#"replicate" was meant as different fields

if(!do_step2 & !only_step1) {
	cat("Reading clustered data, ", step1, "-", step2, "\n", sep = "")
	clustered <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-", step1, "-", step2, " clustered data.Rdata", sep = ""))
}

if(only_step1 & !do_step1) {
	cat("Reading clustered data, ", step1, "\n", sep = "")
	clustered <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-", step1, " clustered data.Rdata", sep = ""))
}

lista <- eval(parse(text = paste("list(", apply(as.matrix(identnames), 2, function(x) paste(x, " = datoscl2[ ,", paste("\"", x, "\"", sep = ""), "]", collapse = ", ")), ")", sep = "")))

lista_cl <- eval(parse(text = paste("list(", apply(as.matrix(identnames), 2, function(x) paste(x, " = datoscl2[ ,", paste("\"", x, "\"", sep = ""), "]", collapse = ", ")), ", cluster = datoscl2$clf)", sep = "")))

medianas <- as.data.frame(
	aggregate(
		datoscl2[ , variables],
		lista_cl,
		median,
		na.rm = TRUE
	)
)

mads <- as.data.frame(
	aggregate(
		datoscl2[ , variables],
		lista_cl,
		function(x) round(mad(x, na.rm = TRUE), 2)
	)
)

names(mads)[names(mads) %in% variables] <- paste(variables, "mad", sep = ".")

size_cl <- as.data.frame(
	aggregate(
		list(size_cl = datoscl2[ , "VCL"]), 
		lista_cl, 
		length
	)
)

size_total <- as.data.frame(
	aggregate(
		list(size_total = datoscl2[ , "VCL"]), 
		lista_cl[names(lista_cl) != "cluster"], 
		length
	)
)

props <- merge(size_cl, size_total, by = identnames)

datos_cl <- merge(medianas, mads, by = c(identnames, "cluster"))
datos_cl <- merge(datos_cl, size_cl, by = c(identnames, "cluster"))
datos_cl <- merge(datos_cl, size_total, by = identnames)

motdata <- read.csv2(
	paste(dirdatos,  "/", proyecto, " ", experimento, " CASA medians.csv", sep = "" ),
	stringsAsFactors = FALSE
)

motdata <- motdata[ , c(identnames, "mot", "prog")]
datos_cl <- merge(datos_cl, motdata, by = identnames)

datos_cl <- within(datos_cl, {
	prop <- round(size_cl / size_total * 100, 2)
	prop_mt <- round(prop * mot / 100, 2)
})

pca <- fast.prcomp(datos_cl[ , variables], scale. = TRUE)
pca_data <- cbind(cluster = datos_cl$cluster, datos_cl[ , identnames], pca$x[ , 1:2], prop = datos_cl$prop, prop_mt = datos_cl$prop)

if(guardar_resultados)
	save(pca, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-PCA-final-", step1, if(!is.null(step2)) paste("-", step2, sep = ""), ".Rdata", sep=""))

if(save_graphics)
	pdf(file = paste(resultados, proyecto, " ", experimento, prefijo, "-gráficas clusters por muestra ", step1, if(!is.null(step2)) paste("-", step2, sep = ""), ".pdf", sep = ""), width = 5, height = 5, onefile = TRUE)
with(pca_data, plot(PC1, PC2, col = as.numeric(cluster), cex = prop / 10))
legend("topleft", legend = unique(pca_data$cluster), col = unique(pca_data$cluster), pch = 1)
if(save_graphics)
	dev.off()

cat("\nSaving results... ")

if (guardar_resultados) {
	save2(datos_cl, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-clusters for analysis-final-", step1, if(!is.null(step2)) paste("-", step2, sep = ""), sep=""))
	do_medians <- 0
}
cat("hecho.\n")
