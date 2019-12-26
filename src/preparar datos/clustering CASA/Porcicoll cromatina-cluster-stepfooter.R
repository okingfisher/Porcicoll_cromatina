if(is_step1) { #this is Step 1
	if (save_step1) {
		save(data4step2, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data medoids.Rdata", sep = ""))
		save(datosmovt, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data transf full.Rdata", sep = ""))
		save(datosmov, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data full.Rdata", sep = ""))
	}
	
} else { #this is Step 2
	cat("\nFinished, processing data... \n")
	data_clusfinal <- cbind(data4step2, clf = clusters_final)
	clusters_step2 <- cbind(clus = unique(datosmov$cluster), clf = clusters_final)
	cat("\nPairing clusters\n")
	cluster.rel <- data_clusfinal[ , c("cluster", "clf")]
	datoscl2 <- merge(datosmov, cluster.rel, by = "cluster")
	if(hclus) {
		ancho <- 10
	} else {
		ancho <- 8
	}
	if(save_graphics) pdf(file = paste(resultados, proyecto, " ", experimento, prefijo, "-gráficas step2 ", step1, "-", step2, ".pdf", sep = ""), width = ancho, height = 5, onefile = TRUE)
	if(hclus) {
		layout(matrix(1:3, ncol = 3))
		plot(fitt2, which.plots = 2)
		rect.hclust(fitt2, k = k_best, border = "red")
	} else {
		layout(matrix(1:2, ncol = 2))
	}
	clusplot(data_step2, clusters_final, color = TRUE, shade = TRUE, labels = 2, lines = 0)
	plotcluster(data_step2, clusters_final)
	if(save_graphics) dev.off()

	if(save_step2)
		save(datoscl2, file = paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-", step1, "-", step2, " clustered data.Rdata", sep = ""))
	
	medianas <- as.data.frame(
		aggregate(
			datoscl2[ , variables],
			list(cluster = datoscl2$clf),
			function(x) paste(round(median(x, na.rm = TRUE), 1), round(mad(x, na.rm = TRUE), 1), sep = "±")
		)
	)
	prop <- round(table(datoscl2$clf) / nrow(datoscl2) * 100, 1)
	datos_summary <- cbind(medianas, prop = as.numeric(prop))
	nvar <- length(variables)
	gvar <- ceiling(nvar / 6)
	for(pos in 1:gvar) {
		end <- pos * 6
		if(end > nvar) {
			end <- nvar
		}
		print(datos_summary[ , c("cluster", "prop", variables[(1 + (pos - 1) * 6):end])])
	}
	if (guardar_resultados)
	{
		write.csv2(datos_summary, file = paste(resultados, proyecto, " ", experimento, prefijo, "-clusters summary-", step1, "-", step2, ".csv", sep=""), row.names = FALSE)
	}
}	
if(guardar_resultados) sink()