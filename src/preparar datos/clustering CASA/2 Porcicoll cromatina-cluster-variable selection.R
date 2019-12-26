#Ejecutar primero uno de los archivos de inicialización

if (thestep1)
{
	método <- step1
	resultados_suffix <- "step1"
	datosvarsel <- datosmovt
} else if (thestep2)
{
	método <- step2
	resultados_suffix <- "step2"
	step2_medoids <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data medoids.Rdata", sep = ""))
	datosvarsel <- data4step2
} else
{
	stop("no step selected")
}

nsink <- sink.number()
if(nsink > 0) for(i in 1:nsink) sink()

if (guardar_resultados)
	sink(file = paste(resultados, proyecto, " ", experimento, prefijo, "-", método, "-selección de variables-", resultados_suffix, ".txt", sep = ""), split = TRUE)

if(use_mostinfluencial) {
	cat("\nVariable selection using getSvdMostIncluential (Gmisc)\n------------------------------------------------------\n")
	#http://gforge.se/2013/04/using-the-svd-to-find-the-needle-in-the-haystack/
	library(Gmisc)
	
	if (guardar_resultados)
		pdf(file = paste(resultados, proyecto, " ", experimento, prefijo, "-", método, "-selección de variables mostinfluential-", resultados_suffix, ".pdf", sep = ""), width = 5, height = 5)
	influential <- getSvdMostInfluential(datosvarsel[ , variables], 
                      quantile=.8, 
                      similarity_threshold = .9,
                      plot_threshold = .05,
                      plot_selection = TRUE)
	if(guardar_resultados) dev.off()
    most_influential <- variables[influential$most_influential]
    cat("Most influential variables:", most_influential, sep = " ")
	cat("\n")
	if(!guardar_resultados) readline()
}

if(use_varclus) {
	cat("\nVariable selection using varclus (Hmisc)\n------------------------------------------------------\n")
	sim_method <- "hoeffding"
	library(Hmisc)
	ldatos <- nrow(datosvarsel)
	if(ldatos < 5000) {
		nsamples <- ldatos
	} else {
		nsamples <- 5000
	}
	
	data_sample <- as.matrix(datosvarsel[sample(1:nrow(datosvarsel), nsamples), variables])
	
	cat("\nRelationship between variables (varclus):\n")
	
	data_sample <- data_sample[!is.na(data_sample[ , 10]), ]
	data_sample <- scale(data_sample)
	
	var_clust <- varclus(data_sample, similarity = sim_method)
	print(var_clust)
	
	cat("\nCorrelations (Pearson)\n")
	print(round(cor(data_sample), 2))
	
	if(guardar_resultados)
		pdf(file = paste(resultados, proyecto, " ", experimento, prefijo, "-", método, "-selección de variables varclus ", resultados_suffix, ".pdf", sep = ""), width = 5, height = 5)
		plot(var_clust)
		mtext(paste("for ", método, ", data transformed and scaled", sep = ""))
	if(guardar_resultados) dev.off()
	if(!guardar_resultados) readline()
}


if(use_compasw) {
	cat("\nVariable selection using CLARA and average silhouette width\n")
	combinaciones <- list(
		mycomb1 = c("VCL", "LIN", "WOB"),
		mycomb2 = c("BCF", "STR", "WOB", "MDAabs"),
		mycomb3 = c("BCF", "STR", "WOB"),
		mycomb4 = c("BCF", "LIN", "WOB"),
		altcomb = c("VCL", "STR", "WOB"),
		altcomb3 = c("VCL", "LIN", "WOB", "MDAalg"),
		altcomb4 = c("VCL", "LIN", "WOB", "MDAabs"),
		altcomb5 = c("VCL", "LIN", "WOB", "MDAalg", "MDAabs"),
		VCL = c("VCL"),
		VSL = c("VSL"),
		VAP = c("VAP"),
		LIN = c("LIN"),
		STR = c("STR"),
		WOB = c("WOB"),
		ALH = c("ALH"),
		BCF = c("BCF"),
		DNC = c("DNC"),
		DNCm = c("DNCm"),
		MDAabs = c("MDAabs"),
		MDAalg = c("MDAalg"),
		FULL = variables,
		FULLb = variables[-c(8, 12)],
		FULLc = variables[-c(12)],
		NODERIV = variables_noderiv,
		NODERIVb = c("VCL", "VSL", "VAP", "ALH", "MDAabs"),
		OPT1 = c("VCL", "ALH", "LIN"),
		OPT2 = c("VCL", "LIN"),
		OPT4 = c("VCL", "ALH", "LIN", "MDAabs"),
		OPT5 = c("VCL", "ALH", "LIN", "DNCm", "MDAabs"),
		OPT6 = c("VCL", "ALH", "LIN", "DNC", "MDAabs"),
		OPT7 = c("VCL", "ALH", "VSL", "MDAabs"),
		OPT8 = c("VAP", "ALH", "LIN", "MDAabs"),
		OPT9 = c("VCL", "BCF", "VSL", "MDAabs"),
		OPT10 = c("VCL", "ALH", "STR", "MDAabs"),
		OPTc1 = c("VCL", "VSL", "VAP", "STR", "ALH", "DNC"),
		OPTc2 = c("VCL", "VSL", "VAP", "ALH", "DNC"),
		OPTc3 = c("VCL", "VSL", "VAP", "ALH"),
		OPTc4 = c("VCL", "VSL", "ALH", "DNC"),
		OPTc5 = c("VCL", "VSL", "ALH"),
		OPTc6 = c("BCF", "VSL", "ALH"),
		OPTc7 = c("ALH", "VSL"),
		OPTc8 = c("VSL", "BCF"),
		OPTc10 = c("BCF", "DNCm", "MDAabs")
	)
	
	cat("Non hierarchical clustering using CLARA with metric ", metrica, "\n", sep = "")
	rangoclus <- 2:9
	metrica = "euclidean"
#	sampsize = 500
#	muestras_kmeans = 10
	for(varset in 1:length(combinaciones)) {
		asw <- numeric(0)
		variables_nhclus <- combinaciones[[varset]]
		cat("\nVariables: ", paste(variables_nhclus, collapse = ", "), "\n", sep = "")
		datos_claracl <- scale(datosvarsel[ , variables_nhclus])
		for (k in rangoclus) {
			cat(k, ", ", sep = "")
			asw[k] <- clara.opt(datos_claracl, kmnclus = k, metrica = metrica, muestras.kmeans = muestras_kmeans, medoides = FALSE, sample.size = sampsize)$silinfo$avg.width
			if(k == 3) 	plot(NULL, xlim = c(min(rangoclus), max(rangoclus)), ylim = c(asw[2] - 0.1, asw[2] + 0.1), type = "n", xlab = "Clusters", ylab = "Average silhouette")
			if(k > 2) segments(k - 1, asw[k - 1], k, asw[k])
		}
		k_best <- which.max(asw) 
		cat("\nsilhouette-optimal number of clusters: ", k_best, " (", asw[k_best], ")\n", sep = "")
		plot(rangoclus, asw[-1], type = "l")
		kmnclus <- k_best
	}
}


#Bootstrapping and Rand Index to compare a classification with classifications of subsets of the dataset. 
if(use_randindex) {
	cat("\nVariable selection using bootstrapping Rand index\n")
	muestras_kmeans <- 50
	sample_size = sampsize
	metrica <- "euclidean"
	
	randi <- c()
	repeticiones <- 100
	randi_list <- list()
		
	datosvarsel_nona <- na.omit(datosvarsel)
	lineas <- nrow(datosvarsel_nona)
	elemento <- 1
	
	cat("Non hierarchical clustering using CLARA with metric ", metrica, "\n", sep = "")
	rangoclus <- 2:9
	metrica = "euclidean"

	for(varset in 1:length(combinaciones)) {
		sample_size <- 500
		muestras_kmeans <- 50
		asw <- numeric(0)
		variables_nhclus <- combinaciones[varset]
		cat("\nVariables: ", paste(unlist(variables_nhclus), collapse = ", "), "\n", sep = "")
		datos_claracl <- scale(datosvarsel[ , unlist(variables_nhclus)])

		for (k in rangoclus) {
			cat(k, ", ", sep = "")
			asw[k] <- clara.opt(datos_claracl, kmnclus = k, metrica = metrica, muestras.kmeans = muestras_kmeans, medoides = FALSE, sample.size = sampsize)$silinfo$avg.width
			if(k == 3) 	plot(NULL, xlim = c(min(rangoclus), max(rangoclus)), ylim = c(asw[2] - 0.1, asw[2] + 0.1), type = "n", xlab = "Clusters", ylab = "Average silhouette")
			if(k > 2) segments(k - 1, asw[k - 1], k, asw[k])
		}
		
		k_best <- which.max(asw) 
		cat("\nsilhouette-optimal number of clusters: ", k_best, " (", asw[k_best], ")\n", sep = "")
		plot(rangoclus, asw[-1], type = "l")
		kmnclus <- k_best
		datos_claracl <- scale(datosvarsel_nona[ , unlist(variables_nhclus)])
		claracl <- clara.opt(datos_claracl, kmnclus = k, metrica = metrica, muestras.kmeans = muestras_kmeans, medoides = FALSE, sample.size = sampsize)
		cluster_full <- claracl$clustering
		cat("\nCalculating Rand Index\n")
		cat("Iteración:  ", sep = "")
		sample_size <- 100
		muestras_kmeans <- 5
		for(bootstrap in 1:repeticiones) {
			cat(rep("\b", nchar(bootstrap - 1)), bootstrap, sep = "")
			muestreo <- sample(1:lineas, 1000, replace = TRUE)
			datosmov_boot <- datos_claracl[muestreo, ]
			claracl_boot <- clara.opt(datosmov_boot, kmnclus = k, metrica = metrica, muestras.kmeans = muestras_kmeans, medoides = FALSE, sample.size = sampsize)
			cluster_boot <- claracl.boot$clustering
			randi[bootstrap] <- adjustedRandIndex(cluster_full[muestreo], cluster_boot)
		}
		
		cat("\n")
		print(summary(randi))
		randi_list[[names(variables_nhclus)]] <- list(variables = variables_nhclus[[1]], rand.index = randi)
	}
	boxplot(sapply(randi_list, "[[", 2))
	if(guardar_resultados) {
#		randi_list[[length(combinaciones) + 1]] <- list(clusters = kmnclus, bootstrap = repeticiones)
		save(randi_list, file = paste(resultados, proyecto, " ", experimento, "-kmeans-selección de variables comp models-Rand Index.Rdata", sep = ""))
	}
#	randi_list_b <- randi_list
	cat("\n")
}

if(guardar_resultados) sink()

