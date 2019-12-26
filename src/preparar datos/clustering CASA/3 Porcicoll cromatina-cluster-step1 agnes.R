#Step 1
is_step1 <- TRUE

pausas <- 0
dist_method <- "euclidean"
clust_method <- agnes_method_step1

source(paste(programas_R, "/", proyecto, " ", experimento, "-cluster-stepheader.R", sep = ""))

cat("\nSTEP 1\n")
cat("Hierarchical clustering with distances using metric ", dist_method, " and clustering using AGNES with method ", clust_method, "\n", sep = "")
cat("Variables: ", paste(variables_step1, collapse = ", "), "\n", sep = "")

datosstep1 <- datosmovt
clus_results <- data.frame()
identificadores <- unique(datosstep1$idents)
nsamples <- length(identificadores)
cont <- 1

for(ident in identificadores) {
	datoshclus1 <- datosstep1[datosstep1$idents == ident, ]
	muestra <- c(strsplit(ident, "#")[[1]], nrow(datoshclus1), paste(" (", cont, " de ", nsamples, ")", sep = ""))
	names(muestra) <- c(identnames, "spz.", "")
	cat("\n")
	print(muestra, quote = FALSE)
	datoshclus2 <- scale(as.matrix(datoshclus1[ , variables_step1]))
	#Selección de número de clusters
	elementos <- nrow(datoshclus2)
	cat("Elementos: ", elementos, "\n", sep = "")
	
	distancias <- dist(datoshclus2, method = dist_method)
	fitt1 <- agnes(distancias, method = clust_method)
	if (is.null(k_max_step1))
		k_max_step1 <- as.integer(sqrt(elementos))	
	if (calculate_optimal_step1)
	{
		if (is.null(k_min_step1))
			k_min_step1 <- as.integer(k_max_step1 / 2)
		if (k_max_step1 < k_min_step1)
			k_min_step1 <- as.integer(k_max_step1 / 2)
		if (k_max_step1 >= elementos)
			k_max_step1 <- elementos - 1
		if (elementos < k_min_step1)
			k_min_step1 <- 2
		mincl <- k_min_step1
		maxcl <- k_max_step1
		rangoclus <- mincl:maxcl
		asw <- c()
		for(k in rangoclus) {
			cat(k, ", ", sep = "")
			clusters <- cutree(fitt1, k)
			cl_stats <- cluster.stats(distancias, clusters, silhouette = TRUE)
			asw[k] <- cl_stats$avg.silwidth
		}
		k_best <- which.max(asw)
		cat("\nsilhouette-optimal number of clusters:", k_best, "\n")
	}
	else
	{
		k_best_step1 <- k.best.step1(elementos, k_max_step1)
		if(elementos <= k_best_step1)
		{
			k_best <- elementos
		} else {
			k_best <- k_best_step1
		}
		cat("\nNumber of clusters:", k_best, "\n", sep = "")
	}
	
	if (0) # not run
	{
		nc <- NbClust(datoshclus2, distance = dist_method, min.nc = 2, max.nc = maxcl, method = clust_method)
		ranking <- table(nc$Best.nc[1,])
		k_best <- as.numeric(names(ranking[which.max(ranking)]))
		cat("\nOptimal number of clusters:", k_best, ", by ", max(ranking), " tests\n", sep = "")
	}
	clusters_step1 <- cutree(fitt1, k_best)
	if (calculate_optimal_step1)
	{
		dh <- dudahart2(datoshclus2, clusters_step1)
		if(dh$cluster1) {
			cat("Duda & Hart test TRUE (", dh$p.value, ") only 1 cluster\n", sep = "")
			#k_best <- 1	# We carry out D&H test, but we consider more than 1 cluster anyway (eventually, step 2 will take care of this)
		}
	}
	results <- data.frame(cluster = as.numeric(paste(cont, "0", clusters_step1, sep = "")))
	results$ident <- ident
	clus_results <- rbind(clus_results, results)

	if(pausas) readline()
	cont <- cont + 1
}

cat("\nFinished, processing data... \n")

clusters <- clus_results$cluster

datosmovt$cluster <- clusters
datosmov$cluster <- clusters

datospoststep1 <- cbind(datosstep1, cluster = clusters)
estructura <- parse(text = paste("cbind(", paste(variables, collapse = ", "), ") ~ ", paste(identnames, collapse = " + "), " + idents + cluster", sep = ""))
data4step2 <- aggregate(eval(estructura), data = datosmovt, median, na.rm = TRUE)
names(data4step2) <- c(identnames, "idents", "cluster", variables)

cat("done\n")

source(paste(programas_R, "/", proyecto, " ", experimento, "-cluster-stepfooter.R", sep = ""))


###########################################################################
#END
###########################################################################
