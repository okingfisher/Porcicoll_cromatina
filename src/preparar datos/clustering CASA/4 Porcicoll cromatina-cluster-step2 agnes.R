#Step 2
is_step1 <- FALSE

dist_method <- "euclidean"
clust_method <- agnes_method_step2
hclus <- 1

source(paste(programas_R, "/", proyecto, " ", experimento, "-cluster-stepheader.R", sep = ""))

cat("\nSTEP 2\n")
cat("Hierarchical clustering with distances using metric ", dist_method, " and clustering using AGNES with method ", clust_method, "\n", sep = "")
cat("Variables: ", paste(variables_step2, collapse = ", "), "\n", sep = "")

#if(!do.hclus.varsel) hclus.varsel <- load(paste(proyecto, "-", experimento, "-mclust variable selection clustvarsel.Rdata", sep = ""))

#prepare a dataset with standardized variables

data_step2 <- scale(data4step2[ , variables_step2])

maxcl <- 6
rangoclus <- 2:maxcl

distancias <- dist(data_step2, method = dist_method)
fitt2 <- agnes(distancias, method = clust_method)

if (0)
{
	asw <- c()
	for(k in rangoclus) {
		cat(k, ", ", sep = "")
		clusters <- cutree(fitt2, k)
		cl_stats <- cluster.stats(distancias, clusters, silhouette = TRUE)
		asw[k] <- cl_stats$avg.silwidth
	}
	k_best <- which.max(asw)
	cat("\nsilhouette-optimal number of clusters:", k_best, "\n")
}
if (clust_method == "ward")
{
	clust_method_NbClust <- "ward.D2"
}
pdf(file = NULL)
nc <- try(NbClust(data_step2, distance = dist_method, min.nc = 2, max.nc = maxcl, method = clust_method_NbClust))
dev.off()

ranking <- table(nc$Best.nc[1,])
k_best <- as.numeric(names(ranking[which.max(ranking)]))
cat("\nOptimal number of clusters: ", k_best, ", by ", max(ranking), " tests\n", sep = "")
clusters_step2 <- cutree(fitt2, k_best)
if(do_dudahart) {
	dh <- dudahart2(data_step2, clusters_step1)
	if(dh$cluster1) {
		cat("Duda & Hart test TRUE (", dh$p.value, "), says 1 cluster\n", sep = "")
#		k_best <- 1	
	}
}

if (!is.null(force_final_clus))
	k_best <- force_final_clus

clusters_final <- cutree(fitt2, k_best)

source(paste(programas_R, "/", proyecto, " ", experimento, "-cluster-stepfooter.R", sep = ""))
