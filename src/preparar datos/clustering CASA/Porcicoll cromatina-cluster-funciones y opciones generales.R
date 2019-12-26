source("programas R/MEMESEM mel conc funciones y opciones.R")

identcasa <- c("Campo", "Nesp", "first", "last", "Npuntos", "area")

#variables <- c("VCL", "VSL", "VAP", "LIN", "STR", "WOB", "ALH", "BCF", "DNC", "DNCm", "MDAabs", "MDAalg") #¿No usar BCF?

#variables_noderiv <- c("VCL", "VAP", "VSL", "ALH", "BCF", "MDAabs")

set.seed(159753)

clara.opt <- function(datos, kmnclus, metrica, muestras.kmeans, medoides, sample.size, ...) {
	claracl <- clara(
		datos, 
		kmnclus,
		metric = metrica, 
		rngR = TRUE, 
		stand = FALSE,
		samples = muestras.kmeans,
		medoids.x = medoides,
		pamLike = TRUE,
		sampsize = sample.size,
		...
	)
	return(claracl)
}

separation.index <- function(clres, x) {
		xrows <- dim(clres$me)[1]
     	xcols <- dim(x)[2]
        ncenters <- dim(clres$centers)[1]
        maxcluster <- double(ncenters)
        minimum <- -1
        for (i in 1:ncenters) {
            maxcluster[i] <- max(dist(matrix(x[clres$cl == i], 
                ncol = xcols)))
        }
   	maxdia <- maxcluster[rev(order(maxcluster))[1]]
	for (i in 1:ncenters) {
		 maxcluster[i] <- max(dist(matrix(x[clres$cl == i], 
		 ncol = xcols)))
	}
 	maxdia <- maxcluster[rev(order(maxcluster))[1]]
	for (i in 1:(ncenters - 1)) {
		 for (j in (i + 1):(ncenters)) {
			 for (m in 1:xrows) {
				 if (clres$cl[m] == i) {
				 for (l in 1:xrows) {
				 	 if (clres$cl[l] == j) {
						diff <- x[m, ] - x[l, ]
						diffdist <- sqrt(t(diff) %*% t(t(diff)))
						fraction <- diffdist/maxdia
						if (minimum == -1) 
							minimum <- fraction
						if (fraction < minimum) 
							minimum <- fraction
						}
					 }
				 }
			 }
		 }
	}
	return(minimum)
}

