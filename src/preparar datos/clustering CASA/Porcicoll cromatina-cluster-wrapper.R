#Ejecutar primero uno de los archivos de inicialización
guardar_resultados <- 1
# According to getSvdMostIncluential, /see results/ are "most influential". However, varclus shows that they are highly related (Pearson shows high correlation), thus they are similarly influential. I chose /see results/.
# getSvdMostIncluential shows a second group in influence: /see results/. I select /see results/, because varclus shows that /see results/ are related and Pearson shows high correlation.
# I discard other variables by per getSvdMostIncluential.

variables_step1 <- c("VCL", "VAP", "LIN", "WOB")
variables_step2 <- c("VCL", "VAP", "LIN", "WOB")

fps <- 53
prop_tiempo_en_campo <- 0.6
nspz <- 1

do_preparedata <- 1 # leave with 1, generally

do_step1_varsel <- 0
do_step1 <- 1
do_step2_varsel <- 0
do_step2 <- 1
do_postprocessing <- 1
do_coding <- 0

calculate_optimal_step1 <- 1
k_best_step1 <- 12
k_min_step1 <- NULL
k_max_step1 <- NULL
k.best.step1 <- function (elementos, k_max_step1) {
	kbest <- as.integer(sqrt(elementos))
	if (kbest > k_max_step1)
		kbest <- k_max_step1
	return(kbest)
}

ruta <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento"
setwd(ruta)

if (!file.exists("resultados/clustering"))
	dir.create("resultados/clustering")

if (do_step1) {
	do_step2_varsel <- 1
	do_step2 <- 0
	do_postprocessing <- 0	
}

if (do_step2) {
	do_step2_varsel <- 0
	do_step2 <- 1
	do_postprocessing <- 1
}

if (do_step1_varsel) {
	do_step1 <- 0
	do_step2_varsel <- 0
	do_step2 <- 0
	do_postprocessing <- 0	
}

agnes_method_step1 <- "ward"
agnes_method_step2 <- "ward"

proyecto <- "Porcicoll"
experimento <- "cromatina"
prefijo <- ""
dirdatos <- "datos/datos procesados"
variables <- c("VCL", "VSL", "VAP", "LIN", "STR", "WOB", "ALH", "BCF", "DNC", "DNCm")

ruta <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento"
setwd(ruta)

#Data compiled, identified and cleaned before, see concatenate script

#user-defined parameters
programas_R <- "programas R/preparar datos/clustering CASA/"
resultados <- "resultados/clustering/"

#generally, we want these two on
save_step1 <- 1
save_step2 <- 1

save_graphics <- 1
override_save <- 1

k.best.step1 <- function (elementos, k_max_step1) {
	kbest <- as.integer(sqrt(elementos))
	if (kbest > k_max_step1)
		kbest <- k_max_step1
	return(kbest)
}

only_step1 <- 0
if(only_step1) {
	do_step2 <- 0
	save_step2 <- 0
}

if(do_step1_varsel | do_step2_varsel) {
	if(do_step1_varsel)
	{
		do_step1 <- 0
	}
	do_step2 <- 0
	do_postprocessing <- 0
	do_coding <- 0
}

npuntos <- function (Npuntos)
{
	npuntos <- ceiling(max(Npuntos, na.rm = TRUE) * 0.7)
	return(npuntos)
}

force_final_clus <- NULL
do_dudahart <- 1

if(override_save) {
	guardar_resultados <- 1
	save_graphics <- 1
}

#varsel
use_mostinfluencial <- 1
use_varclus <- 1
use_randindex <- 0
use_compasw <- 0

do_clusters <- 1 # of course!
read_again <- 1 # fast if we have Rdata, leave with 1

change_step1 <- 1
steps1 <- c("pam", "clara", "mclust", "agnes", "cmeans")
steps2 <- c("pam", "mclust", "agnes", "cmeans")
step1 <- steps1[4]
step2 <- NULL
step2 <- steps2[3]

transf <- 1

suffixstep1 <- step1
suffixstep2 <- step2

#inicialization
#library(mclust) #model-based clustering
library(cluster) #clara, agnes, etc.
library(gmodels) #fast.prcomp
library(fpc) #cluster.stats
library(NbClust) #NbClust
#library(mvoutlier) #sign2, outliers
#library(lattice)
is.step2 <- NULL
movfull <- 0
options(width = 100)

cat("\n\n", proyecto, experimento, "\n-----------------\n")

cat("\n==============\nWrapper will do:\n", sep = "")

if (do_preparedata) cat("Data preparation\n", sep = "")
if (do_step1_varsel) cat("Step 1: variable selection, ", sep = "")
if (do_step1) cat("Step 1: ", step1, " clustering\n", sep = "")
if (do_step2_varsel) cat("Step 2: variable selection, ", sep = "")
if (do_step2) cat("Step 2: ", step2, " clustering\n", sep = "")
if (do_postprocessing) cat("Results post-processing and saving\n", sep = "")
if (do_coding) cat("Recoding the data\n", sep = "")

cat("\n", sep = "")

#modules
if (do_preparedata) {
	cat("Data preparation... ")
	source(paste(programas_R, "1 ", proyecto, " ", experimento, "-cluster-prepare data.R", sep = ""))
	cat(" done\n", sep = "")
}

if (do_step1_varsel) {
	thestep1 <- 1
	thestep2 <- 0
	cat("Step 1: variable selection... \n")
	source(paste(programas_R, "2 ", proyecto, " ", experimento, "-cluster-variable selection.R", sep = ""))
	cat("Step 1: variable selection done\n", sep = "")
}
if (do_step1) {
	cat("Step 1: ", step1, " clustering... \n", sep = "")
	source(paste(programas_R, "3 ", proyecto, " ", experimento, "-cluster-step1 ", step1,".R", sep = ""))
	cat("Step 1:", step1, " done\n", sep = "")
}
if (do_step2_varsel) {
	thestep1 <- 0
	thestep2 <- 1
	cat("Step 2: variable selection... \n")
	source(paste(programas_R, "2 ", proyecto, " ", experimento, "-cluster-variable selection.R", sep = ""))
	cat("Step 2: variable selection done\n", sep = "")
}
if (do_step2) {
	cat("Step 2: ", step2, " clustering... \n", sep = "")
	source(paste(programas_R, "4 ", proyecto, " ", experimento, "-cluster-step2 ", step2,".R", sep = ""))
	cat("Step 2:", step2, " done\n", sep = "")
}
if (do_postprocessing) {
	cat("Results post-processing and saving... \n", sep = "")
	source(paste(programas_R, "5 ", proyecto, " ", experimento, "-cluster-postprocessing.R", sep = ""))
	cat(" done\n", sep = "")
}
if (do_coding) {
	cat("Recoding... \n", sep = "")
#	source(paste(programas_R, "../CASA preparación/", proyecto, "-CASA-cruzar códigos.R", sep = ""))
	cat(" done\n", sep = "")
}

cat("\n\n==============\nWrapper: done\n", sep = "")
