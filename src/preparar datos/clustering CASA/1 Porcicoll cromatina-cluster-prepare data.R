if(read_again | !exists("datosmov"))
{
	if(read_again) {
		cat("Reading data... ")
		nombre_dataframe <- load(paste(dirdatos, "/", proyecto, " ", experimento, " CASA procesados.Rdata", sep = ""))
		cat("done.\n")
	}
	datosmot <- datos
	rm("datos")
	if(do_clusters)	{
		#leemos el archivo con las medianas
		datos_info <- read.csv2(paste(dirdatos, "/", proyecto, " ", experimento, " CASA medians.csv", sep = ""), stringsAsFactors = FALSE)
		datos_info$id <- gsub(" ", "", apply(datos_info[ , ids], 1, paste, collapse = "."))
		admitidos <- datos_info$id[datos_info$mot > 0 & datos_info$totalmot > nspz]
		id <- gsub(" ", "", apply(datosmot[ , ids], 1, paste, collapse = "."))
		datosmot <- datosmot[id %in% admitidos, ]
	}

	datosmov <- datosmot[order(datosmot[ , ids]), ]
	if(do_clusters) {
#		datosmov <- subset(datosmov, !is.na(VCL) & !is.na(LIN) & !is.na(ALH) & !is.na(BCF)) #only motile spermatozoa; many checkings (CASA produces odd data sometimes)
		datosmov <- na.omit(datosmov)
		
		datosmov <- subset(datosmov, VCL >= VAP & VAP >= VSL) #only motile spermatozoa; many checkings (CASA produces odd data sometimes)
		
################################
		
	}
	
	datosmov <- subset(datosmov, Npuntos >= fps * prop_tiempo_en_campo)
	
	datosmov$idents <- apply(datosmov[ , identnames], 1, paste, collapse = "#")
	datosmov$idents <- sub("  ", " ", datosmov$idents)
	

	if (do_clusters) {
		datosl <- aggregate(datosmov$VCL,list(idents = datosmov$idents), length)
		datosmov <- datosmov[datosmov$idents %in% datosl$idents[datosl$x > nspz], ] #remove samples with too few spermatozoa
		datosmov <- datosmov[order(datosmov$idents), ]
		#Transformaciones
		datosmovt <- datosmov
		if(transf) {
			datosmovt[ , variables[!(variables %in% c("LIN", "STR", "WOB"))]] <- log1p(datosmovt[ , variables[!(variables %in% c("LIN", "STR", "WOB"))]])
			datosmovt[ , c("LIN", "STR", "WOB")] <- asin(sqrt(datosmovt[ , c("LIN", "STR", "WOB")] / 100))
		}
	#MDAalg no requiere transformación
	}
}

