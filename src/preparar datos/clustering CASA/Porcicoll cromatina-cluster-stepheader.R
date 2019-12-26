nsink <- sink.number()
if(nsink > 0) for(i in 1:nsink) sink()
if (is_step1)
{ #this is Step 1
	if(guardar_resultados)
		sink(file = paste(resultados, proyecto, " ", experimento, prefijo, "-step1-", step1, ".txt", sep = ""), split = TRUE)
} else
{ #this is Step 2
	if(guardar_resultados)
		sink(file = paste(resultados, proyecto, " ", experimento, prefijo, "-", step1, "-", step2, ".txt", sep = ""), split = TRUE)
	if(!do_step1) {
		cat("Reading step 1 data, ", step1, "\n", sep = "")
		step2_medoids <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data medoids.Rdata", sep = ""))
		full_t <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data transf full.Rdata", sep = ""))
		full <- load(paste(dirdatos, "/", proyecto, " ", experimento, prefijo, "-step1 ", step1, " data full.Rdata", sep = ""))
	}
}

