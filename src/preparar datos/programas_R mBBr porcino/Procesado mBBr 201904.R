######################
# Procesar datos
######################
# El funcionamiento es similar al del SCSA. Se hace una carpeta por día de análisis. Los datos van en una subcarpeta del mismo nombre, añadiendo "data".
# En "programas R" hay un "000 para procesar.R" en el que van los nombres de las carpetas a procesar (en MacOS, copiar lista de archivos y pegar directamente).
# Hay un "000 analysis config mBBr.R" y un "000 analysis config mBBr.csv". Habría que modificar este último si fuese necesario. El "000 analysis config mBBr.csv" se llena automáticamente con los nombres de los archivos y SAMPLEID la primera vez que se corre el programa.
# Poner examinar a 1, y comprobar que todo esté bien; modificar config.R si fuese necesario (cambiar el punto de corte, etc.). Es posible que la primera ejecución resulte en error, repertir.
# Poner examinar a 0 y ejecutar para guardar los resultados y obtener las gráficas.

examinar <- 1

# dejar a 1 excepto si se quiere mirar un archivo concreto
ir_a_archivo <- 1

do_singlets <- 0

# ¿Generar gráficas?

gráficos <- 1

ruta_general <- "~/Grupo Imapor/Grupo IMAPOR - Documentos/Análisis/mBBr/"

ruta_programas <- paste(ruta_general, "programas R/", sep = "")
directorios <- scan(paste(ruta_programas, "000 para procesar.R", sep = ""), what = "character", sep = "\n", comment.char = "#")

if(examinar) {
	guardar <- 0
	guardar_gráficas <- 0
	guardar_figuras_informe <- 0
	pausas <- 1
} else {
	guardar <- 1
	guardar_gráficas <- 1
	guardar_figuras_informe <- 1
	pausas <- 0
}

guardar_muestra <- TRUE
datosfil <- ""

ancho <- 800

funciones <- list.files(ruta_programas, pattern = "config y funciones.*.R")
source(paste(ruta_programas, funciones, sep = ""))


for(dirtrabajo in directorios) {
	directorio_analisis <- paste(ruta_general, dirtrabajo, sep = "")
	setwd(directorio_analisis)
	cat("Analizando ", dirtrabajo, "\n\n", sep = "")	
	if(!file.exists("000 analysis config mBBr.R"))
	{
		file.copy(paste(ruta_programas, "000 analysis config mBBr.R", sep = ""), "./000 analysis config mBBr.R", overwrite = FALSE)
		file.copy(paste(ruta_programas, "000 analysis config mBBr.csv", sep = ""), "./000 analysis config mBBr.csv", overwrite = FALSE)
		cat("ATENCIÓN, aplicando configuración por defecto\n\n")	
	}
	id <- ""
	datos_orig <- NULL
	source("000 analysis config mBBr.R")

	crear.directorios(ruta_general, dirtrabajo)
	if(is.null(directorios)) {
		kdir <- list.files(pattern = "data$")
		kdir <- kdir[file.info(kdir)$isdir]
	} else {
		kdir <- list.files(pattern = "data$")
	}

	for(directorio in kdir) {
		setwd(paste(directorio_analisis, "/", directorio, sep = ""))
		archivo_salida <- paste(directorio, "-summary", sep = "")
		if(file.exists(archivo_salida)) {
			file.copy(archivo_salida, paste(directorio, "-summary backup.xlsx", sep = ""), overwrite = FALSE)
		}	
		datosfiles <- list.files()
		
		contador <- 1 + ir_a_archivo - 1
		respuesta <- ""
	
		limites <- NULL
	
		resultados <- data.frame()
	#	configuración <- read.csv2("../000 analysis config mBBr.csv", stringsAsFactors = FALSE)

		while(contador <= length(datosfiles))
		{
			resultado <- data.frame()
			guardar_muestra <- TRUE
			elarchivo <- datosfiles[contador]	
			last_message <- capture.output(frame <- invisible(read.FCS(elarchivo, transformation = "linearize", emptyValue = FALSE)))
			citómetro <- unlist(keyword(frame, "CYT"))
			if (is.null(citómetro))
			{
				citómetro <- unlist(keyword(frame, "$CYT"))
			}
			if (citómetro == "FACSCalibur")
			{
				sampleid <- keyword(frame, "SAMPLE ID")
			} else if (citómetro == "CyAn")
			{
				sampleid <- keyword(frame, "SAMPLEID")
			} else if (grepl("MACSQuant", citómetro))
			{
				thesampleid <- keyword(frame, "$CELLS")
				parámetro <- names(frame@description)[grepl(mbbrblueh, frame@description, ignore.case = TRUE)]
				parámetroa <- names(frame@description)[grepl(mbbrbluea, frame@description, ignore.case = TRUE)]
				parnum <- substr(parámetro, 3, nchar(parámetro) - 1)
				parnuma <- substr(parámetroa, 3, nchar(parámetroa) - 1)
				voltaje <- keyword(frame, paste("$P",parnum, "V", sep = ""))
				voltajea <- keyword(frame, paste("$P",parnuma, "V", sep = ""))
				thewell <- unlist(keyword(frame, "$WELLID"))
			}
			id <- unlist(thesampleid)
			cat(contador, "\n", sep = "")
			cat("SAMPLEID: ", id, "\n", sep = "")
			
			# --------------------
			# Inicio override
			source("../000 analysis config mBBr.R")
			if (examinar)
				configuración <- read.csv2("../000 analysis config mBBr.csv", stringsAsFactors = FALSE)
			fsgate <- rectangleGate("FSC-H" = c(limpieza_fscssc["fsc_low"], limpieza_fscssc["fsc_high"]), "SSC-H" = c(limpieza_fscssc["ssc_low"], limpieza_fscssc["ssc_high"]))
			# --------------------
			# Ponemos ids a la configuración si está vacío (al menos, para el archivo actual)
			if (nrow(configuración) < contador | is.na(configuración[contador, 1])) {
				guardar_configuración <- TRUE
				nueva_línea <- data.frame(elarchivo, id, matrix(rep(NA, ncol(configuración) - 2), ncol = ncol(configuración) - 2))
				names(nueva_línea) <- names(configuración)
				configuración <- rbind(configuración, nueva_línea)
			}
			configuración_archivo <- subset(configuración, archivo == elarchivo)
			if (!is.na(configuración_archivo$ajuste_mbbr))
				ajuste_fluombbr <- configuración_archivo$ajuste_mbbr
			if (!is.na(configuración_archivo$corte_mbbr_bajo))
				corte_lowmbbr <- configuración_archivo$corte_mbbr_bajo
			if (!is.na(configuración_archivo$corte_pi_bajo))
				corte_lowpi <- configuración_archivo$corte_pi_bajo
	#		comentarios <- paste(ifelse(id == paste(sampleid, "-", réplica, sep = ""), "", "error ID"), sep = "")
			comentarios <- paste(ifelse(id == configuración_archivo$sampleid, "", "error ID"), sep = "")
			comentarios <- paste(configuración_archivo$comentarios, ifelse(comentarios == "", "", " "), ifelse(is.na(configuración_archivo$observaciones), "", configuración_archivo$observaciones), sep = "")
			thissampleid <- configuración_archivo$sampleid
			repl <- configuración_archivo$réplica
			begin_time <- description(frame)$`$BTIM`
			end_time <- description(frame)$`$ETIM`
			time_start <- ifelse(is.na(configuración_archivo$start_s), 1, configuración_archivo$start_s)
			time_end <- ifelse(is.null(configuración_archivo$end_s) | is.na(configuración_archivo$end_s), max(exprs(frame)[ , 1]) - 2, max(exprs(frame)[ , 1]) - configuración_archivo$end_s)
			# Fin override					
			# --------------------
			id <- paste(thissampleid, repl, sep = "-")
			cat("Identificación: ", id, "\n", sep = "")
			# --------------------
			# Siempre está bien quitar los eventos iniciales y finales
			timegate0 <- rectangleGate("Time" = c(time_start, time_end))
			frame_timefiltered <- Subset(frame, timegate0)
			# --------------------
			# Filtramos los eventos por tiempo, quitamos zonas con flujo inestable.
			if (!is.na(configuración_archivo$time_gates)) {
				timegates <- unlist(strsplit(configuración_archivo$time_gates, ";"))
				for (timelimits in timegates) {
					timelimits <- as.numeric(unlist(strsplit(timelimits, "-")))
					timegate <- rectangleGate("Time" = c(timelimits[1], timelimits[2]))
				#	timefilter <- filter(frame_timefiltered, timegate)
					frame_timefiltered <- Subset(frame_timefiltered, !timegate)
				}
			}
			# gate para singlets
			try(singletgate <- gate_singlet(frame, maxit = 100, wider_gate = TRUE, prediction_level = 0.99999))
			if (do_singlets)	 {
				# filtramos singlets
				frame_fsafshfiltered <- Subset(frame_timefiltered, singletgate)
			} else 
			{
				frame_fsafshfiltered <- frame_timefiltered
			}
			## Defining the initial gate
			initialgate <- rectangleGate("FSC-H" = c(2, 800), "SSC-H" = c(2, 800))
			initialgatembbr <- rectangleGate("FL1-A" = c(0, 999), "FL1-H" = c(0, 999))
			initialgatepi <- rectangleGate("FL5-A" = c(0, 999), "FL5-H" = c(0, 999))
			# filtramos valores extremos para FSC/SSC
			frame_fsafshfiltered <- Subset(frame_fsafshfiltered, initialgate)
			frame_fsafshfiltered <- Subset(frame_fsafshfiltered, initialgatembbr)
			frame_fsafshfiltered <- Subset(frame_fsafshfiltered, initialgatepi)
			# y arreglamos los minRange
			frame_fsafshfiltered@parameters@data$minRange[11] <- min(exprs(frame_fsafshfiltered)[ , "FL1-A"])
			frame_fsafshfiltered@parameters@data$minRange[17] <- min(exprs(frame_fsafshfiltered)[ , "FL5-A"])
			######
			# transformamos datos a una escala adecuada
			parámetros <- pData(parameters(frame))$name[pData(parameters(frame))$desc %in% c(fsch, fsca, ssch, ssca, mbbrblueh, mbbrbluea, pih, pia)]
			transf <- transformList(parámetros, transfmethod)
			dataTransformed <- transform(frame_fsafshfiltered, transf)	
			# ¿Hay preprocesado?
			dataTransformed <- preprocessing(dataTransformed, id)
			# filtramos espermatozoides en FSC/SSC
			frame_fsssfiltered <- Subset(dataTransformed, fsgate)
			parámetros <- pData(parameters(frame))$name[c(11, 12, 17, 18)]
		#	transf <- transformList(parámetros, logiclet)
		#	frame_fsssfiltered_logicle <- transform(frame_fsssfiltered, transf)
			# quitamos debris por pi-H
			pilowdebrisgate <- rectangleGate(filterId = "debrislowpi", "FL5-H" = c(corte_lowpi, Inf))
			frame_pilowfiltered <- Subset(frame_fsssfiltered, pilowdebrisgate)
			try(densidad <- density(frame_pilowfiltered@exprs[frame_pilowfiltered@exprs[ , "FL5-H"] > 0.5 & frame_pilowfiltered@exprs[ , "FL5-H"] < 2.5, "FL5-H"]))
			picopi <- densidad$x[median(find.peaks(densidad$y, m = 500))]
			pidebrisgate <- rectangleGate(filterId = "debris", "FL5-H" = c(picopi - tolerancia_pimin, picopi + tolerancia_pihigh))
			frame_pifiltered <- Subset(frame_pilowfiltered, pidebrisgate)
			# Eliminamos debris, con mBBr bajo
			mbbrdebrisgate <- rectangleGate(filterId = "debris", "FL1-H" = c(corte_lowmbbr, Inf))
			frame_filtered <- Subset(frame_pifiltered, mbbrdebrisgate)			
			frame_filtered@exprs[ , "FL1-H"] <- frame_filtered@exprs[ , "FL1-H"] + ajuste_fluombbr
			frame_filtered@exprs[ , "FL1-A"] <- frame_filtered@exprs[ , "FL1-A"] + ajuste_fluombbra
			theids <- strsplit(thissampleid, " ")
			if (any(theids[[1]] %in% c("D", "DTT")))
			{
				mbbrdebrisgate2 <- rectangleGate(filterId = "debris", "FL1-H" = c(1.2, 2.8))
			} else
			{
				mbbrdebrisgate2 <- rectangleGate(filterId = "debris", "FL1-H" = c(0.2, 2))
			}
			frame_filtered <- Subset(frame_filtered, mbbrdebrisgate2)
		#	frame_filtered_logicle <- transform(frame_filtered, transf)

			# Averiguamos dónde está el pico, pero primero eliminamos las colas, por si hay picos que puedan ser mayores.
			try(densidad <- density(frame_filtered@exprs[frame_filtered@exprs[ , "FL1-H"] > 0.5 & frame_filtered@exprs[ , "FL1-H"] < 2.5, "FL1-H"]))
			picos <- find.peaks(densidad$y, m = 25)
			picomax <- picos[densidad$y[picos] == max(densidad$y[picos])]
			picos <- picos[densidad$y[picos] > densidad$y[picomax] * 0.1]
			if (length(picos) > 1)
			{
				comentarios <- paste(comentarios, ifelse(nchar(comentarios) > 0, " ", ""), "MANYPEAKS", sep = "")
			}
			elpico <- max(picos)
		#	picombbr <- densidad$x[median(picos)]
			picombbr <- densidad$x[elpico]
			try(densidada <- density(frame_filtered@exprs[frame_filtered@exprs[ , "FL1-A"] > 0.5 & frame_filtered@exprs[ , "FL1-A"] < 2.5, "FL1-A"]))
			picosa <- find.peaks(densidada$y, m = 25)
			picomaxa <- picosa[densidada$y[picosa] == max(densidada$y[picosa])]
			picosa <- picosa[densidada$y[picosa] > densidada$y[picomaxa] * 0.1]
			if (length(picosa) > 1 & !grepl("MANYPEAKS", comentarios))
			{
				comentarios <- paste(comentarios, ifelse(nchar(comentarios) > 0, " ", ""), "MANYPEAKS", sep = "")
			}
			elpicoa <- max(picosa)
		#	picombbra <- densidada$x[median(picosa)]		
			picombbra <- densidada$x[elpicoa]		
			if (!pico_override)
			{
				# picombbr 1; anchos aproximados de cada región.
				limmin <- limmin_mbbr
				limlow <- picombbr - tolerancia_mbbr
				limmed <- picombbr + tolerancia_mbbr
				limhigh <- limmax_mbbr
				limmina <- limmin_mbbra
				limlowa <- picombbra - tolerancia_mbbra
				limmeda <- picombbra + tolerancia_mbbra
				limhigha <- limmax_mbbra			
			}
			
			############ Inicio override
			if (!is.na(configuración_archivo$medlow))
				limlow <- configuración_archivo$medlow
			if (!is.na(configuración_archivo$medhigh))
				limmed <- configuración_archivo$medhigh
			if (!is.na(configuración_archivo$medlowa))
				limlowa <- configuración_archivo$medlowa
			if (!is.na(configuración_archivo$medhigh))
				limmeda <- configuración_archivo$medhigha
			############ Fin override					
			
			# definimos regiones en mBBr-H y mBBr-A
						
			glow <- rectangleGate(filterId = "low", "FL1-H" = c(limmin, limlow))
			gmed <- rectangleGate(filterId = "med", "FL1-H" = c(limlow, limmed))
			ghigh <- rectangleGate(filterId = "high", "FL1-H" = c(limmed, limhigh))
			glowa <- rectangleGate(filterId = "low", "FL1-A" = c(limmina, limlowa))
			gmeda <- rectangleGate(filterId = "med", "FL1-A" = c(limlowa, limmeda))
			ghigha <- rectangleGate(filterId = "high", "FL1-A" = c(limmeda, limhigha))
			
			regiones <- filters(list(glow, gmed, ghigh))
			regionesa <- filters(list(glowa, gmeda, ghigha))
			
			meanmbbr <- mean(frame_filtered@exprs[ , "FL1-H"])
			medianmbbr <- median(frame_filtered@exprs[ , "FL1-H"])
			meanmbbra <- mean(frame_filtered@exprs[ , "FL1-A"])
			medianmbbra <- median(frame_filtered@exprs[ , "FL1-A"])
							
			if (!guardar_gráficas)
			{	
				plot_time_mbbr <- xyplot(`FL1-H` ~ `Time`, data = frame, smooth = FALSE, filter = timegate0, ylim = c(0, quantile(exprs(frame)[ , 12], probs = 0.99)))
				plot_time_mbbrfiltered <- xyplot(`FL1-H` ~ `Time`, data = frame_timefiltered, smooth = FALSE, ylim = c(0, quantile(exprs(frame)[ , 12], probs = 0.99)))
				plot_fsh_fsa <- xyplot(`FSC-A` ~ `FSC-H`, data = frame_timefiltered, smooth = FALSE, filter = singletgate)
				plot_fs_ss <- xyplot(`SSC-H` ~ `FSC-H`, data = dataTransformed, smooth = FALSE, filter = fsgate)
				plot_pi_ss <- xyplot(`SSC-H` ~ `FL5-H`, data = frame_fsssfiltered, smooth = FALSE, filter = pidebrisgate)
				plot_mbbr_ss_filt <- xyplot(`SSC-H` ~ `FL1-H`, data = frame_pifiltered, smooth = FALSE, filter = regiones)
				plot_mbbra_ss_filt <- xyplot(`SSC-H` ~ `FL1-A`, data = frame_filtered, smooth = FALSE, filter = regionesa)
				plot_mbbr_pi_filt <- xyplot(`FL5-H` ~ `FL1-H`, data = frame_filtered, smooth = FALSE, filter = regiones)
				plot_mbbra_pi_filt <- xyplot(`FL5-H` ~ `FL1-A`, data = frame_filtered, smooth = FALSE, filter = regionesa)
				# dibujamos histogramas (densityplots)	
				my_lines <- c(limmin, limlow, limmed, limhigh)
				my_linesa <- c(limmina, limlowa, limmeda, limhigha)
								
				hist_mbbrf <- densityplot(~ `FL1-H`, data = frame_filtered, xlim = c(0, 3)) + # No sale por filter
				layer(panel.abline(v = my_lines, col.line = "gray60", alpha = 0.4)) +
				layer(panel.abline(v = picombbr, col.line = "blue", alpha = 0.8)) +
				layer(panel.abline(v = meanmbbr, col.line = "purple", alpha = 0.8)) +
				layer(panel.abline(v = medianmbbr, col.line = "red", alpha = 0.8))
					
				
				hist_mbbraf <- densityplot(~ `FL1-A`, data = frame_filtered, xlim = c(0, 3)) + # No sale por filter
				layer(panel.abline(v = my_linesa, col.line = "gray60", alpha = 0.4)) +
				layer(panel.abline(v = picombbra, col.line = "blue", alpha = 0.8)) +
				layer(panel.abline(v = meanmbbra, col.line = "purple", alpha = 0.8)) +
				layer(panel.abline(v = medianmbbra, col.line = "red", alpha = 0.8))
				
				grid.arrange(plot_time_mbbr, plot_time_mbbrfiltered , plot_fsh_fsa, plot_fs_ss, plot_pi_ss, plot_mbbr_pi_filt, plot_mbbra_pi_filt, plot_mbbr_ss_filt, hist_mbbrf, hist_mbbraf, ncol = 4, nrow = 3)
			}
			
			# Sacamos los datos para estadísticas
			resultado <- data.frame(archivo = elarchivo, cit = citómetro, muestra = thissampleid, réplica = repl, well = thewell, n = nrow(frame_filtered), lowmbbr.n = 0, medmbbr.n = 0, highmbbr.n = 0, lowmbbr.p = 0, medmbbr.p = 0, highmbbr.p = 0, meanmbbr = meanmbbr, medianmbbr = medianmbbr, picombbr = picombbr, lowmbbrA.n = 0, medmbbrA.n = 0, highmbbrA.n = 0, lowmbbrA.p = 0, medmbbrA.p = 0, highmbbrA.p = 0, meanmbbrA = meanmbbra, medianmbbrA = medianmbbra, picombbrA = picombbra, observaciones = comentarios, stringsAsFactors = FALSE)
			
			for (orden in 1:length(regiones))
			{
				result <- summary(filter(frame_filtered, regiones[[orden]]))
				resultado[1 , paste(nombres[orden], "mbbr.n", sep = "")] <- result$true
				resultado[1 , paste(nombres[orden], "mbbr.p", sep = "")] <- round(result$p * 100, 2)
				resulta <- summary(filter(frame_filtered, regionesa[[orden]])) 
				resultado[1 , paste(nombres[orden], "mbbrA.n", sep = "")] <- resulta$true
				resultado[1 , paste(nombres[orden], "mbbrA.p", sep = "")] <- round(resulta$p * 100, 2)
			}
						
			resultado <- data.frame(resultado, stringsAsFactors = FALSE)
			rownames(resultado) <- NULL
			
			if (!guardar)
				print(resultado)
			
			if (pausas)
			{
				respuesta <- ""
				if (contador == 1)
				{
					respuesta <- readline("[Intro]: siguiente; r: releer\n")
				} else
				{
					respuesta <- readline("[Intro]: siguiente; r: releer; b: retroceder\n")
				}
				if(respuesta == "r")
				{
					next()
				} else if (respuesta == "b")
				{
					contador <- contador - 1
					next()
				}
			}				
				
					
			resultados <- rbind(resultados, resultado)
			contador <- contador + 1
			cat("\n")
		}
		cat("\n")
	}

	if (guardar_muestra & guardar)
	{
		write.csv2(resultados, file = paste("../", archivo_salida, ".csv", sep = ""), row.names = FALSE, na = "")
				
		if(file.exists(paste("../", archivo_salida, sep = "")))
		{
			file.copy(paste("../", archivo_salida, sep = ""), paste("../", archivo_salida, "-anterior.xlsx", sep = ""))
		}
		write.xlsx(resultados, file = paste("../", archivo_salida, ".xlsx", sep = ""), sheetName = paste("Chromosperm", dirtrabajo), row.names = FALSE, col.names = TRUE, append = FALSE)
	}
	if (guardar_configuración) write.csv2(configuración, file = paste("../000 analysis config mBBr.csv", sep = ""), row.names = FALSE, na = "")
}
