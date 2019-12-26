#Instrucciones
#Añadir "data" al nombre de las carpetas a analizar
#Modificar lo que sea necesario en analysis config.R.
#Modificar dirtrabajo con el nombre de la carpeta del análisis.
#Poner examinar a 1, y comprobar que todo esté bien; modificar config.R si fuese necesario (cambiar el punto de corte, etc.).
## try http:// if https:// URLs are not supported
# source("https://bioconductor.org/biocLite.R")
# biocLite("flowViz")
# biocLite("flowStats")
# Sys.setenv(PATH=paste("/opt/local/bin", Sys.getenv("PATH"), sep=":"))
# biocLite("flowQ")
# etc...

#Poner examinar a 0 y ejecutar para guardar los resultados y obtener las gráficas.

examinar <- 0
ir_a_archivo <- 1 #dejar a 1 excepto si se quiere mirar un archivo concreto
ruta_base <- "~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento/"
directorio <- "datos/SCSA Porcicoll"
equipo <- "ibiomed"
ruta_general <- paste(ruta_base, "/", directorio, "/", sep = "")
hacer_gráficas <- 1
corte_fijo <- 1

limpieza_fscssc_auto <- 1
corregir_por_std <- 1

source(paste(ruta_base, "/programas R/preparar datos/programas_R SCSA porcino/ProcesadoSCSA porcino con suciedad config y funciones 201901.R", sep = "/"))

offset_red <- 0
offset_green <- 0

directorios <- scan(paste(ruta_base, "/programas R/preparar datos/programas_R SCSA porcino/000 para procesar.R", sep = "/"), what = "character", sep = "\n", comment.char = "#")

options(warn = 1)

scsa_plot <- 0
if(examinar) {
	guardar <- 0
	guardar_graficas <- 0
	guardar_figuras_informe <- 0
	corte_fijo <- 1
	kAuto <- 0
} else {
	guardar <- 1
	guardar_graficas <- 1
	guardar_figuras_informe <- 1
	corte_fijo <- 1
	kAuto <- 1
}
guardar_figuras_informe <- 0

especial_limpieza <- NULL

guardar_muestra <- TRUE


ancho <- 1066
if(scsa_plot) ancho <- 1600
contador <- 0
	
for(dirtrabajo in directorios) {

	directorio_analisis <- paste(ruta_general, dirtrabajo, sep = "/")
	setwd(directorio_analisis)
	
	cat("\n----------\nAnalizando ", dirtrabajo, "\n\n", sep = "")	

	datosfil <- ""
	id <- ""
	
	if (grepl("micros", dirtrabajo, ignore.case = TRUE))
	{
		equipo <- "micros"
	} else if (!exists("equipo"))
	{
		equipo <- "ibiomed"
	}	
	if(!file.exists("000 analysis config.R")) {
		file.copy(paste(ruta_base, "/programas R/preparar datos/programas_R SCSA porcino/000 analysis config ", equipo,".R", sep = ""), "./000 analysis config.R", overwrite = FALSE)
		cat("ATENCIÓN, aplicando configuración para ", toupper(equipo), " por defecto\n\n", sep = "")	
	}
	
	datos_orig <- NULL
	source("000 analysis config.R")
	
	crear.directorios(ruta_general, dirtrabajo)
		
	if(is.null(directorios)) {
		kdir <- list.files(pattern = "data$")
		kdir <- kdir[file.info(kdir)$isdir]
	} else {
		kdir <- list.files(pattern = "data$")
	}

	for(directorio in kdir) {		
		
		setwd(paste(directorio_analisis, "/", directorio, sep = ""))
	archivo_salida <- paste(directorio, "-summary SCSA.xlsx", sep = "")
		if(file.exists(archivo_salida)) {
			file.copy(archivo_salida, paste(directorio, "-summary SCSA backup.xlsx", sep = ""), overwrite = FALSE)
		}	
		datosfiles <- list.files()
		thesefiles <- length(datosfiles)
		cat("Hay ", thesefiles, " archivos en este directorio.\n\n", sep = "")	

		contador <- 1 + ir_a_archivo - 1
		
		respuesta <- ""
	
		limites <- NULL
	
		resultados <- data.frame()
		
		while(contador <= length(datosfiles))
		{
			guardar_muestra <- TRUE
			datosfil <- datosfiles[contador]
			
			fcs_data <- read.FCS(datosfil, transformation = "linearize", emptyValue = FALSE)
			citómetro <- unlist(keyword(fcs_data, "CYT"))
			if (is.null(citómetro))
			{
				citómetro <- keyword(fcs_data, "$CYT")
			}
			if (citómetro == "FACSCalibur")
			{
				id <- paste(fcs_data@description$'SAMPLE ID', fcs_data@description$'PATIENT ID')
			} else if (citómetro == "CyAn")
			{
				id <- fcs_data@description$SAMPLEID
			}
			id <- trim(id)
			if (length(id) == 0)
				id <- "noID"

			#En caso de que activemos FL-4 por error; esto podría arreglarse de otra manera, en analysis config, pero se ejecuta luego y podría romper más cosas si muevo el source
			if(names(as.data.frame(fcs_data@exprs))[7] == "FL4-H") {
				nombre_FL2W <- "FL4-H"
			} else {
				if (citómetro != "CyAn")
				{
					nombre_FL2W <- "FL2-W"
				}
			}
					
			datos_orig <- as.data.frame(fcs_data@exprs[ , c(nombre_fsc, nombre_ssc, nombre_rojo, nombre_verde, nombre_naranja, nombre_FL2A, nombre_FL2W)])
			
			names(datos_orig) <- c("FSC", "SSC", "Red", "Green", "Orange", "FL2A", "FL2W")
			if (citómetro == "CyAn")
			{
				datos_orig <- within(datos_orig,
				{
					Red <- round(Red * 1024 / 2^16)
					Green <- round(Green * 1024 / 2^16)
					Orange <- round(Orange * 1024 / 2^16)
					FL2A <- round(FL2A * 1024 / 2^16)
					FL2W <- round(FL2W * 1024 / 2^16)
				})
			}
			datos_orig$or_gr_ratio <- datos_orig$Orange / datos_orig$Green
			datos_orig <- preprocessing(datos_orig, id, contador)
			
			source("../000 analysis config.R")
			
			#Cambio en junio 2015, para configuraciones previas:
			if(kCorte + kpico < 100) {
				kCorte <- 50
				kpico <- 200
			}
			######
						
			datos_orig <- preprocessing(datos_orig, id, contador)
			id2 <- gsub("[\\|/:.]", "_", id)


			datos_orig <- within(datos_orig, {
				dfi <- Red / (Green + Red) * 1023
				dfi[is.na(dfi)] <- 0
				fscgreen <- FSC / Green * multiplicador_fscgreen
			})

			datos_limpios <- limpieza(datos_orig, limpieza_origen, hacer_limpieza_origen, especial_limpieza = especial_limpieza, limpieza_fscssc =  limpieza_fscssc, limpieza_fscssc_auto = limpieza_fscssc_auto, modif_limpieza_fscssc_orig = modif_limpieza_fscssc_orig)
			limpieza_fscssc <- datos_limpios$limpieza_fscssc
			limpieza_fscssc_orig <- datos_limpios$limpieza_fscssc_orig
			datos_limpios <- datos_limpios$datos
			rojo_orig <- round(mean(datos_limpios$Red), 1)
			verde_orig <- round(mean(datos_limpios$Green), 1)
			if(corregir_por_std) {
				if(grepl("std", tolower(id)) | grepl("est", tolower(id))) {
					offset_red <- (130 - rojo_orig) * 0.8
					offset_green <- (500 - verde_orig) * 0.8
				}
				if(abs(offset_red) >= 3) {	
					datos_limpios$Red <- datos_limpios$Red + offset_red
				} else {
					offset_red <- 0
				}
				if(abs(offset_green) >= 3) {	
					datos_limpios$Green <- datos_limpios$Green + offset_green
				} else {
					offset_green <- 0
				}
			}
			datos_limpios <- subset(datos_limpios, Green > limpieza_y)
			datos <- postprocessing(datos_limpios, datosfil, directorio, contador, id)	
			datos <- within(datos,
			{
				dfi <- Red / (Green + Red) * 1023
				dfi[is.na(dfi)] <- 0
				fscgreen <- FSC / Green * multiplicador_fscgreen
			})
			ogr_density <- density(datos$or_gr_ratio)
			limite_ogr <- limite.ogr2(ogr_density, verbose = FALSE)
			limite <- limite_ogr$limite
			picos_pos <- limite_ogr$picos_pos
			if(hacer_limpieza_ogr | id %in% limpieza_ogr_lista)
			{
				datos <- subset(datos, or_gr_ratio < limpieza_ogr$a + limpieza_ogr$b * Green)
			}
			if (hacer_limpieza_fscgreen)
			{
				datos <- subset(datos, fscgreen < ((datos$Green - limpieza_fscgreen$a) / limpieza_fscgreen$b) | (dfi > 250 & dfi < (Green + Red) * 3 - 700 & Green + Red > 400))
			}
			if(nrow(datos) < 2) #Para evitar un error con density() si no tenemos células
			{
				datos <- rbind(datos, rep(0, 10), rep(0, 10))
			}
			limites <- c(limites, limite)
			if (corte_fijo) {
			  pico <- kpico
			} else {
				densidad <- density(datos$dfi)
				breaks <- ceiling(length(datos$dfi) / 100)
				bins <- hist(datos$dfi, breaks = breaks, plot = FALSE)
				picoy <- max(bins$counts, na.rm = TRUE)
				bins_bottom <- mean(bins$breaks[bins$counts == picoy])
				bins_top <- mean(bins$breaks[which(bins$counts == picoy) + 1])
				picox <- median(datos$dfi[datos$dfi > bins_bottom & datos$dfi < bins_top])
				pico <- picox
				if(is.na(pico)) {
					pico <- 200
				} else if(pico > 250) {
					pico <- 250
				}
		    }
			n <- length(datos$dfi)
			mdfi <- round(length(datos$dfi[datos$dfi > (pico + kCorte) & datos$dfi < khDFICut]) / n * 100, 2)
			hdfi <- round(length(datos$dfi[datos$dfi >= khDFICut]) / n * 100, 2)
			tdfi <- round(mdfi + hdfi, 2)
			rojo <- round(mean(datos$Red), 1)
			verde <- round(mean(datos$Green), 1)
			if(guardar_graficas & hacer_gráficas) {
				pdf(paste(ruta_general, "/", dirtrabajo, "/", dirtrabajo, " cytograms/DNAcerdo-", dirtrabajo, "-", directorio, "-", contador, "-", id2, ".pdf", sep = ""), width = 6, height = 6)
				graficas(pico, datos, datos_orig = datos_orig, datos_limpios = datos_limpios, datosfil, id, tdfi, limite, limite = limite, auto = kAuto,  limpieza_fscssc =  limpieza_fscssc, limpieza_fscssc_orig = limpieza_fscssc_orig, limpieza_ogr_a = limpieza_ogr$a, limpieza_ogr_slope = limpieza_ogr$b)
				dev.off()
			} else {
				if(!kAuto) {
					if (hacer_gráficas)
						graficas(pico, datos, datos_orig = datos_orig, datos_limpios = datos_limpios, datosfil, id, tdfi, limite, limite = limite,  limpieza_fscssc = limpieza_fscssc, limpieza_ogr_a = limpieza_ogr$a, limpieza_ogr_slope = limpieza_ogr$b)
				} else {
					datos <- subset(datos, Green > limpieza_ogr$a + Orange * limpieza_ogr$b | Green > 400)
				}
			}
			n <- length(datos$dfi)
			meddfi <- round(median(datos$dfi), 2)
			maddfi <- round(mad(datos$dfi), 2)
			skewdfi <- round(skewness(datos$dfi), 2)
			xdfi <- round(mean(datos$dfi), 2)
			sddfi <- round(sd(datos$dfi), 2)
			mdfi <- round(length(datos$dfi[datos$dfi > (pico + kCorte) & datos$dfi < khDFICut]) / n * 100, 2)
			hdfi <- round(length(datos$dfi[datos$dfi >= khDFICut]) / n * 100, 2)
			tdfi <- round(mdfi + hdfi, 2)
			hds <- round(length(datos$Green[datos$Green > kHDSCut]) / n * 100, 2)
			rojo <- round(mean(datos$Red), 1)
			verde <- round(mean(datos$Green), 1)
			resultado <- data.frame(directorio =  dirtrabajo, archivo = datosfil, id, n, corte = round(pico + kCorte), meddfi, maddfi, skewdfi, xdfi, sddfi, mdfi, hdfi, tdfi, hds, rojo, verde)
			cat("\n", contador, " de ", thesefiles, "\n", sep = "")
			cat("Pico:", round(pico), "\n")
			if(corregir_por_std) {
				cat("rojo orig: ", rojo_orig, ", verde orig: ", verde_orig, "\n", sep = "")
				cat("delta rojo: ", offset_red, ", delta verde: ", offset_green, "\n", sep = "")
			}
			print(resultado[ , 1:(floor(ncol(resultado) / 2) - 3)])
			print(resultado[ , (floor(ncol(resultado) / 2) + 2):ncol(resultado)])
			if(guardar_figuras_informe & hacer_gráficas) {
				png(file = paste(ruta_general, "/", dirtrabajo, "/", dirtrabajo, " figures for reports/", dirtrabajo, "-", directorio, "-", id, ".png", sep = ""), width = ancho, height = 520, pointsize = 24)
				graficas.informe(pico, datos, datos_orig, datos_limpios = datos_limpios, datosfil, id, tdfi, limite, scsa_plot = scsa_plot)
				dev.off()
			}			
			respuesta <- ""
			
			if (!kAuto)
			{
				if(corte_fijo) {
					if (contador == 1)
					{
						respuesta <- readline("[Intro]: siguiente; r: releer\n")
						ifelse(respuesta == "r", contador <- contador - 1, contador <- contador)
					} else
					{
						respuesta <- readline("[Intro]: siguiente; r: releer; b: retroceder\n")
					ifelse(respuesta == "r", contador <- contador - 1, ifelse(respuesta == "b", contador <- contador - 2, contador <- contador))
					}
				} else {
					respuesta <- readline("Pico:")
					pico.new <- pico
					if(respuesta != "") {
						pico.new <- as.numeric(respuesta)
						meddfi <- round(median(datos$dfi), 2)
						maddfi <- round(mad(datos$dfi), 2)
						skewdfi <- round(skewness(datos$dfi), 2)
						xdfi <- round(mean(datos$dfi), 2)
						sddfi <- round(mean(datos$sddfi), 2)
						mdfi <- round(length(datos$dfi[ datos$dfi > (pico.new + kCorte) & datos$dfi < khDFICut ]) / n * 100, 2)
						hdfi <- round(length(datos$dfi[ datos$dfi >= khDFICut ]) / n * 100, 2)
						tdfi <- round(mdfi + hdfi, 2)
						hds <- round(length(datos$Green[ datos$Green > kHDSCut ]) / n * 100, 2)
						resultado <- data.frame(directorio =  dirtrabajo, archivo = datosfil, id, n, corte = round(pico + kCorte), meddfi, maddfi, skewdfi, xdfi, sddfi, mdfi, hdfi, tdfi, hds, rojo, verde)
						cat("\n", contador, "\n", sep = "")
						cat("Pico:", round(pico), "\n")
						if(corregir_por_std) {
							cat("delta rojo:, ", offset_red, ", delta verde: ", offset_green, "\n", sep = "")
						}
						print(resultado[ , 1:(floor(ncol(resultado) / 2) - 3)])
						print(resultado[ , (floor(ncol(resultado) / 2) + 2):ncol(resultado)])
						if(hacer_gráficas) {
							if(guardar_graficas)
								pdf(paste(ruta_general, "/", dirtrabajo, "/", dirtrabajo, " cytograms/DNAcerdo-", dirtrabajo, "-", directorio, "-", contador, "-", id2, ".pdf", sep = ""))
							graficas(pico, datos, datos_orig = datos_orig, datos_limpios = datos_limpios, datosfil, id, tdfi, limite, limite = limite, auto = kAuto,  limpieza_fscssc =  limpieza_fscssc, limpieza_ogr_a = limpieza_ogr$a, limpieza_ogr_slope = limpieza_ogr$b)
							if(guardar_graficas)
								dev.off()
							if(guardar_figuras_informe)
							{
								png(file = paste(ruta_general, "/", dirtrabajo, "/", dirtrabajo, " figures for reports/", dirtrabajo, "-", directorio, "-", contador, "-", id, ".png", sep = ""), width = ancho, height = 520, pointsize = 24)
								graficas.informe(pico, datos, datos_orig, datosfil, id, tdfi, limite, scsa_plot = scsa_plot)
								dev.off()
							}
						}
						if (contador == 1)
						{
							respuesta <- readline("[Intro]: siguiente; r: releer\n")
							ifelse(respuesta == "r", contador <- contador - 1, contador <- contador)
						} else
						{
							respuesta <- readline("[Intro]: siguiente; r: releer; b: retroceder\n")
												ifelse(respuesta == "r", contador <- contador - 1, ifelse(respuesta == "b", contador <- contador - 2, contador <- contador))						
						}
					}
				}
			}
			citómetro <- unlist(citómetro)
			names(citómetro) <- NULL
			resultado <- cbind(resultado, citómetro)
	#		names(resultado)[length(resultado)] <- "citómetro"
			resultados <- rbind(resultados, resultado)
			contador <- contador + 1
		}
	}
	if (guardar_muestra & guardar) {
		archivo.salida <- paste(ruta_general, "/", dirtrabajo, "/", dirtrabajo, "-summary SCSA.csv", sep = "")
		write.csv2(resultados, file = archivo.salida)
	}
}