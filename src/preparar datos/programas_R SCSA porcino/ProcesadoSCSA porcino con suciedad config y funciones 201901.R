#####################
# 20190124 Modifico para que graficar sea más rápido, eliminando puntos que se "montan".
# 20190125 Modifico escala del histograma DFI de 0 a 1000.

kCorte <- 50
khDFICut <- 500
kHDSCut <- 670 # cerdo

offset_green <- 0
offset_red <- 0
directorios <- NULL
scsa_plot <- 0
dirtrabajo <- NULL
directorio_analisis <- NULL
kdir <- NULL
directorio <- NULL
archivo_salida <- NULL
datosfiles <- NULL
datosfil <- NULL
id <- NULL
id2 <- NULL
contador <- NULL
datos_orig <- NULL
datos_limpios <- NULL
datos <- NULL
limite <- NULL
limites <- NULL
limite_ogr <- NULL
ogr_density <- NULL
densidad <- NULL
picos_pos <- NULL
hacer_limpieza_ogr <- NULL
limpieza_ogr_lista <- NULL
hacer_limpieza_fscgreen <- NULL
breaks <- NULL
bins <- NULL
picoy <- NULL
picox <- NULL
pico <- NULL
n <- NULL
mdfi <- NULL
hdfi <- NULL
tdfi <- NULL
guardar_graficas <- NULL
meddfi <- NULL
maddfi <- NULL
skewdfi <- NULL
xdfi <- NULL
sddfi <- NULL
hds <- NULL
rojo <- NULL
verde <- NULL

limpieza_fscssc <- NULL
limpieza_origen <- NULL
limpieza_sscfl2a <- NULL
limpieza_fl2a <- NULL
limpieza_fl2w <- NULL
limpieza_fscgreen <- NULL
multiplicador_fscgreen <- NULL
limpieza_y <- NULL
limpieza_ogr <- NULL
limpieza_ogr_lista <- NULL
limpieza_ogr_slope <- NULL
limpieza_ogr_a <- NULL

nombre_fsc <- NULL
nombre_ssc <- NULL
nombre_verde <- NULL
nombre_rojo <- NULL
nombre_naranja <- NULL
nombre_FL2A <- NULL
nombre_FL2W <- NULL


#####################
#bibliotecas

library(moments)
library(gdata)
library(xlsx)
options(java.parameters = "-Xmx3g")
library(IDPmisc)
library(flowCore)

#####################
# funciones

crear.directorios <- function(ruta.general, dirtrabajo) {
	directorio.analisis <- paste(ruta.general, dirtrabajo, "/", dirtrabajo, sep = "")
	dir.graficas <- paste(directorio.analisis, " cytograms", sep = "")
	dir.figuras <- paste(directorio.analisis, " figures for reports", sep = "")
	dir.informes <- paste(directorio.analisis, " reports", sep = "")
	if(!file.exists(dir.graficas)) dir.create(dir.graficas)
}

limpieza <- function(datos, limpieza_origen, hacer_limpieza_origen, limpieza_fscssc, especial_limpieza = NULL, limpieza_fscssc_auto = FALSE, modif_limpieza_fscssc_orig) {
	datos <- subset(datos, Red < 1023 & Green < 1023)
	datos <- subset(datos, FSC > limpieza_fscssc$fsc_low & FSC < limpieza_fscssc$fsc_high  & SSC < limpieza_fscssc$ssc_high & SSC > limpieza_fscssc$ssc_low)
	if (hacer_limpieza_origen) { #Limpiamos debris en origen
		datos <- subset(datos,  Green > limpieza_origen$a + limpieza_origen$b * Red) 
		if (!is.null(especial_limpieza))
		{
			datos <- especial_limpieza(datos)		
		}
	}
	if(limpieza_fscssc_auto) {
		limpieza_fscssc_orig <- limpieza_fscssc
		limpieza_fscssc$fsc_low <- 10 ^ ((log10(median(datos$FSC)) * 256 - (220 + modif_limpieza_fscssc_orig$fsc_low)) / 256)
		limpieza_fscssc$fsc_high <- 10 ^ ((log10(median(datos$FSC)) * 256 + (55 + modif_limpieza_fscssc_orig$fsc_high)) / 256)
		limpieza_fscssc$ssc_low <- 10 ^ ((log10(median(datos$SSC)) * 256 - (120 + modif_limpieza_fscssc_orig$ssc_low)) / 256)
		limpieza_fscssc$ssc_high <- 10 ^ ((log10(median(datos$SSC)) * 256 + (320 + modif_limpieza_fscssc_orig$ssc_high)) / 256)
		datos <- subset(datos, FSC > limpieza_fscssc$fsc_low & FSC < limpieza_fscssc$fsc_high  & SSC < limpieza_fscssc$ssc_high & SSC > limpieza_fscssc$ssc_low)
	}

#	datos <- subset(datos, !(Green < 50 & Green < 100 + Red * 0.1)) #Limpiamos debris con Green bajo
#	datos <- subset(datos, or_gr_ratio < 400) #Limpiamos más debris
	return(list(datos = datos, limpieza_fscssc = limpieza_fscssc, limpieza_fscssc_orig = limpieza_fscssc_orig))
}

limite.ogr2 <- function(ogr_density, verbose = FALSE) {
	densidad <- cbind(x = ogr_density$x, y = ogr_density$y)
	picos_pos <- peaks(densidad, minPH = 0.2, thr = 0.1, stepF = 0.49)
	if(verbose) {
		cat("picos según peaks (IDPmisc):", round(picos_pos$x, 1), "\n", sep = " ")
	}
	if(nrow(picos_pos) < 2) {
		limite <- max(ogr_density$x)
	} else if (picos_pos$x[2] - picos_pos$x[1] < 0.1) {
		if(nrow(picos_pos) > 2) {
			picos_pos <- picos_pos[-2, ]
			if (picos_pos$x[2] - picos_pos$x[1] < 0.1) {
				limite <- max(ogr_density$x)
			} else {
				bt.x <- ogr_density$x[ogr_density$x > picos_pos$x[1] & ogr_density$x < picos_pos$x[2]]
				bt.y <- ogr_density$y[ogr_density$x > picos_pos$x[1] & ogr_density$x < picos_pos$x[2]]
				limite <- bt.x[bt.y %in% min(bt.y)][1]
			}	
		} else {
			limite <- max(ogr_density$x)
		}
	} else {
		bt.x <- ogr_density$x[ogr_density$x>picos_pos$x[1] & ogr_density$x < picos_pos$x[2]]
		bt.y <- ogr_density$y[ogr_density$x>picos_pos$x[1] & ogr_density$x < picos_pos$x[2]]
		limite <- bt.x[bt.y %in% min(bt.y)][1]
	}
	if(verbose) {
		cat("limite según peaks (IDPmisc): ", round(limite), "\n", sep = "")
	}
	return(list(limite = limite, picos_pos = picos_pos$x))	
}

cytogram <- function(x, y, main, xlab = NULL, ylab = NULL, ylim = c(0, 1023), xlim = c(0, 1023), parg = NULL, ...) {
	datos <- unique(data.frame(x = x, y = y))
	par(parg)
	plot(datos, pch = 16, cex = 0.2, ylim = ylim, xlim = xlim, main = main, ylab = ylab, xlab = xlab, ...)
}

denscytogram <- function(x, y, main, xlab = NULL, ylab = NULL, xlim, ylim, ...) {
	colramp <- densCols(x, y, colramp = colorRampPalette(c("darkblue", "darkred", "yellow"), space = "Lab"))
	plot(x, y,  pch = 16, cex = 0.2, ylim = ylim, xlim = xlim, main = main, ylab = ylab, xlab = xlab, col = colramp, xaxs = "i", yaxs = "i", ...)
}

graficas <- function(pico, datos, datos_orig, datos_limpios, archivo, id, tdfi = tdfi, limite, auto = FALSE,  limpieza_fscssc, limpieza_ogr_a = NULL, limpieza_ogr_slope = NULL, ...) {
	
	###############################
	# Gráficas
	###############################
	
	layout(matrix(1:12, 4, 3, byrow = TRUE))
	parg <- list(cex.axis = 1.1, cex.lab = 1.3, mar = c(3.5, 4, 1, 0.5), mgp = c(2, 1, 0))
	par(parg)

######

	ssclin <- log10(datos_orig$SSC) * 256
	fsc <- datos_orig$FSC
	ssc <- datos_orig$SSC
	red <- datos_orig$Red
	green <- datos_orig$Green
	orange <- datos_orig$Orange
	fl2a <- datos_orig$FL2A
	fl2w <- datos_orig$FL2W
	ogr <- datos_orig$or_gr_ratio
	or_gr <- datos_orig$or_gr
	dfi <- datos_orig$dfi
	densidad <- density(dfi, na.rm = TRUE)
	densidad.ogr <- density(ogr, na.rm = TRUE)
	fscgreen <- fsc / green * multiplicador_fscgreen
		
##### 1 FSC/SSC

#	plot(x = datos_orig$FSC, y = datos_orig$SSC, main = paste("FSC/SSC ", id, sep = ""), xlab = "FSC", ylab = "SSC", ylim = c(1, limpieza_fscssc$ssc.high), xlim = c(1, limpieza_fscssc$fsc.high), log = "xy", pch = 16, cex = )

	cytogram(x = fsc, y = ssc, main = paste("FSC/SSC ", id, sep = ""), xlab = "FSC", ylab = "SSC", ylim = c(1, 10^4), xlim = c(1, 10^4), log = "xy")
	abline(v = limpieza_fscssc$fsc_low, lty = 2)
	abline(v = limpieza_fscssc$fsc_high, lty = 2)
	abline(h =  limpieza_fscssc$ssc_low, lty = 2)
	abline(h =  limpieza_fscssc$ssc_high, lty = 2)
	abline(v = limpieza_fscssc_orig$fsc_low, lty = 3)
	abline(v = limpieza_fscssc_orig$fsc_high, lty = 3)
	abline(h =  limpieza_fscssc_orig$ssc_low, lty = 3)
	abline(h =  limpieza_fscssc_orig$ssc_high, lty = 3)

##### 2 FL2-W vs. FL2-A sin limpiar

	cytogram(x = fl2w, y = fl2a, main = paste("FL2-A/FL2-W ", id, sep = ""), ylab = "FL2-A", xlab = "FL2-W", parg = parg)
	if(hacer_limpieza_origen) {
		if(limpieza_fl2w$b == 0) {
			abline(v = limpieza_fl2w$a, lty = 3)
		} else {
			abline(a = - limpieza_fl2w$a / limpieza_fl2w$b, b = 1 / limpieza_fl2w$b, lty = 3)
		}
		abline(a = limpieza_fl2a$a, b = limpieza_fl2a$b, lty = 3)
	}
#####limpieza_fscssc_orig

##### 3 SSC vs. FL2-A

#	cytogram(x = fl2w, y = fl2a, main = paste("FL2-A/FL2-W ", id, sep = ""), xlab = "FL2-A", ylab = "FL2-W", parg = parg)

	cytogram(x = ssclin, y = fl2a, main = paste("SSC/FL2-A ", id, sep = ""), xlab = "SSC", ylab = "FL2-A", parg = parg)
	if(hacer_limpieza_origen) {
#		abline(a = limpieza_sscfl2a$a, b = limpieza_sscfl2a$b, lty = 3)
	}
#####


##### 4 rojo vs. verde sin limpiar
	
	cytogram(x = red, y = green, main = paste("AO ", id, sep = ""), xlab = "red", ylab = "green", parg = parg)
	if(hacer_limpieza_origen) {
		abline(a = limpieza_origen$a, b = limpieza_origen$b, lty = 3)
	}
######

###### 5 total vs. DFI
	
	datosu <- data.frame(x = green + red, y = dfi, green = green, red = red)
	datosu <- datosu[rownames(unique(datosu[ , c("x", "y")])), ]
	colores <- rep("black", nrow(datosu))
	colores[datosu$green < limpieza_origen$a + datosu$red * limpieza_origen$b] <- "red"
	plot(datosu[ , c("x", "y")], main = paste("DFI ", id, sep = ""), xlab = "total", ylab = "dfi", xlim = c(0, 1023), ylim = c(0, 1023), pch = 16, cex = 0.2, col = colores)
	abline(h = pico, lty = "15")
	abline(h = pico + kCorte, lty = "18")
	abline(h = khDFICut, lty = "18")
	abline(v = 400, lty = 3)
	abline(a = -700, b = 3, lty = 3)

######
	
###### naranja vs. verde sin limpiar

	ssclin <- ssclin[green > limpieza_origen$a + limpieza_origen$b * red]
	fl2a <- fl2a[green > limpieza_origen$a + limpieza_origen$b * red]
	fl2w <- fl2w[green > limpieza_origen$a + limpieza_origen$b * red]

	fscgreen_old <- fscgreen
	green_old <- green
	fsc <- datos_limpios$FSC
	red <- datos_limpios$Red
	green <- datos_limpios$Green
	orange <- datos_limpios$Orange
	ogr <- datos_limpios$or_gr_ratio
	or_gr <- datos_limpios$or_gr
	dfi <- datos_limpios$dfi
	densidad <- density(dfi, na.rm = TRUE)
	ogr_density <- density(ogr, na.rm = TRUE)
	fscgreen <- fsc / green * multiplicador_fscgreen

##### 6 verde vs. naranja/verde limpio

	limites_x <- range(green)
	limites_y <- range(or_gr)
	limites_x <- c(0, 1024)
	limites_y <- c(0, 1)
	datosu <- unique(data.frame(x = green, y = or_gr))
	plot(datosu, main = paste("or/gr ", id, " limpio", sep = ""), xlab = "green", ylab = "orange / green", pch = 16, cex = 0.2, ylim = limites_y, xlim = limites_x)
	abline(a = limpieza_ogr_a, b = limpieza_ogr_slope, lty = "22")
	abline(h = picos_pos[1], lty = "15")
	abline(h = limite, lty = "22")
	
###### 7 FSC vs. FL1 sin limpiar

	datosu <- data.frame(x = fscgreen_old, y = green_old, red = datos_orig$Red)
	datosu <- datosu[rownames(unique(datosu[ , c("x", "y")])), ]
	colores <- rep("black", nrow(datosu))
	colores[datosu$y < limpieza_origen$a + datosu$red * limpieza_origen$b] <- "red"

	cytogram(x = datosu$x, y = datosu$y, main = paste("FSC/FL1-H ", id, " inicial", sep = ""), ylab = "Green", xlab = "FSC/FL1-H", parg = parg, col = colores)
	abline(a = limpieza_fscgreen$a, b = limpieza_fscgreen$b, lty = 3)

###### 8 FSC vs. FL1 limpio

	cytogram(x = fscgreen, y = green, main = paste("FSC/FL1-H ", id, " limpio", sep = ""), ylab = "Green", xlab = "FSC/FL1-H", parg = parg)
	abline(a = limpieza_fscgreen$a, b = limpieza_fscgreen$b, lty = 3)

#####

	fsc <- datos$FSC
	ssc <- datos$SSC
	ssclin <- log10(datos$SSC) * 256
	red <- datos$Red
	green <- datos$Green
	orange <- datos$Orange
	fl2a <- datos$FL2A
	ogr <- datos$or_gr_ratio
	or_gr <- datos$or_gr
	dfi <- datos$dfi
	densidad <- density(dfi, na.rm = TRUE)
	densidad_ogr <- density(ogr, na.rm = TRUE)

	fscgreen <- fsc / green * multiplicador_fscgreen
	fscred <- fsc / red * multiplicador_fscgreen
	fscorange <- fsc / orange * multiplicador_fscgreen

###### 9 FSC vs. FL1 filtrado

	cytogram(x = fscgreen, y = green, main = paste("FSC/FL1-H ", id, " filtrado", sep = ""), ylab = "Green", xlab = "FSC/FL1-H", parg = parg)
	abline(a = limpieza_fscgreen$a, b = limpieza_fscgreen$b, lty = 3)

	
###### 10 rojo vs. verde filtrado
	
	cytogram(x = red, y = green, main = paste("AO filtered ", id, sep = ""), xlab = "red", ylab = "green")
	abline(h = kHDSCut, lty = "18")
	abline(a = limpieza_origen$a, b = limpieza_origen$b, lty = 3)

######
	
###### 11 total vs. DFI filtrado

	datosu <- unique(data.frame(x = green + red, y = dfi))
	plot(datosu, main = paste("DFI filtered ", id, sep = ""), xlab = "total", ylab = "dfi", xlim = c(0, 1023), ylim = c(0, 1023), pch = 16, cex = 0.2)
	abline(h = pico, lty = "15")
	abline(h = pico + kCorte, lty = "18")
	abline(h = khDFICut, lty = "18")

######

###### 12 histograma DFI filtrado

#	limitesx <- c(0, max(densidad$x))
	limitesx <- c(0, 1000)
	plot(densidad, main = paste("DFI filtered ", id, sep = ""), xaxp = c(0, 1020, 20), ylab = "Density", xlab = "", xlim = limitesx)
	abline(v = pico, lty = "15")
	abline(v = pico + kCorte, lty = "18")
	abline(v = khDFICut, lty = "18")
	posiciony <- max(densidad$y) * 0.9
	text(pico - 150, posiciony, adj = 0, label = round(pico), cex = 1)
	text(1000, posiciony, adj = 1, label = paste("%DFI:", tdfi), cex = 1)
######

#	return(datos)

}


graficas.informe <- function(pico, datos, datos_orig, archivo, id, tdfi = tdfi,  scsa.plot = FALSE, ...) {
	fsc <- datos_orig$FSC
	ssc <- datos_orig$SSC
	red <- datos_orig$Red
	green <- datos_orig$Green
	orange <- datos_orig$Orange
	ogr <- datos$or_gr_ratio
	npanels <- 2
	if(scsa.plot) {
		layout(matrix(1:3, ncol = 3, byrow = TRUE))
	} else
	{
		layout(matrix(1:2, ncol = 2, nrow = 1, byrow = TRUE))
	}
	
	par(cex.axis = 1.1, cex.lab = 1.5, mar = c(1, 4.5, 1.5, 1.1), cex.axis = 1.2, oma=c(3.5,1,1,1))

	if(scsa.plot) {
		denscytogram(x = red, y = green, main = NULL, xlab = "Rojo (ADN Fragmentado)", ylab = "Verde (Fluorescencia Nativa del ADN)", ylim = c(0, 1023), xlim = c(0, 1023))
		segments(10, 1015, 1015, 1015)
		segments(10, 350, 10, 1015)
		segments(1015, 1015, 1015, 10)
		segments(150, 10, 1015, 10)
		segments(150, 10, 130, 100)
		segments(130, 100, 100, 200)
		segments(100, 200, 60, 280)
		segments(60, 280, 10, 350)
		
		segments(10, kHDSCut, 1010, kHDSCut, lty = 2, lwd = 2) #HDS
		segments(105, 195, 200, kHDSCut, lty = 2, lwd = 2) #COMP
	}
	dfi <- datos$dfi
	red <- datos$Red
	green <- datos$Green	

	denscytogram(x = dfi, y = (green + red), main = NULL, ylab = "Fluorescencia Total", xlab = "", ylim = c(0, 2000), xlim = c(0, 100), xaxp = c(0, 100, 4))
	abline(v = pico + 5, lty = 2, lwd = 2)
	abline(v = khDFICut, lty = 2, lwd = 2)

	nclases <- nclass.scott(dfi)
	if(nclases < 100) nclases <- 100
	histograma <- hist(dfi, breaks = nclases, plot = FALSE)
	plot(histograma$breaks, c(0, histograma$counts), col = "darkblue", ylim = c(0, max(histograma$counts)), xlim = c(0, 100), type = "l", ylab = "Espermatozoides", xlab = "", xaxs = "i", yaxs = "i", xaxp = c(0, 100, 4), lwd = 2, lend = 2)
	abline(v = 25, lty = 2, lwd = 2)
	abline(v = khDFICut, lty = 2, lwd = 2)
	
	mtext("Fragmentación del ADN", side = 1, line = 2, adj = 0.5, cex = 1.5, outer = TRUE)
	
}

