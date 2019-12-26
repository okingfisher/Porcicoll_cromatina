
save2 <- function(datos, filename)
{
	save(datos, file = paste(filename, "Rdata", sep = "."))
	write.csv2(datos, file = paste(filename, "csv", sep = "."), row.names = FALSE, na = "")
	return(1)
}

arreglar.datos <- function(archivo) {
	nombre <- load(archivo)
	datos <- get(nombre)
	return(datos)
}

ajustar.dec <- function(x, dec = 1)
{
	if (!is.numeric(x))
		stop("Error en ajustar.dec, x no es un número")
	x <- round(x, dec) # por si acaso
	x <- as.character(x)
	lx <- nchar(x)
	decs <- unlist(strsplit(x, "\\."))[2]
	if (is.na(decs))
	{
		x <- paste(x, ".", paste(rep(0, dec), collapse = ""), sep = "")
	} else if (nchar(decs) < dec)
	{
		x <- paste(x, rep(0, dec - nchar(decs)), sep = "") 
	}
	return(x)
}
stats.tables <- function(x, dec) 
{

	mediana <- round(median(x, na.rm = TRUE), dec)
	mediana <- ajustar.dec(mediana, dec)
	madev <- round(mad(x, na.rm = TRUE), dec)
	madev <- ajustar.dec(madev, dec)
	return(paste(mediana, "±", madev, sep = ""))
}

hacer.tabla.word <- function(datos, variables)
{
	require(officer)
	require(flextable)
	latabla <- data.frame(Day = rep(c(0:4, 7), each = 3), Type = rep(c("CTL", "SLC", "SLCm"), 6))
	for (variable in variables)
	{
		mediasem <- summarySEwithin(datos, measurevar = variable, withinvar = c("Day", "Type"), idvar = "verraco", na.rm = TRUE)
		mediasem[ , variable] <- paste(round(mediasem[ , variable], 1), "±", round(mediasem$se, 1), sep = "")
		mediasem <- mediasem[ , c("Day", "Type", variable)]
		latabla<- merge(latabla, mediasem, by = c("Day", "Type"), sort = FALSE)
	}
	latabla$Day <- as.character(latabla$Day)
	latabla$Type <- as.character(latabla$Type)
	latabla$Day[-c(seq(1, length(latabla$Day), 3))] <- ""
	ft <- flextable(latabla)
	#ft <- autofit(ft)
	ft <- border_remove(ft)
	borde <- fp_border(color = "black", style = "solid", width = 1)
	ft <- hline_top(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "body")
	ft <- align(ft, i = NULL, j = NULL, align = "center", part = "all")
	return(ft)
}

hacer.tablacor.word <- function(latabla, heading = NULL)
{
	require(officer)
	require(flextable)
	for (fila in 1:(nrow(latabla) - 1))
	{
		latabla[fila, ] <- c(unlist(latabla[fila, 1:(fila + 1)]), rep("", ncol(latabla) - fila - 1))
	}
	
	if (!is.null(heading))
		names(latabla) <- heading
		
	ft <- flextable(latabla)
	#ft <- autofit(ft)
	ft <- border_remove(ft)
	borde <- fp_border(color = "black", style = "solid", width = 1)
	ft <- hline_top(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "body")
	ft <- align(ft, i = NULL, j = NULL, align = "center", part = "all")
	return(ft)
}

hacer.tablasimple.word <- function(latabla, heading = NULL)
{
	require(officer)
	require(flextable)
	if (!is.null(heading))
		names(latabla) <- heading
	
	ft <- flextable(latabla)
	#ft <- autofit(ft)
	ft <- border_remove(ft)
	borde <- fp_border(color = "black", style = "solid", width = 1)
	ft <- hline_top(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "header")
	ft <- hline_bottom(ft, j = NULL, border = borde, part = "body")
	ft <- align(ft, i = NULL, j = NULL, align = "center", part = "all")
	return(ft)
}


aov.sig <- function(theaov){
	if (class(theaov)[1] == "anova.lme")
	{
		theaov$sig <- ""
		for (fila in 2:nrow(theaov))
		{
			theaov$sig[fila] <- ifelse(theaov$"p-value"[fila] < 0.001, "***", ifelse(theaov$"p-value"[fila] < 0.01, "**", ifelse(theaov$"p-value"[fila] < 0.05, "*", "")))
		}
	} else {
		stop("Error de aov.sig, la entrada no es anova.lme")
	}
	return(theaov)
}


lmetable <- function(analisis) print(round(summary(analisis)$tTable, 4))

descriptiva <- function(datos, factores, idvar = NULL, variable = "resp", tipo = "SEwithin", mostrar = TRUE)
{
	if (tipo == "SEwithin")
	{
		require(Rmisc)
		expresión <- paste("summarySEwithin(datos, measurevar = '", variable, "', withinvars = c('", paste(factores, collapse = "', '"), "'), idvar = c('", paste(idvar, collapse = "', '"), "'), na.rm = TRUE)", sep = "")
	} else
	{
		require(plyr)
		require(sciplot)
		expresión <- paste("ddply(datos, .(", paste(factores, collapse = " ,"), "),  summarize, n = length(", variable, "), media = mean(", variable, ", na.rm = TRUE), se = se(", variable, ", na.rm = TRUE), sd = sd(", variable, ", na.rm = TRUE))", sep = "")
	}
	ds <- eval(parse(text = expresión))	
	if (tipo != "SEwithin")
		names(ds)[3] <- variable
	if (mostrar)
	{
		print(ds)
		cat("\n")
	}
	return(ds)
}

título <- function(título)
{
	ltit <- nchar(título)
	cat("\n", rep("#", ltit + 6), "\n", sep = "")
	cat("#  ", rep(" ", ltit), "  #\n", sep = "")
	cat("#  ", título, "  #\n", sep = "")
	cat("#  ", rep(" ", ltit), "  #\n", sep = "")
	cat(rep("#", ltit + 6), "\n\n", sep = "")	
}

subtítulo <- function(subtítulo)
{
	ltit <- nchar(subtítulo)
	cat("\n", rep("-", ltit + 2), "\n", sep = "")
	cat(" ", subtítulo, "\n", sep = "")
	cat(rep("-", ltit + 2), "\n\n", sep = "")	
}

descripción <- function(descripción)
{
	cat(descripción, "\n\n", sep = "")
}

transformar <- function(datos, variable, transf = TRUE) {
	if (transf) {
		if (variable %in% c("vap", "vcl", "vsl", "alh", "bcf", "dnc", "dncm", "cfmediana", "medianmbbr.C", "medianmbbr.D", "medianmbbrA.C", "medianmbbrA.D", "freethiols_median", "freethiols_mean", "freethiols_medianA", "freethiols_meanA", "fi.1", "fi.2") | grepl("vap|vcl|vsl|alh|bcf|dnc|dncm", variable) | grepl("\\..+r$", variable) | grepl("\\..+d$", variable)) {
			# Si hay números negativos, movemos la serie al rango positivo.
			if (min(datos[ , variable], na.rm = TRUE) < 0) {
				b <- -min(datos[ , variable], na.rm = TRUE)
			} else {
				b <- 0
			}
			resp <- log1p(datos[ , variable] + b)
		} else if (max(datos[ , variable], na.rm = TRUE) > 100 | min(datos[ , variable], na.rm = TRUE) < 0) {
			stop(paste("La variable", variable, "debería ser un porcentaje, pero incluye valores fuera del rango [0, 100]"))
		} else {
			resp <- asin(sqrt(datos[ , variable] / 100))
		}
	} else {
		resp <- datos[ , variable]
	}
	return(resp)
}

myglm <- function(formula, datos, showmodel = TRUE)
{
	analisis <- glm(formula, data = datos)
	if (showmodel)
	{
		print(summary(analisis))
	}
	print(summary(aov(analisis)))
	cat("\n")
	return(analisis)
}


análisis.brzp2016 <- function(datos, variable, factores, sujetos)
{
	# Factores y covariables:
	# toro, estación/fecha/época, edad, diluyente, calidad precong.: vol., masal, conc.
	# Análisis diluyente
	
	require(nlme)
	require(lme4)
	require(Rmisc)
	require(MuMIn) # r.squaredGLMM
	require(AICcmodavg) # aictab	
	
	ds <- summarySEwithin(datos, variable, withinvars = factores, idvar = sujetos, na.rm = TRUE)
	print(ds)
	variablet <- paste(variable, "t", sep = "")
	datos$respt <- transformar(datos, variable)	
	names(datos)[names(datos) == "respt"] <- variablet
	analisis <- NULL
	cat("\nANOVA\n")
	modelo_aov <- as.formula(paste(variable, "~", paste(sujetos, collapse = "+"), "+", paste(factores, collapse = "+"), sep = ""))
	analisis <- aov(modelo_aov, data = datosh)
	cat("\n")
	print(myaov <- summary(analisis))
	cat("\n")
	modelo_aov <- as.formula(paste(variable, "~", paste(factores, collapse = "+"), "+ Error(", paste(sujetos, collapse = "/"), ")", sep = ""))
	analisis <- aov(modelo_aov, data = datos)
	cat("\n")
	print(myaov <- summary(analisis))
	cat("\n")
	cat("\nLME\n")
	modelo_fijo <- as.formula(paste(variable, "~", paste(factores, collapse = "+"), sep = ""))
	modelo_aleat <- as.formula(paste("~ 1 | ", paste(sujetos, collapse = "/"), sep = ""))
	analisis <- lme(fixed = modelo_fijo, random = modelo_aleat, data = datos, na.action = na.omit)
	print(round(summary(analisis)$tTable, 4))
	cat("\n")
	# You can use MuMIn package and its r.squaredGLMM() function which will give you 2 approximated r-squared values based on Nakagawa & Schielzeth (2012) and Johnson (2014):
# Marginal R^2 is the proportion of variance explained by the fixed effects alone.
# Conditional R^2 is the proportion of variance explained by the fixed and random effects jointly.
#	rsquared <- r.squaredGLMM(analisis)
#	cat("Varianza explicada por efectos fijos:\t\t", round(rsquared[1] * 100, 1), "\n")
#	cat("Varianza explicada por efectos aleatorios:\t", round((rsquared[2] - rsquared[1]) * 100, 1), "\n")
	cat("\n")
	myaov <- anova(analisis)
	myaov <- aov.sig(myaov)
	print(myaov)
	for (fila in 2:nrow(myaov))
	{
		if (myaov[fila, 4] < 0.05 & is.factor(factores[fila - 1]))
		{
			if (nlevels(factores[fila - 1]) > 2)
				Tukey.glht(analisis, factores)
		}
	}
	modelo0 <- as.formula(paste(variable, "~ 1 + (1 | ", paste(sujetos, collapse = "/"), ")", sep = ""))	
	modelo_full <- as.formula(paste(variable, "~", paste(factores, collapse = "+"), "+ (1 | ", paste(sujetos, collapse = "/"), ")",  sep = ""))
	mod0 <- lmer(modelo0, data = datos, REML = FALSE, na.action = na.omit)
	mod1 <- lmer(modelo_full, data = datos, REML = FALSE, na.action = na.omit)
	Cand_m <- list(mod0, mod1)
	Cand_names <- c("nulo", "estación")
	B <- aictab(cand.set = Cand_m, Cand_names, sort = TRUE)
	print(B)		
	cat("\n")
}

transformar.estaciones <- function(estaciones) {
	estaciones[grepl("^P", ignore.case = TRUE, estaciones)] <- "primavera"
	estaciones[grepl("^V", ignore.case = TRUE, estaciones)] <- "verano"
	estaciones[grepl("^O", ignore.case = TRUE, estaciones)] <- "otoño"
	estaciones[grepl("^I", ignore.case = TRUE, estaciones)] <- "invierno"
	return(estaciones)
}

Tukey.glht <- function(analisis, expl, nombrearch = NULL, mostrar.resultado = TRUE, guardar.plots = FALSE, show.plots = FALSE) {
	require(multcomp)
	cat("\n")
	pairwcomp <- summary(glht(analisis, linfct = eval(parse(text = paste("mcp(", expl, " = 'Tukey')", sep = "")))))
	if(mostrar.resultado) {
		cat( "\nPairwise comparison (adjusted P-values) for ", expl, "\n", sep = "")
		print(pairwcomp)
	}
	if(guardar.plots) {
		pdf(file = paste(resultados, "Tukey/", prefijo, experimento, "-Tukey ", nombrearch, ".pdf", sep = ""), width = 4, height = 3)
		plot(pairwcomp, las = 1, sub = nombrearch)
		dev.off()
	}
	if(show.plots) plot(pairwcomp, las = 1, sub = nombrearch)
}

mysink <- function(ruta, prefijo, guardar) {
	nsink <- sink.number()
	if(nsink > 0) for(i in 1:nsink) sink()
	if(guardar)
		sink(file = paste(ruta, "/", prefijo, if(!transf) " sin transf", ".txt", sep = ""), split = TRUE)
}

mytheme_bw <- function(base_size = 12, ...) {
	theme_bw(base_size = base_size) %+replace%
  theme(
    axis.line =         element_blank(),
    axis.text.x =       element_text(size = base_size * 0.8, lineheight = 0.7, vjust = 1.2),
    axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.7, hjust = 1),
    axis.ticks =        element_line(colour = "black", size = 0.2),
    axis.title.x =      element_text(size = base_size * 0.9, vjust = 0.2),
    axis.title.y =      element_text(size = base_size * 0.9, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    
    legend.background = element_rect(colour=NA), 
    legend.key =        element_rect(colour = "grey80"),
    legend.key.size =   unit(1.5, "lines"),
    legend.text =       element_text(size = base_size * 0.9),
    legend.title =      element_text(size = base_size * 1, face = "bold", hjust = 0),
    legend.position =   "right",
    legend.text.align =		0,
	legend.margin =			unit(0, "lines"),

    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = NA, size = 0.2),
    panel.grid.minor =  element_line(colour = NA, size = 0.5),
    panel.margin =      unit(0, "lines"),

    strip.background =  element_rect(fill = "grey90", colour = "grey50"), 
    strip.text.x =      element_text(size = base_size * 0.9, vjust = 0.5),
    strip.text.y =      element_text(size = base_size * 0.9, angle = -90),

    plot.background =   element_rect(colour = NA),
    plot.title =        element_text(size = base_size * 0.9, vjust = 1.2),
    plot.margin =       unit(c(0.4, 0.2, 0.4, 0.5), "lines")
	)
}

cbbPalette <- c("#666666", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot.meansem <- function(ds, x = NULL, group = NULL, facets = NULL, title = "", target = c("paper", "supplementary"), append.name = "", tipo = tipo, limitesy = NULL, xlabel = "", ylabel = "%", path, ancho = 4, alto = 4, letragen = 12, do.legend = TRUE, show.sigs = FALSE, guardar.resultados = FALSE, analisis, dodging = 0.5, prefijo = "", experimento = "", variable  = "", rawdata = NULL, sigs = NULL, tipo.imagen = c("pdf", "png"), ...) {
	require(ggplot2)
	require(grid)

	ds$x <- ds[ , x]
	ds$group <- ds[, group]
	if(!is.null(facets)) {
		ds$facets <- ds[ , facets]
	}
	if(!is.null(limitesy)) {
		ymin <- limitesy[1]
		ymax <- limitesy[2]
		if(ymin > min(ds$media, na.rm = TRUE)) {
			ymin <- min(ds$media - ds$sem, na.rm = TRUE)
		}
		if(ymax < max(ds$media, na.rm = TRUE)) {
			ymax <- max(ds$media + ds$sem, na.rm = TRUE)
		}
	} else {
		ymin <- min(ds$media - ds$sem, na.rm = TRUE)
		ymax <- max(ds$media + ds$sem, na.rm = TRUE)
	}
	ds$rango <- ymax - ymin

	g <- ggplot(data = ds, aes(x = x, y = media, group = group, ymax = max(media * 1.1, na.rm = TRUE))) +
		xlab(xlabel) + ylab(ylabel)

	tamletra <- letragen

	pd <- position_dodge(dodging)
	if(target == "paper") {
		g <- g + 
			geom_line(aes(linetype = group, col = group), position = pd) +
			geom_errorbar(aes(ymin = media - sem, ymax = media + sem), colour = "gray60", width = 0.1, position = pd) +
			geom_point(aes(shape = group, col = group), position = pd) +
			mytheme_bw(letragen) +
			scale_y_continuous(limits = c(ymin, ymax))   # Set y range
#			scale_colour_brewer(name = "hola", type = "qual", palette = 4, breaks = c("Control", "SuinFort"))

		if(!do.legend) {
			g <- g + theme(legend.position="none")
		} else {
			g <- g + scale_colour_discrete(name = "") +
				scale_linetype_discrete(name = "") +
				scale_shape_discrete(name = "")
		}
	} else if(target == "supplementary") {
		g <- g + 
			geom_line(aes(linetype = group, col = group), position = pd) +
			geom_errorbar(aes(ymin = media - sem, ymax = media + sem), colour = "gray60", width = 0.1, position = pd) +
			geom_point(aes(shape = group, col = group), position = pd) +
			theme_grey(base_size = letragen) +
			scale_y_continuous(limits = c(ymin, ymax)) +  # Set y range
			scale_colour_brewer(name = "", type = "qual", palette = 3)
		if(!do.legend) {
			g <- g +	theme(legend.position="none")
		}
	}
	
	if(show.sigs) {
		g <- g + geom_text(aes(x = x, y = media + sem + rango * 0.03, group = group, label = sig), position = pd, parse = TRUE, size = letragen * 0.25)
	}

	if(title != "") {
		g <- g + theme(plot.margin = unit(c(0.4, 0.2, 0.3, 0.3), "lines")) + labs(title = title)
	}
							
	if(!is.null(facets)) {
		g <- g + facet_grid(. ~ facets)
		ancho <- length(unique(ds$facets)) * 2
		alto <- 4
	}
	if(tipo.imagen != "pdf") {
		ancho <- ancho * 150
		alto <- alto * 150
		res <- 150
	}
	
	if(guardar.resultados) {
		if(target == "paper") {
			filename <- paste(prefijo, "-", experimento,"-", variable, "-", analisis, if(nchar(append.name) > 0) paste("-", append.name, sep = "") else "", sep = "")
		} else {
			filename <- paste(prefijo, "-", experimento, "-", variable,"-", analisis, if(nchar(append.name) > 0) paste("-", append.name, sep = "") else "", sep = "")
		}
		filename <- paste(path, "/", gsub("[ \\.]", "_", filename), ".", tipo.imagen, sep = "")
		save.plot(dibujo = g, tipo = tipo.imagen, file = filename, width = ancho, height = alto, res = 150)	
	} else {
		plot(g)
	}
}

save.plot <- function(dibujo, tipo = "pdf", file, width = 5, height = 5, res = 150, ...) {
	if(tipo == "pdf") {
		pdf(file = file, width = width, height = height, ...)	
	} else if (tipo == "png") {
		png(filename = file, width = width, height = height, res = res, ...)	
	}
	plot(dibujo)
	#Override clipping
	#Para escribir fuera del área del gráfico
	gt <- ggplot_gtable(ggplot_build(dibujo))
	gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.draw(gt)
	dev.off()
}
