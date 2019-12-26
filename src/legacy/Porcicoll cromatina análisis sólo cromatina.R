pausas <- 0
twoplots <- 1
if (pausas) {
	guardar_resultados <- 0
} else {
	guardar_resultados <- 1
}
proyecto <- "Porcicoll"
experimento <- "cromatina"
dirplots <- "resultados ICAR/plots/"
dirresultados <- "resultados ICAR/"

library(ggplot2)
library(lme4)
library(car)
library(multcomp)
library(Rmisc)

setwd("~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento")

load(paste("datos/datos procesados/", proyecto, " ", experimento, " sólo cromatina.Rdata", sep = ""))

names(datos)[1:4] <- c("boar", "weekn", "treatm", "expday")

datos <- within(datos, {
	boarw <- factor(paste(boar, weekn))
	boar <- factor(boar)
	treatm <- factor(treatm)
	weekn[weekn == 4] <- 3 # Metemos al boar de la última weekn en la 3
	weekn <- factor(weekn)
	expday <- factor(expday)
})

variables_scsa <- c("sddfi", "tdfi", "hds")
variables_mbbr <- c("disulphide_mean", "disulphide_median","disulphide_mode", "lowmbbr_p_F", "medmbbr_p_F", "highmbbr_p_F")
variables_cma3 <- c("lowcma3_p", "medcma3_p", "highcma3_p", "meancma3", "mediancma3", "picocma3")
variables <- c(variables_scsa, variables_mbbr, variables_cma3)

# Quitamos posible outlier de CMA3:
datos <- within(datos, {
	lowcma3_p[lowcma3_p > 50] <- NA
	medcma3_p[medcma3_p < 50] <- NA
	highcma3_p[highcma3_p < 1] <- NA
	picocma3[picocma3 > 1.5] <- NA
})

nsink <- sink.number()
if(nsink > 0) for(i in 1:nsink) sink()
if(guardar_resultados)
		sink(file = paste(dirresultados, proyecto, " ", experimento, " análisis cromatina sólo.txt", sep = ""), split = TRUE)

título("Chromatin analysis of the Porcicoll chromatin experiment, for ICAR 2020")
pd <- position_dodge(width = 0.3)
#print(aggregate(cbind(mot, prog) ~ treatm, function)
for (variable in variables) {
	datos$resp <- datos[ , variable]
	if(grepl("_p", variable) | variable %in% c("tdfi", "hds")) {
		datos$respt <- asin(sqrt(datos$resp / 100))
		theylabel <- "%"
	} else {
		datos$respt <- log1p(datos$resp)
		theylabel <- ""
	}
	título(variable)
	ds <- summarySEwithin(datos, measurevar = "resp", withinvar = c("expday", "treatm"), idvar = c("boar", "weekn"), na.rm = TRUE)
	print(ds)
	cat("\n")
	dsd <- summarySEwithin(datos, measurevar = "resp", withinvar = c("expday"), idvar = c("boar", "weekn", "treatm"), na.rm = TRUE)
	print(dsd)
	cat("\n")
	dst <- summarySEwithin(datos, measurevar = "resp", withinvar = c("treatm"), idvar = c("boar", "weekn", "expday"), na.rm = TRUE)
	print(dst)
	cat("\n")

	análisis <- lmer(respt ~ treatm * expday + (1 | boar / weekn), data = datos, na.action = na.omit)
	print(round(summary(análisis)$coefficients, 4))
	cat("\n")
	print(myaov <- Anova(análisis))
	cat("\n")
	if (myaov[3, 3] > 0.05) {
		if (myaov[1, 3] < 0.05) {
			subtítulo("Treatment comparison")
			análisis <- lmer(respt ~ treatm + (1 | boar / weekn / expday), data = datos, na.action = na.omit)
			print(round(summary(análisis)$coefficients, 4))
			cat("\n")
			print(myaov <- Anova(análisis))
			cat("\n")		
			print(summary(glht(análisis, linfct = mcp(treatm = "Tukey")), test = adjusted("holm")))
		}
	} else {
		subtítulo(paste(variable, "Day 0"))
		análisis <- lmer(respt ~ treatm + (1 | boar / weekn), data = datos[datos$expday == "0", ], na.action = na.omit)
		print(round(summary(análisis)$coefficients, 4))
		cat("\n")
		print(Anova(análisis))	
		cat("\n")
		subtítulo(paste(variable, "Day 3"))
		análisis <- lmer(respt ~ treatm + (1 | boar / weekn), data = datos[datos$expday == "3", ], na.action = na.omit)
		print(round(summary(análisis)$coefficients, 4))
		cat("\n")
		print(Anova(análisis))
	}

	g <- ggplot(datos, aes(x = treatm, y = resp)) +
		geom_boxplot(aes(col = treatm)) +
		facet_wrap( ~ expday) +
		ylab(theylabel) + xlab("") + ggtitle(variable)
	if (guardar_resultados) {
		ggsave(filename = paste(dirplots, proyecto, " ", experimento, " ", variable, " boxplot.png", sep = ""))
	} else {
		plot(g)
	}

	if (twoplots) {
		g <- ggplot(datos, aes(x = treatm, y = resp, group = interaction(boar, weekn))) +
			geom_point(aes(col = boar, shape = weekn), position = pd) +
			geom_line(aes(col = boar), alpha = 0.3, position = pd) +
			facet_wrap( ~ expday) +
			ylab(theylabel) + xlab("") + ggtitle(variable)
		if (guardar_resultados) {
			ggsave(filename = paste(dirplots, proyecto, " ", experimento, " ", variable, " boarplot.png", sep = ""))
		} else {
			plot(g)
		}
	}
	if (pausas)
		readline("#")
}

if(guardar_resultados) sink()