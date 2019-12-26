pausas <- 1
if (pausas) {
	guardar <- 0
} else {
	guardar <- 1
}

library(ggplot2)

setwd("~/Documents/Trabajo/Proyectos/Topigs análisis porcino/proyecto Porcicoll-cromatina Topigs/experimento")

load("datos/datos procesados/Porcicoll cromatina datos combinados.Rdata")
datos <- within(datos, {
	boar <- factor(boar)
	treatm <- sub("-", "", treatm)
	treatm <- factor(treatm)
	expday <- factor(expday)
})

parorig <- par()

for (variable in c("finalconc", "mot", "prog")) {
	datos$resp <- datos[ , variable]
	if (guardar)
		png(filename = paste("plots/Porcicoll_boxplot_", variable, ".png", sep = ""))
	par(mfrow = c(1, 2))
	boxplot(resp ~ treatm, datos[datos$expday == 0, ], main = paste("expday 0", variable), ylim = c(0, 100))
	boxplot(resp ~ treatm, datos[datos$expday == 3, ], main = paste("expday 3", variable), ylim = c(0, 100))
	if (guardar)
		dev.off()
	g <- ggplot(datos, aes(x = expday, y = resp, group = boar)) +
		geom_point(aes(col = boar, shape = boar)) +
		geom_line(aes(col = boar)) + ylab (ifelse(grepl("conc", variable), "mill./ml", "%")) + ggtitle(variable) +
		facet_wrap( ~ treatm) +
		scale_shape_manual(values = c(16:25, 0:15))
	if (guardar)
	{
		ggsave(filename = paste("plots/Porcicoll_", variable, ".png", sep = ""))
	} else {
		plot(g)
	}
	if (pausas)
		readline("#")
}
par <- parorig

datosw <- data.frame(recast(datos[ , c("expday", "treatm", "boar", "finalconc", "finalvol")], boar + expday ~ treatm + variable))

datosw <- within(datosw, {
	Control_SLC1_finalvol <- 0.5
	Control_SLC4_finalvol <- 4.5
	Control_SLC1_spz <- Control_SLC1_finalvol * Control_finalconc
	Control_SLC4_spz <- Control_SLC4_finalvol * Control_finalconc
	SLC1_spz <- SLC1_finalvol * SLC1_finalconc
	SLC4_spz <- SLC4_finalvol * SLC4_finalconc
	SLC1_recovery <- SLC1_spz / Control_SLC1_spz * 100
	SLC4_recovery <- SLC4_spz / Control_SLC4_spz * 100
	expday <- factor(expday)
	boar <- factor(boar)
})

datosm <- data.frame(melt(datosw[ , c("boar", "expday", "SLC1_recovery", "SLC4_recovery")], idvar = c("boar", "expday")))
names(datosm)[3:4] <- c("treatm", "recovery")
datosm <- within(datosm, {
	treatm <- factor(sub("_recovery", "", treatm))
})

variable <- "recovery"
parorig <- par()

if (guardar)
	png(filename = paste("plots/Porcicoll_boxplot_", variable, ".png", sep = ""))
par(mfrow = c(1, 2))
boxplot(recovery ~ treatm, datosm[datosm$expday == 0, ], main = paste("expday 0", variable), ylim = c(0, 110))
boxplot(recovery ~ treatm, datosm[datosm$expday == 3, ], main = paste("expday 3", variable), ylim = c(0, 110))
if (guardar)
	dev.off()
g <- ggplot(datosm, aes(x = expday, y = recovery, group = boar)) +
	geom_point(aes(col = boar, shape = boar)) +
	geom_line(aes(col = boar)) +
	ylab ("%") + ggtitle("recovery") +
	facet_wrap( ~ treatm) +
	scale_shape_manual(values = c(16:25, 0:15))
if (guardar) {
	ggsave(filename = paste("plots/Porcicoll_", variable, ".png", sep = ""))
} else
{
	plot(g)
}
if (pausas)
	readline("#")
par <- parorig


