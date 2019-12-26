# 201807

#####################
#bibliotecas

library(flowCore)
library(flowViz)
library(flowStats)
library(flowWorkspace)
library(flowClust)
library(openCyto)
library(gridExtra)
library(latticeExtra)
library(xlsx)
options(warn = 0)
library(dichromat)


#####################
# general

flowViz.par.set("gate", list(lwd = 0.6, col = "#FF0000"))
transfmethod <- new("transform", .Data = function(x) x <- log(x + 1, 10))
transfmethod@transformationId <- "LogTransform"
logiclet <- logicleTransform()
lattice.options(
  layout.heights = list(bottom.padding = list(x = 0), top.padding = list(x = 0.5), main = list(x = 0), strip = list(x = 0), axis.xlab.padding = list(x = 0), axis.panel = list(x = 0), between = list(x = 0), axis.top = list(x = 0), sub = list(x = 0), xlab.top = list(x = 0), xlab = list(x = 0), axis.bottom = list(x = 0)),
  layout.widths = list(left.padding = list(x = 1), right.padding = list(x = 0), axis.ylab.padding = list(x = 0)),
  axis.units = list(outer = list(top = list(pad1 = list(x = 0), pad2 = list(x = 0)), right = list(pad1 = list(x = 0), pad2 = list(x = 0)), bottom = list(pad1 = list(x = 1), pad2 = list(x = 2)), left = list(pad1 = list(x = 1), pad2 = list(x = 1))))
)
flowViz.par.set(list(panel.background = list(col = "gray99"), reference.line = list(col = "transparent")))

nombres <- c(low = "low", med = "med", high = "high")

#####################
# funciones


crear.directorios <- function(ruta.general, dirtrabajo) {
	directorio.analisis <- paste(ruta.general, dirtrabajo, "/", dirtrabajo, sep = "")
	dir.graficas <- paste(directorio.analisis, " cytograms", sep = "")
	if(!file.exists(dir.graficas)) dir.create(dir.graficas)
}


find.peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
       z <- i - m + 1
       z <- ifelse(z > 0, z, 1)
       w <- i + m + 1
       w <- ifelse(w < length(x), w, length(x))
       if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
     pks <- unlist(pks)
     pks
}
