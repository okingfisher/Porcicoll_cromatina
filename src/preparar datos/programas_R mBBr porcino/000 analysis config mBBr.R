#Copiar a la carpeta del análisis y modificar lo que sea necesario

# Nombres de los parámetros

tiempo <- "HDR-T"
fsch <- "FSC-H"
fsca <- "FSC-A"
ssch <- "SSC-H"
ssca <- "SSC-A"
mbbrblueh <- "mBBrBlue-H"
mbbrbluea <- "mBBrBlue-A"
pih <- "PI-H"
pia <- "PI-A"


limmin_mbbr <- 0.4
limmin_mbbra <- 0.5
limmax_mbbr <- 2.5
limmax_mbbra <- 2.5
tolerancia_mbbr <- 0.2
tolerancia_mbbra <- 0.2
ajuste_fluombbr <- 0
corte_lowmbbr <- 0.4
ajuste_fluombbra <- 0
corte_lowmbbra <- 0.5
tolerancia_pimin <- 0.3
tolerancia_pihigh <- 0.25
corte_lowpi <- 0.5

limpieza_fscssc <- c(fsc_low = 1.4, fsc_high = 1.9, ssc_low = 0.5, ssc_high = 2.6)

pico_override <- FALSE
picombbr <- 1
picombbra <- 1

comentarios <- ""

preprocessing <- function(datos, id, contador) {


	return(datos)
}