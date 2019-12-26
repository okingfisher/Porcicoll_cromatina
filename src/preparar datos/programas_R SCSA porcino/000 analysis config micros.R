#Copiar a la carpeta del análisis y modificar lo que sea necesario
#Configuración para FACSCalibur Microscopía
kcorte <- 250 #según estándar/controles
kpico <- kcorte - 50
hacer_limpieza_ogr <- 1 #Cuando esté muy sucio y no se quite con la limpieza del origen; usar con precaución.
hacer_limpieza_origen <- 1
hacer_limpieza_fscgreen <- 1
especial_limpieza <- NULL

prefijo <- "SCSA" #¿qué pusiste al principio del nombre de los archivos?

limpieza_fscssc <- list(fsc_low = 8, fsc_high = 100, ssc_low = 20, ssc_high = 1.5*10^3)
limpieza_sscfl2a <- list(a = -1400, b = 2.8)
limpieza_fl2a <- list(a = 50, b = 0)
corte_ssc <- 20

limpieza_origen <- list(a = 380, b = -1.5)
limpieza_fl2w <- list(a = -1, b = 0)
limpieza_fscgreen <- list(a = -400, b = 4)
limpieza_ogr <- list(a = 0.2, b = 0.0005)
limpieza_ogr_lista <- ""
ogr_lim <- list(llim = NA, hlim = NA)
limpieza_y <- 100
multiplicador_fscgreen <- 1000
ajuste_red <- 0
ajuste_green <- 0
modif_limpieza_fscssc_orig <- list(fsc_low = 0, fsc_high = 40, ssc_low = 0, ssc_high = 0)

nombre_fsc <- "FSC-H"
nombre_ssc <- "SSC-H"
nombre_verde <- "FL1-H"
nombre_rojo <- "FL3-H"
nombre_naranja <- "FL2-H"
nombre_FL2A <- "FL2-A"
nombre_FL2W <- "FL2-W"

comentarios <- ""

#Antes de limpiar y de generar DFI
preprocessing <- function(datos, id, contador)
{
	datos <- subset(datos, SSC > corte_ssc)
	return(datos)
}

#Después de limpiar y con DFI
postprocessing <- function(datos, archivo, directorio, contador, id)
{
	datos$Red <- datos$Red + 0
	datos$Green <- datos$Green + 0
	return(datos)
}