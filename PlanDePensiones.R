# Especificar el directorio de trabajo.
setwd("/Users/bmond/Desktop/Plan de Pensiones")

# Base de datos de empleados.
baseEmpleados = read.csv("./BaseEmpleados.csv", header = TRUE)

# Hipotesis actuariales.

# Table de decrementos unicos acosiados.
unicosAsociados = read.csv("./TablaDecrementosUnicosAsociados.csv", header = TRUE)

# Tasa nomial de descuento
tasaNominalDeDescuento <- 0.075

# Factor nominal de descuento
v <- 1/(1+tasaNominalDeDescuento)

# Tasa de incremento salarial
tasaIncrementoSalarial <- 0.05

# Hipotesis contables

# Edades de retiro.
edadesRetiro <- c(60,61,62,63,64,65)

# Dias de aguinaldo
diasDeAguinaldo <- 38

#' Calcular el valor presente actuarial de las obligaciones totales.
#'
#' @param edadVluacion Edad de la persona a la fecha de evaluación.
#' @param sueldoBaseMensual El sueldo de la persona en la fecha de valución.
#' @return El VPOT de una persona de edad \code{edadValuacion} con sueldo base mensual de \code{sueldoBaseMensual}.
#' @examples
#' getVPOT(40, 30000)
#' getVPOT(55, 45000)
getVPOT <- function(edadValuacion, sueldoBaseMensual) {
  sum <- 0
  for (r in edadesRetiro) {
    # Tiempo que se traerá el flujo a valor presente.
    tiempo <- r-edadValuacion
    # Beneficio proyectado a la edad de retiro r.
    benef <- getBeneficioProyectado(tiempo, sueldoBaseMensual)
    # Probabilidad permanecer hasta la edad de retiro r.
    probSup <- probabilidadDeSupervivencia(tiempo, edadValuacion)
    # Probabi
    probRet <- probabilidadDeRetiro(r)
    sum <- sum + benef*probSup*probRet*v^tiempo    
  }
  vpot <- sum
  vpot
}

#' Calcular el beneficio proyectado de la pension.
#'
#' @param tiempo Tiempo se se proyectará el beneficio.
#' @param sueldoBaseMensual El sueldo a proyectar.
#' @return El beneficio \code{sueldoBaseMensual} proyectado un tiempo \code{tiempo}.
#' @examples
#' getBeneficioProyectado(30, 25000)
#' getBeneficioProyectado(5, 60000)
getBeneficioProyectado <- function(tiempo, sueldoBaseMensual) {
  # Proyectar el salario para obtener el sueldo base.
  sueldoBaseMensualProyectado <- sueldoBaseMensual*(1+tasaIncrementoSalarial)^tiempo
  
  # Calcular el aguinaldo con base en el salario proyectado.
  sueldoDiarioProyectado <- sueldoBaseMensualProyectado/30
  aguinaldoProyectado <- sueldoDiarioProyectado*diasDeAguinaldo
  
  beneficioProyectado <- sueldoDiarioProyectado + aguinaldoProyectado
  beneficioProyectado
}

#' Calcular el beneficio de la pension.
#'
#' @param sueldoBaseMensual Sueldo sobre el cual se calcula el beneficio.
#' @param servicio Años de servicio que el empleado ha acumulado.
#' @return El beneficio de la pension que se estaría recibiendo dado \code{sueldoBaseMensual} y \code{servicio}.
#' @examples
#' getBeneficio(25000, 10)
#' getBeneficio(60000, 35)
getBeneficio <- function(sueldoBaseMensual, servicio) {
  sueldoDiario <- sueldoBaseMensual/30
  beneficio <- sueldoBaseMensual*(3)+(sueldoDiario*20)*servicio
}


# tPx (T)
probabilidadDeSupervivencia <- function(t, x) {
  productorio <- 1
  k <- x+t
  for (i in x:k) {
    row <- unicosAsociados[i-14,]
    pMuerte <- 1-row[[2]]
    pInvalidez <- 1-row[[3]]
    pTerminacion <- 1-row[[4]]
    pRetiro <- 1-row[[5]]
    productorio <- productorio*pMuerte*pInvalidez*pTerminacion*pRetiro
  }
  probabilidad <- productorio
  probabilidad
}

# qx (r)
probabilidadDeRetiro <- function(r) {
    row <- unicosAsociados[r-14,]
    probabilidad <- row[[5]]
    probabilidad
}

# For principal
for (i in 1:nrow(baseEmpleados)) {
  # Obtencion de informacion
  row <- baseEmpleados[i,]
  fechaNac <- as.Date(row[[2]],format='%d/%m/%Y')
  fechaIng <- as.Date(row[[3]],format='%d/%m/%Y')
  sueldoBaseMensual <- row[[4]]
  
  # Caculo de la edad de contratacion.
  anioNac <-format(as.Date(fechaNac, format="%d/%m/%Y"),"%Y")
  anioIng <- format(as.Date(fechaIng, format="%d/%m/%Y"),"%Y")
  edadContratacion <- as.numeric(anioIng) - as.numeric(anioNac)
  
  # Calculo de la antiguedad.
  hoy <- Sys.Date()
  anioHoy <- format(as.Date(hoy, format="%d/%m/%Y"),"%Y")
  antiguedad <- as.numeric(anioHoy) - as.numeric(anioIng)
  
  # Edad a la fecha de valuacion
  edadValuacion <- as.numeric(anioHoy) - as.numeric(anioNac)
  
  # Calcular el VPOT.
  vpot <- getVPOT(edadValuacion, sueldoBaseMensual)
  vpot
}


