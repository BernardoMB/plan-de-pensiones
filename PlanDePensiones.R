# Especificar el directorio de trabajo.
setwd("C:/Users/hca/Desktop/plan-de-pensiones")

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
edadesRetiro <- seq(from = 50, to = 65, by = 1)

#' Calcular el valor presente actuarial de las obligaciones totales.
#'
#' @param edadValuacion Edad de la persona a la fecha de evaluaci�n.
#' @param sueldoBaseMensual El sueldo de la persona en la fecha de valuci�n.
#' @param servicio A�os de servicio.
#' @return El VPOT de una persona de edad \code{edadValuacion} con sueldo base mensual de \code{sueldoBaseMensual}.
#' @examples
#' getVPOT(40, 30000, 10)
#' getVPOT(55, 45000, 15)
getVPOT <- function(edadValuacion, sueldoBaseMensual, servicio) {
  edades <- c()
  if (edadValuacion < min(edadesRetiro)) {
    edades <- edadesRetiro
  } else {
    edades <- seq(from = edadValuacion, to = 65, by = 1)
  }
  sum <- 0
  for (r in edades) {
    # Tiempo que se traer� el flujo a valor presente.
    tiempo <- r-edadValuacion
    # Calcular el sueldo proyectado
    sueldoProyectado <- getSueldoProyectado(tiempo, sueldoBaseMensual)
    # Calcular el sueldo pensionable
    sueldoPensionable <- sueldoProyectado + getAguinaldo(sueldoProyectado)
    # Beneficio proyectado a la edad de retiro r.
    benef <- getBeneficio(sueldoPensionable, servicio)
    # Probabilidad permanecer hasta la edad de retiro r.
    probSup <- probabilidadDeSupervivencia(tiempo, edadValuacion)
    # Probabi
    probRet <- probabilidadDeRetiro(r)
    sum <- sum + (benef*v^tiempo)*probSup*probRet
  }
  vpot <- sum
  vpot
}

#' Calcular el pasivo acumulado.
#'
#' @param edadValuacion Edad de la persona a la fecha de evaluaci�n.
#' @param sueldoBaseMensual El sueldo de la persona en la fecha de valuci�n.
#' @param servicio A�os de servicio.
#' @return El PA de una persona de edad \code{edadValuacion} con sueldo base mensual de \code{sueldoBaseMensual} y antiguedad de \code{servicio}.
#' @examples
#' getPA(40, 30000, 10)
#' getPA(55, 45000, 15)
getPA <- function(edadValuacion, sueldoBaseMensual, servicio) {
  edades <- c()
  if (edadValuacion < min(edadesRetiro)) {
    edades <- edadesRetiro
  } else {
    edades <- seq(from = edadValuacion, to = 65, by = 1)
  }
  # Calcular el sueldo pensionable
  sueldoPensionable <- sueldoBaseMensual + getAguinaldo(sueldoBaseMensual)
  # Beneficio que le toca a la edad de valuaci�n.
  benef <- getBeneficio(sueldoPensionable, servicio)
  sum <- 0
  for (r in edades) {
    # Tiempo que se traer� el flujo a valor presente.
    tiempo <- r-edadValuacion
    # Probabilidad permanecer hasta la edad de retiro r.
    probSup <- probabilidadDeSupervivencia(tiempo, edadValuacion)
    # Probabi
    probRet <- probabilidadDeRetiro(r)
    sum <- sum + (benef*v^tiempo)*probSup*probRet
  }
  pa <- sum
  pa
}

#' Calcular el costo normal.
#'
#' @param edadValuacion Edad de la persona a la fecha de evaluaci�n.
#' @param sueldoBaseMensual El sueldo de la persona en la fecha de valuci�n.
#' @param servicio A�os de servicio.
#' @return El CN de una persona de edad \code{edadValuacion} con sueldo base mensual de \code{sueldoBaseMensual} y antiguedad de \code{servicio}.
#' @examples
#' getCN(40, 30000, 10)
#' getCN(55, 45000, 15)
getCN <- function(edadValuacion, sueldoBaseMensual, servicio) {
  PA1 <- getPA(edadValuacion, sueldoBaseMensual, servicio)
  edadValuacion2 <- edadValuacion + 1
  sueldoBaseMensual2 <- sueldoBaseMensual*(1+tasaIncrementoSalarial)
  servicio2 <- servicio + 1
  PA2 <- getPA(edadValuacion2, sueldoBaseMensual2, servicio2)
  cn <- PA2 - PA1
  cn
}

#' Calcular el sueldo proyectado
#'
#' @param tiempo Tiempo se se proyectar� el sueldo.
#' @param sueldoMensual El sueldo a proyectar.
#' @return El sueldo mensual que recibir� el empleado des pues de un tiempo \code{tiempo}.
#' @examples
#' getSueldoProyectado(10, 35000)
#' getSueltoProyectado(20, 25000)
getSueldoProyectado <- function(tiempo, sueldoMensual) {
  sueldoProyectado <- sueldoMensual*(1+tasaIncrementoSalarial)^tiempo
  sueldoProyectado
}

#' Calcular el beneficio de la pension.
#'
#' @param sueldoPensionable Sueldo sobre el cual se calcula el beneficio.
#' @param servicio A�os de servicio que el empleado ha acumulado.
#' @return El beneficio de la pension que se estar�a recibiendo dado \code{sueldoPensionable} y \code{servicio}.
#' @examples
#' getBeneficio(25000, 10)
#' getBeneficio(60000, 35)
getBeneficio <- function(sueldoPensionable, servicio) {
  sueldoPensionableDiario <- sueldoPensionable/30
  beneficio <- sueldoPensionable*3 + sueldoPensionableDiario*20*servicio
  beneficio
}

#' Calcular el aguinaldo.
#'
#' @param sueldoMensual Sueldo mensual sobre el cual se calcula el aguinaldo.
#' @return El aguinaldo seg�n el \code{sueldoMensual}
#' @examples
#' getAguinaldo(25000)
#' getAguinaldo(10000)
getAguinaldo <- function(sueldoMensual) {
  sueldoDiario <- sueldoMensual/30
  aguinaldo <- sueldoMensual + sueldoDiario*8
  aguinaldo
}

#' Calcular la probabilidad de que un empleado sobreviva a todos los decrementos.
#'
#' @param t tiempo.
#' @param x edad.
#' @return Probabilidad de que un empleado de edad \code{x} sobreviva a todos los decrementos \code{t} a�os.
#' @examples
#' probabilidadDeSupervivencia(10, 35)
#' probabilidadDeSupervivencia(1,60)
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

#' Calcular la probabilidad de terminacion de un empleado por la causa de retiro.
#'
#' @param t tiempo.
#' @param x edad.
#' @return Probabilidad de que un empleado de edad \code{x} se termine por la cause de retiro.
#' @examples
#' probabilidadDeRetiro(50)
#' probabilidadDeRetiro(62)
probabilidadDeRetiro <- function(r) {
    row <- unicosAsociados[r-14,]
    probabilidad <- row[[5]]
    probabilidad
}

VPOTs <- c()
PAs <- c()
CNs <- c()

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
  
  # Calculo de los a�os de servicio
  hoy <- Sys.Date()
  anioHoy <- format(as.Date(hoy, format="%d/%m/%Y"),"%Y")
  servicio <- as.numeric(anioHoy) - as.numeric(anioIng)
  
  # Edad a la fecha de valuacion
  edadValuacion <- as.numeric(anioHoy) - as.numeric(anioNac)
  
  # Calcular el VPOT.
  vpot <- getVPOT(edadValuacion, sueldoBaseMensual, servicio)
  vpot
  VPOTs <- c(VPOTs, vpot)
  
  # Calcular el pasivo acumulado.
  pa <- getPA(edadValuacion, sueldoBaseMensual, servicio)
  pa
  PAs <- c(PAs, pa)
  
  # Calcular el costo normal
  cn <- getCN(edadValuacion, sueldoBaseMensual, servicio)
  cn
  CNs <- c(CNs, cn)
}

pension.plan <- data.frame(VPOTs, PAs, CNs)
pension.plan.names <- c('VPOT', 'PA', 'CN')


