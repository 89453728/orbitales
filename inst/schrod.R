## Funciones de probabilidad de la posible localizacion del electron
## para alguno de los orbitales cuanticos en coordenadas esfericas.

# no se cuanto valen las constantes, estos valores son arbitrarios.
a0 <- .1
Z <- 2.3

# Obrbital (n,m,l) = (1,0,0)
orbital.phi_100 <- function (r=c(0)) {
    1 / sqrt(pi) * (Z/a0)^(3/2) * exp(-Z*r/a0)
}

# Obrbital (n,m,l) = (2,0,0)
orbital.phi_200 <- function (r=c(0)) {
    1/(4*sqrt(2*pi)) * (Z/a0)^(3/2) * (2 - Z*r/a0) * exp(-Z*r/2/a0) 
}

# Obrbital (n,m,l) = (2,1,0)
orbital.phi_210 <- function (r=c(0),theta=c(0)) {
    if (length(r) != length(theta))
        return(FALSE)
    1/(4*sqrt(2*pi)) * (Z/a0)^(3/2) * (Z*r/a0)*exp(-Z*r/2/a0) *cos(theta)
}

# Obrbital (n,m,l) = (2,1,+-1)
orbital.phi_21pm1 <- function (r=c(0),theta=c(0),phi=c(0),plus=TRUE) {
    if (length(r) != length(theta) && length(theta) != length(phi) && length(r) != length(phi))
        return(FALSE)
    1/(sqrt(pi)*8) * (Z/a0)^(3/2) * Z*r/a0 * exp(-Z*r/2/a0) * sin(theta) * 
        exp(phi * (function(){
                if (plus)
                    -1
                else
                    1})())
}

# Obrbital (n,m,l) = (3,0,0)
orbital.phi_300 <- function (r=c(0)) {
    1/(81 * sqrt(3*pi)) * (Z/a0)^(3/2) * (27 - 18*Z*r/a0 + 2 *(Z*r/a0)^2 ) * exp(-Z*r/3/a0)
}

# Obrbital (n,m,l) = (3,1,0)
orbital.phi_310 <- function (r=c(0),theta=c(0)) {
    if (length(r) != length(theta))
        return(FALSE)
    sqrt(2)/(81*sqrt(pi)) *(Z/a0)^(3/2) * (6 - Z*r/a0)*Z*r/a0 * exp(-Z*r/3/a0) * cos(theta)
}
