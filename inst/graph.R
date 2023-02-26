source('schrod.R')

err <- try(expr = library(rgl),silent=TRUE)
if (length(err) == 1) {
    err <- readline(prompt = 'Se necesita rgl para mostrar los resultados, desea instalar el paquete? [y/n] ')
    if (err == 'y')
        install.packages(rgl)
    else
        quit('no')
} else
    library(rgl)

Npoints <- 3e5
xMIN <- .0; xMAX <- 3.0; yMIN <- 0.0; yMAX <- pi; zMIN <- 0.0; zMAX <- 2*pi

x <- runif(Npoints, min=xMIN, max=xMAX)
y <- runif(Npoints, min=yMIN, max=yMAX)
z <- runif(Npoints, min=zMIN, max=zMAX)

r <- sqrt(x*x + y*y + z*z)
phi <- atan(y/x)
theta <- acos(z/r)

# cota de probabilidad (cuanto % del maximo de probabilidad se va a representar graficamente).
prob_quote <- .4
# tolerancia (% de probabilidad) como margen superior e inferior de probabilidad que se le quiere dar
tolerance <- .01

menu_status <- TRUE
prob_results <- c(0)

while (menu_status){
    print(read.delim('menu.txt',header=FALSE))
    opt <- readline('Chose an option: ')
    menu_status = FALSE
    if (opt == 1) 
        prob_results <- orbital.phi_100(r)
    else if (opt == 2)
        prob_results <- orbital.phi_200(r)
    else if (opt == 3)
        prob_results <- orbital.phi_210(r,theta)
    else if (opt == 4)
        prob_results <- orbital.phi_21pm1(r,theta,phi,plus=TRUE)
    else if (opt == 5)
        prob_results <- orbital.phi_300(r)
    else {
        message('>> bad option\n\n')
        menu_status = TRUE
    }
}

prob_results <- abs(prob_results)
prob_results_max <- max(prob_results)

results <- {}

results$x <- x
results$y <- y
results$z <- z
results$prob <- prob_results

contained <- function (value, maxvalue, quote=.1, tolerance = .1) {
    (value >= maxvalue*(quote*(1 - tolerance)) && value <= maxvalue*(quote*(1+tolerance)))
}

reduced_results <- {}; reduced_results$x = c(); reduced_results$y = c(); reduced_results$z = c(); reduced_results$prob = c()

for (e in seq(from = 1, to = length(results$prob), by=1)) {
    if (contained(results$prob[e],prob_results_max,quote = prob_quote,tolerance = tolerance)) {
        reduced_results$x <- append(reduced_results$x, results$x[e])
        reduced_results$y <- append(reduced_results$x, results$y[e])
        reduced_results$z <- append(reduced_results$x, results$z[e])
        reduced_results$prop <- append(reduced_results$x, results$prop[e])
    }
}

plot3d(x = reduced_results$x,y = reduced_results$y, z = reduced_results$z, 
       col='oldlace',type='s',radius=.1)

