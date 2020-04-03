library(earlyR)
library(dplyr)
library(ggplot2)
library(deSolve)
library(writexl)
data <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")

Infected <- data$totale_attualmente_positivi
Morti=data$deceduti
Guariti=data$dimessi_guariti
Day <- 1:(length(Infected))
N <- 60000000 # population of italy

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I - alfa * I
    dM <- gamma * I
    dG <- gamma * I
    list(c(dS, dI, dM, dG))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], M=Morti[1], G=Guariti[1])
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "alfa")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(1, 1, 1)) 
Opt$message
Opt_par <- setNames(Opt$par, c("beta", "gamma","alfa"))
Opt_par

t <- 1:32
Italia <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
write_xlsx(Italia, path = "Italia.xlsx", col_names = TRUE)

col <- 1:4
matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Deads", "Healeds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model 2019-nCoV Italia", outer = TRUE, line = -2)

