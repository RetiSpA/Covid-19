library(earlyR)
library(dplyr)
library(ggplot2)
library(deSolve)
library(writexl)

data <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv")

for (prov in data$codice_regione) {
  assign(paste("data_",prov,sep=""),data[data$codice_regione==prov,])
}

Infected <- data_2$totale_attualmente_positivi[data_2$totale_attualmente_positivi>0]
Morti=data_2$deceduti
Guariti=data_2$dimessi_guariti

data_2$totale_attualmente_positivi[1]
Day=(max(which(data_2$totale_attualmente_positivi==0))+1):(length(Infected))

N <- 125666  # population of regione

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

t <- 1:100
VallediAosta<- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
write_xlsx(VallediAosta, path = "VallediAosta.xlsx", col_names = TRUE)

