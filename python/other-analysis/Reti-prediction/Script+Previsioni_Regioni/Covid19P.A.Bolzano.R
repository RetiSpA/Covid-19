library(earlyR)
library(dplyr)
library(ggplot2)
library(deSolve)
library(writexl)

data <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv")

for (prov in data$codice_regione) {
  assign(paste("data_",prov,sep=""),data[data$codice_regione==prov,])
}

Bolzano=data_4[data_4$denominazione_regione=="P.A. Bolzano",]
Infected <- Bolzano$totale_attualmente_positivi[Bolzano$totale_attualmente_positivi>0]
Morti=Bolzano$deceduti
Guariti=Bolzano$dimessi_guariti

Bolzano$totale_attualmente_positivi[1]
Day=(max(which(Bolzano$totale_attualmente_positivi==0))+1):(length(Infected))

N <- 531178  # population of regione

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
P.A.Bolzano<- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
write_xlsx(P.A.Bolzano, path = "P.A.Bolzano.xlsx", col_names = TRUE)

