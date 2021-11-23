library(tidyverse)
library(readxl)
library(xts)
library(PerformanceAnalytics)
library(lmtest)
library(AER)
library(ggthemes)
library(lubridate)
library(stargazer)
library(kableExtra)
library(writexl)
library(quantmod)
library(depmixS4)

precos_ind <- read.csv("precos_indices_inptMarkov.csv")
precos_ind <- xts(precos_ind[ ,-1], as.Date(precos_ind$Data))

tictoc::tic()

ratio_buy_geral <- vector("list", length = (ncol(precos_ind) / 2))
exp_ret_geral <- vector("list", length = (ncol(precos_ind) / 2))
ret_paises_geral <- vector("list", length = (ncol(precos_ind) / 2))
for(j in 1:(ncol(precos_ind) / 2)){
  pais <- precos_ind[,c(j*2-1,j*2)]
  pais <- na.omit(pais)
  
  pais_week <- (pais[endpoints(index(pais), on = "weeks"),])
  
  pais_ret <- apply(pais_week, 2, function(price) diff(log(as.numeric(price)))) 
  pais_ret <- xts(pais_ret, index(pais_week)[-1])
  ret_paises_geral[[j]] <- pais_ret
  
  inicio <- nrow(pais_ret["/2013"])
  exp_ret <- vector()
  ratio_buy <- vector()
  for(i in 1:(nrow(pais_ret)-inicio)){
    pais_ret_loop <- pais_ret[1:(inicio - 1 + i),, drop = FALSE]
    
    mod4 <- depmix(list(pais_ret_loop[,2] ~ 1,pais_ret_loop[,1] ~ 1), family = list(gaussian(),gaussian()), nstates = 4, data = pais_ret_loop)
    
    set.seed(1)
    fm4 <- fit(mod4, verbose = FALSE)
    
    selm = fm4
    
    tsp <- as.ts(pais_ret_loop)
    
    matriz_trans <- t(sapply(selm@transition,function(ob)ob@parameters$coefficients))
    
    prob_atual <- as.numeric(posterior(selm, type = "smoothing")[nrow(pais_ret_loop),])
    
    ret_estado <- summary(selm, which = "response")[,1]
    
    exp_ret[i] <- sum(ret_estado * (prob_atual %*% matriz_trans))
    names(exp_ret)[i] <- as.character(index(pais_ret_loop)[nrow(pais_ret_loop)])
    
    ratio_buy[i] <- sum(exp_ret[i] > pais_ret_loop[,2])/nrow(pais_ret_loop)
    names(ratio_buy)[i] <- as.character(index(pais_ret_loop)[nrow(pais_ret_loop)])
  }
  ratio_buy_geral[[j]] <- ratio_buy
  exp_ret_geral[[j]] <- exp_ret
}


tictoc::toc()


# Por questões relacionadas a disponibilidade de dados, IBOV possui menos semanas de dados
# Nessas semanas, como não temos método melhor, definiremos que deve ser comprado o mesmo valor que o mes anterior

# Corrigimos IBOV Index
ratio5 <- ratio_buy_geral[[5]]
ratio5_names <- names(ratio5)
ratio5_names <- append(ratio5_names, "2014-01-03", after = 1)
ratio5_names <- append(ratio5_names, "2019-12-20", after = 312)
ratio5_names <- append(ratio5_names, "2019-12-27", after = 313)

ratio5_valores <- ratio5
ratio5_valores <- append(ratio5_valores, ratio5_valores[1], after = 1)
ratio5_valores <- append(ratio5_valores, ratio5_valores[312], after = 312)
ratio5_valores <- append(ratio5_valores, ratio5_valores[313], after = 313)

names(ratio5_valores) <- ratio5_names

ratio_buy_geral[[5]] <- ratio5_valores


# Criamos um vetor contendo as datas de cada semana
# Comecaremos uma semana depois da primeira semana, dado que sao previsoes
Data <- ymd(20140103) %m+% weeks(0:365)


ratio_geral <- lapply(ratio_buy_geral, as.data.frame)
ratio_geral <- do.call("cbind", ratio_geral)
colnames(ratio_geral) <- c("UKX", "CAC", "DAX", "SPX", "IBOV")

soma_pesos <- apply(ratio_geral, 1, sum)  

ratio_geral <- apply(ratio_geral, 2, function(x) x/soma_pesos) 

ratio_geral <- as.data.frame(ratio_geral)

ratio_geral <- ratio_geral %>% mutate(Data, .before = 1)
rownames(ratio_geral) <- c()


write.csv(ratio_geral, paste(getwd(), "pesos_markov_outMarkov.csv", sep = "\\"), row.names = FALSE)
