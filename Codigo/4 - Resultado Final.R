library(tidyverse)
library(tidyquant)
library(readxl)
library(PerformanceAnalytics)
library(ggthemes)
library(kableExtra)
library(lubridate)
library(stargazer)

# Dados de câmbio
cambio <- read.csv("cambio_inptResultadoFinal.csv")[,-1]
cambio <- cambio %>% spread(symbol, PX_LAST) # Long to wide
cambio <- xts(cambio[ ,-1], as.Date(cambio$date))

# Dados de peso por Markov
pesos <- read.csv("pesos_markov_outMarkov.csv")
pesos <- pesos[-nrow(pesos), ] # Essa data era depois do utlimo rebalanceamento 
pesos <- pesos %>% dplyr::select(Data, CAC, UKX, IBOV, DAX, SPX)
pesos_ew <- pesos
pesos_ew[,-1] <- 0.2

# Dados retorno dos portfólios
ret_port <- read.csv("rets_ports_paises_outBacktestAR1.csv")
ret_port <- xts(ret_port[ ,-1], as.Date(ret_port$Data))

# Dados Indice (MSCI World)
preco_ind <- read_excel('Dados_inptResultadoFinal.xlsx', sheet = 1)
preco_ind <- xts(preco_ind[,-1], preco_ind$Data)
preco_ind <- merge.xts(ret_port, preco_ind, join = "left")
preco_ind <- preco_ind[ ,c(ncol(preco_ind) - 1, ncol(preco_ind))]
ret_ind <- xts(apply(preco_ind, 2, function(x) diff(x)/x[-length(x)]), as.Date(index(preco_ind)[-1]))

# Dados de risk free
risk_free <- read_excel('Dados_inptResultadoFinal.xlsx', sheet = 2)
risk_free <- xts(apply(risk_free[,-1], 2, as.numeric), risk_free$Data)
risk_free <- (risk_free/100 + 1) ^ (1/252) - 1
risk_free[is.na(risk_free)] <- 0
risk_free <- cumprod(risk_free + 1)
risk_free <- merge.xts(ret_port[,1], risk_free, join = "left")
risk_free <- risk_free[,-1]
risk_free <- xts(apply(risk_free, 2, function(x) diff(x)/x[-length(x)]), as.Date(index(risk_free)[-1]))
risk_free <- risk_free[,c(4,7,8,1,3)]

# Dados Nefin
nefin <- read_excel('Dados_inptResultadoFinal.xlsx', sheet = 3)
nefin <- xts(nefin[,-1], nefin$Data)
nefin <- cumprod(nefin + 1)
nefin <- merge.xts(ret_port[,1], nefin, join = "left")
nefin <- nefin[,-1]
nefin <- na.locf(nefin)
nefin <- xts(apply(nefin, 2, function(x) diff(x)/x[-length(x)]), as.Date(index(nefin)[-1]))
colnames(nefin) <- c("Mercado", "Size", "Value", "Momentum", "Risk_free")

# Dados Fama & French
fama_french <- read_excel('Dados_inptResultadoFinal.xlsx', sheet = 4)
fama_french[,-1] <- apply(fama_french[,-1], 2, as.numeric)
fama_french <- xts(fama_french[,-1], ymd(fama_french$Data))
fama_french <- cumprod(fama_french/100 + 1)
fama_french <- merge.xts(ret_port[,1], fama_french, join = "left")
fama_french <- fama_french[,-1]
fama_french <- na.locf(fama_french)
fama_french <- xts(apply(fama_french, 2, function(x) diff(x)/x[-length(x)]), as.Date(index(fama_french)[-1]))
colnames(fama_french) <- c("Mercado", "Size", "Value", "Momentum")

# Alterações no câmbio para termos o retorno na visão do investidor comprado na moeda estrangeira
cambio <- merge.xts(ret_port[,1], cambio, join = "left")[,-1]
cambio <- na.locf(cambio)
cambio <- xts(apply(cambio, 2, function(x) 1 / x), as.Date(index(cambio)))
colnames(cambio) <- c("EURBRL", "GBPBRL", "USDBRL")
ret_cambio <- xts(apply(cambio, 2, function(x) diff(x)/x[-length(x)]), as.Date(index(cambio)[-1]))

#charts.PerformanceSummary(ret_cambio[,c(1,2,3)], engine = "plotly")

# Selecionamos qual estratégia queremos utilizar para construir o portfólio
# 1: long_est6_hp1; 2: ls_est6_hp1; 3: long_est6_hp3; 4: ls_est6_hp3; 
# 5: long_est12 _hp1; 6: ls_est12_hp1; 7: long_est12_hp3; 8: ls_est12_hp3
ret_estrat <- ret_port[ ,seq(5, ncol(ret_port), by = 10)]
ret_indices <- ret_port[ ,seq(9, ncol(ret_port), by = 10)]

# Criamos uma df da mesma dimensão de "ret_estrat" para podermos somar o retorno diário da estratégia com o câmbio
ret_cambio_estrat <- data.frame(EURBRL = ret_cambio$EURBRL, 
                                GBPBRL = ret_cambio$GBPBRL,
                                BRLBRL = rep(0, nrow(ret_cambio)),
                                EURBRL2 = ret_cambio$EURBRL,
                                USDBRL = ret_cambio$USDBRL)
ret_cambio_estrat <- xts(ret_cambio_estrat, as.Date(rownames(ret_cambio_estrat)))

ret_estrat <- ret_estrat + ret_cambio_estrat
ret_indices <- ret_indices + ret_cambio_estrat

# Adiciona a variação cambial aos fatores de FF
for (i in 1:ncol(fama_french)) {
  fama_french[,i] <- fama_french[,i] + ret_cambio$USDBRL
}

# Encontramos o último dia de cada semana
data_fim_semena <- index(ret_estrat[endpoints(ret_estrat, on = "weeks"), ])
data_fim_semena <- data_fim_semena[-length(data_fim_semena)] # Data apos ultimo rebal

# Alteramos as datas dos pesos para permitir o rebalanceamento correto
pesos <- xts(pesos[,-1], data_fim_semena)
pesos_ew <- xts(pesos_ew[,-1], data_fim_semena)

# Calculamos o ret do portfolio e adicionamos à df o ret do benchmark
ret_port_final <- Return.portfolio(ret_estrat, pesos)
ret_port_final <- merge.xts(ret_port_final, ret_ind, join = "left")
ret_port_final[nrow(ret_port_final), 2] <- 0 

# Calculamos o ret do nosso "tailor made" index
ret_indices_final <- Return.portfolio(ret_indices, pesos_ew)
ret_port_final <- merge.xts(ret_port_final, ret_indices_final, join = "left")
ret_port_final <- ret_port_final[,-2]

# Somamos a variação cambial ao retorno indice tambem
ret_port_final[,2] <- ret_port_final[,2] + ret_cambio$USDBRL
colnames(ret_port_final)[c(1,2,3)] <- c("Estratégia", "ACWI Index", "Tailor Made Index")

# Portfólio fatores
pesos_fatores <- pesos$IBOV
pesos_fatores$developed <- 1 - pesos_fatores$IBOV

fatores_constr <- list()
for (i in 1:ncol(fama_french)) {
  fatores_regres <- nefin[,i]
  fatores_regres$Fama_french <- fama_french[,i]
  
  fatores_constr[[i]] <- Return.portfolio(fatores_regres, pesos_fatores)
}

fatores_constr <- do.call("cbind.xts", fatores_constr)
colnames(fatores_constr) <- c("Mercado", "Size", "Value", "Momentum")

######### RESULTADOS

# Função Estat Resultados ----

estat_ret <- function(ret_port, ind, rf) {
  ret_period <- function(ret) {
    prod(ret + 1)^(252 / nrow(ret)) - 1
  }
  
  regres <- lm(I(ret_port - rf) ~ I(ind - rf))
  
  alfa <- as.numeric((coefficients(regres)[1] + 1)^252 - 1)
  p_valor_alfa <- summary(regres)$coefficients[1, 4]
  beta <- as.numeric(coefficients(regres)[2])
  
  ret_acumul <- ret_period(ret_port)
  vol <- sd(ret_port) * sqrt(252)
  SR <- ret_period((ret_port - rf)) / vol
  treynor <- ret_acumul / beta
  cond_var <- as.numeric(CVaR(ret_port))
  IR <- ret_period((ret_port - rf)) / (sd(ret_port - ind) * sqrt(252))
  
  m2 <- SR * sd(ind) * sqrt(252) + ret_period(rf)
  
  resultados <- data.frame(c(
    alfa, p_valor_alfa, beta, m2,
    ret_acumul, vol, SR, treynor,
    cond_var, IR
  )) %>%
    set_names(colnames(ret_port))
  
  rownames(resultados) <- c(
    "Alfa", "P-valor Alfa", "Beta", "M2",
    "Retorno Anual.", "Vol", "Sharpe",
    "Treynor", "CVaR", "Inform. Ratio"
  )
  
  resultados[c(2, 3, 7, 8, 10), 1] <- round(resultados[c(2, 3, 7, 8, 10), 1], 2)
  resultados[c(1, 4, 5, 6, 9), 1] <- paste(round(resultados[c(1, 4, 5, 6, 9), 1] * 100, 2), "%", sep = "")
  
  return(resultados)
}

# END ----

# Regressão dos retornos
regres <- list()

regres[[1]] <- lm(I(ret_port_final$Estratégia - nefin$Risk_free) ~ I(fatores_constr$Mercado - nefin$Risk_free) + fatores_constr$Size + fatores_constr$Value + fatores_constr$Momentum)
for (i in 1:5) {
  regres[[i+1]] <- lm(I(ret_estrat[,i] - nefin$Risk_free) ~ I(fama_french$Mercado - nefin$Risk_free) + fama_french$Size + fama_french$Value + fama_french$Momentum)
}
regres[[4]] <- lm(I(ret_estrat[,3] - nefin$Risk_free) ~ nefin$Mercado + nefin$Size + nefin$Value + nefin$Momentum)


stargazer(regres[[1]], regres[[2]], regres[[3]], regres[[4]], regres[[5]], regres[[6]],
          title="Regressão Long Only - Cahart", align=TRUE, 
          covariate.labels = c("Mercado", "Size", "Value", "Momentum", "Intercepto"),
          report=("vc*p"),
          column.labels = c("Estratégia", "França", "Reino Unido", "Brasil", "Alemanhã", "EUA"),
          dep.var.labels = rep("", 6),
          omit.stat=c("ser","f"))


# Grafico com o peso de cada mercado no portfólio
pesos_graf <- pesos
pesos_graf <- pesos_graf * 100
colnames(pesos_graf) <- c("França", "Reino Unido", "Brasil", "Alemanha", "EUA")
chart.StackedBar(pesos_graf, main = "Pesos de cada País", ylab = "Alocação (%)", colorset=c(6,2,3,4,5))

chart.CumReturns(ret_port_final[,c(1,2,3)], main = "", plot.engine = "plotly")

# Performance Attribution
ret_indices <- ret_port[ ,seq(9, ncol(ret_port), by = 10)]
ret_indices <- ret_indices + ret_cambio_estrat

ret_acumul_indices <- list()
for (i in 1:ncol(ret_indices)) {
  port1 <- Return.portfolio(ret_indices[,i], weights = pesos[,i])
  ret_acumul_indices[[i]] <- prod(1 + port1) ^ (252/nrow(port1)) - 1
}

ret_acumul_indices <- do.call("c", ret_acumul_indices)

ret_acumul_ports <- list()
for (i in 1:ncol(ret_estrat)) {
  port1 <- Return.portfolio(ret_estrat[,i], weights = pesos[,i])
  ret_acumul_ports[[i]] <- prod(1 + port1) ^ (252/nrow(port1)) - 1
}
ret_acumul_ports <- do.call("c", ret_acumul_ports)

ret_acumul_port_final <- prod(1 + ret_port_final$Port_final) ^ (252/nrow(ret_port_final$Port_final)) - 1
ret_acumul_ind_final <- prod(1 + ret_port_final$MXWO.Index) ^ (252/nrow(ret_port_final$MXWO.Index)) - 1

paises <- c("França", "Reino Unido", "Brasil", "Alemanha", "EUA")
df_perf_attr <- data.frame(Pais = rep(paises, 2), Perf_Attrib = c(rep(c("Asset Allocation"), 5), rep(c("Stock Selection"), 5)), c(ret_acumul_indices, (ret_acumul_ports - ret_acumul_indices)))
colnames(df_perf_attr) <- c("Pais", "Performance Attribution", "Retorno Anualizado (%)")
df_perf_attr$`Retorno Anualizado (%)` <- round(df_perf_attr$`Retorno Anualizado (%)`*100,2)
df_perf_attr$`Performance Attribution` <- as.factor(df_perf_attr$`Performance Attribution`)
df_perf_attr$`Performance Attribution` <- relevel(df_perf_attr$`Performance Attribution`, "Stock Selection")

# clipr::write_clip(df_perf_attr)

ggplot() + 
  geom_bar(data = df_perf_attr, aes(fill=`Performance Attribution`, y=`Retorno Anualizado (%)`, x=Pais), position="stack", stat="identity") + 
  theme_solarized_2() + ggtitle("Perfomance Attribution") +
  scale_fill_manual("Performance Attribution", values = c("Stock Selection" = "orange", "Asset Allocation" = "dark blue")) +
  geom_bar(aes(x = "Portfólio", y=round(ret_acumul_port_final*100,2)), stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=22))

# Estatisticas dos retornos (Países isolados)
ret_indices_local_curr <- ret_port[ ,seq(9, ncol(ret_port), by = 10)]
ret_ports_local_curr <- ret_port[ ,seq(5, ncol(ret_port), by = 10)]

estat_paises_isolados <- list()

for (i in 1:ncol(ret_estrat)) {
  estat_paises_isolados[[i]] <- estat_ret(ret_ports_local_curr[,i], ret_indices_local_curr[,i], risk_free[,i])
}

estat_paises_isolados <- do.call("cbind", estat_paises_isolados)
colnames(estat_paises_isolados) <- c("França", "Reino Unido", "Brasil", "Alemanha", "EUA")

kable(estat_paises_isolados, "latex", booktabs = TRUE, align = rep('c', 5))

# Estatisticas dos retornos (Estratégia Final)
estat_port_final <- list()
for (i in 1:ncol(ret_port_final)) {
  estat_port_final[[i]] <- estat_ret(ret_port_final[,i], ret_port_final$`ACWI Index`, risk_free$GEBR10Y.Index)
}

estat_port_final <- do.call("cbind", estat_port_final)
estat_port_final[c(1,2,3,4,8,10),c(2,3)] <- ""

kable(estat_port_final, "latex", booktabs = TRUE, align = rep('c', 5))