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
library(furrr)
library(progressr)
library(parallel)

plan(multisession, workers = parallel::detectCores())

source("99 - Funções Backtest.R")

retornos_geral <- list()
for(pais in c("France", "UK", "Brazil", "Germany", "USA")){
  "%ni%" <- Negate("%in%")
  # Importa a composicao do indice
  dbComp_ip <- read_csv(paste("C:/Users/Pedro/Desktop/Comp_inds/Dados/", pais, "\\", pais, "_comp.csv", sep = ""))
  dbComp <- data.frame(dbComp_ip$Ativos, apply(dbComp_ip[,-1], 2, as.numeric))
  colnames(dbComp) <- colnames(dbComp_ip)
  rm(dbComp_ip)
  
  # Importa o retorno diario do indice
  dbRet_ind_ip <- read_csv(paste("C:/Users/Pedro/Desktop/Comp_inds/Dados/", pais, "\\", pais, "_indice.csv", sep = ""))
  dbRet_ind <- data.frame(as.Date(dbRet_ind_ip$Data), apply(dbRet_ind_ip[,-1], 2, as.numeric))
  colnames(dbRet_ind) <- colnames(dbRet_ind_ip)
  rm(dbRet_ind_ip)
  
  # Importa o retorno diario da taxa livre de risco
  dbRisk_free_ip <- read_csv(paste("C:/Users/Pedro/Desktop/Comp_inds/Dados/", pais, "\\", pais, "_rf.csv", sep = ""))
  dbRisk_free <- data.frame(as.Date(dbRisk_free_ip$Data), apply(dbRisk_free_ip[,-1], 2, as.numeric))
  colnames(dbRisk_free) <- colnames(dbRisk_free_ip)
  rm(dbRisk_free_ip)
  
  # Immporta o retorno diário dos ativos que compoe o indice
  dbRet_ativos_ip <- read_csv(paste("C:/Users/Pedro/Desktop/Comp_inds/Dados/", pais, "\\", pais, "_ativos.csv", sep = ""))
  dbRet_ativos <- data.frame(as.Date(dbRet_ativos_ip$Data), apply(dbRet_ativos_ip[,-1], 2, as.numeric))
  colnames(dbRet_ativos) <- colnames(dbRet_ativos_ip)
  rm(dbRet_ativos_ip)
  
  # inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
  lista_ativos <- params_estim(20131231, 20201231, c(1,3), c(6,12), dbRet_ativos, "AR", "asc", 6)
  
  ret_port_geral <- list()
  for(j in names(lista_ativos)){
    for(i in names(lista_ativos[[j]])){
      nome <- paste(j, i, sep = "_")
      ret_port_geral[[nome]] <- backtest(lista_ativos[[j]][[i]], 20131231, 20201231, 0.3, "HRP") 
    }
  }
  
  df_ports <- do.call("cbind.xts", ret_port_geral)
  
  nomes_df_ports <- vector()
  cont <- 1
  for(i in names(ret_port_geral)){
    for(j in c("long", "ls")){
      nomes_df_ports[cont] <- paste(j, i, sep = "_")
      cont <- cont + 1
    }
  }
  
  colnames(df_ports) <- paste(pais, nomes_df_ports, sep = "_")
  
  ind_aval <- dbRet_ind %>% dplyr::filter(Data >= "2013-12-31" & Data <= "2020-12-31")
  ind_aval <- xts(ind_aval[, 2, drop = FALSE], ind_aval$Data)
  rf_aval <- dbRisk_free %>% dplyr::filter(Data >= "2013-12-31" & Data <= "2020-12-31")
  rf_aval <- xts(rf_aval[, 2, drop = FALSE], rf_aval$Data)
  
  #for (i in seq(2, ncol(df_ports), by = 2)) {
  #    df_ports[[i]] <- df_ports[[i]] + rf_aval[[1]]
  #}
  
  df_ports <- merge.xts(df_ports, merge.xts(ind_aval, rf_aval))
  
  retornos_geral[[pais]] <- df_ports
  
}    

retornos_paises <- do.call("cbind.xts", retornos_geral)

retornos_paises <- retornos_paises[-1,]

ret_paises_base <- retornos_paises

retornos_paises[is.na(retornos_paises)] <- 0

pdf("Retornos_paises_outBacktestAR1.pdf")

tipos_port <- c("Long Only", "Long & Short")
ratio <- ncol(retornos_paises) / length(retornos_geral)
for(j in seq_along(tipos_port)){
  for(i in seq_along(retornos_geral)){
    charts.PerformanceSummary(retornos_paises[,seq(j + ratio*(i-1), j-1 + ratio*i, by = 2)], main = paste("AR(1)", tipos_port[j] ,names(retornos_geral[i]), sep = " - "))
  }
}

dev.off()

retornos_paises_df <- data.frame(Data = index(retornos_paises), as.data.frame(retornos_paises))

write.csv(retornos_paises_df, "rets_ports_paises_outBacktestAR1.csv", row.names = FALSE)
