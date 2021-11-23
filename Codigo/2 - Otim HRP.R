library(dplyr)
library(ggplot2)
library(tidyr)
library(PerformanceAnalytics)
library(tseries)
library(quantmod)
library(lattice)
library(furrr)
library(tictoc)
library(lubridate)

plan(multisession, workers = parallel::detectCores())

df_ret <- read.csv("ret_ativos_inptOtimHRP.csv")
df_ret <- xts(df_ret[, -1], as.Date(df_ret$Data))

# Funções que realizam a alocação de pesos através de Matrix Seriation e Recursive Bisection ----

getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}

getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}


getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}

recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- as.numeric(1 - cVar0/(cVar0 + cVar1))
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <- w[cItems0] * alpha
  w[cItems1] <- w[cItems1] * (1-alpha)
  
  if(length(cItems0) > 1) {
    w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}




# Parte II dados mensais comparacao par covar  ----

# function to append missing asset names and sort into original order
appendMissingAssets <- function(wts, allAssetNames, wtsDate) {
  absentAssets <- allAssetNames[!allAssetNames %in% names(wts)]
  absentWts <- rep(0, length(absentAssets))
  names(absentWts) <- absentAssets
  wts <- c(wts, absentWts)
  wts <- xts(t(wts), order.by=wtsDate)
  wts <- wts[,allAssetNames]
  return(wts)
}


grid <- expand.grid(meses=1:12,dias=c(15, 21, 30, 42, 63, 84, 126, 168, 210, 252)) # meses dias

# ini
sharpe_grid <- function(j) {
  
  cat(j, "\n\n", sep = "")
  
  set.seed(j)
  df_ret_sp <- df_ret[,sample(1:ncol(df_ret), 100)]
  
  perfser <-  list()  # séries simul
  for(k in 1:nrow(grid)){
    #cat(paste(round(k/nrow(grid)*100, 1), "%", sep = ""), "")
    rets <- df_ret_sp
    #ep <- endpoints(rets, on =  "months")
    
    hrpWts <- list()
    
    nMonths = grid[k,1] # meses lookback (6 as per parameters from allocateSmartly)
    nVol = grid[k,2] # day lookback for volatility (20 ibid)
    
    data_inicial <- as.Date("2010-01-01") %m-% months(nMonths)
    
    retSubset <- rets[paste(data_inicial, "/", sep = "")]
    
    ep <- endpoints(retSubset, on =  "months")
    
    #BT
    for(i in 1:(length(ep)-nMonths)) {
      
      # get returns subset 
      selectedSubset <- retSubset[c(ep[i]:ep[(i+nMonths)]),]
      
      cors <- cor(selectedSubset) # correlation
      volSubset <- tail(selectedSubset, nVol) # 20 day volatility
      vols <- t(as.matrix(apply(volSubset, 2, sd)))
      covs <- t(vols) %*% vols * cors
      
      # hrp weights
      clustOrder <- hclust(dist(cors), method = 'single')$order
      hrpWt <- getRecBipart(covs, clustOrder)
      names(hrpWt) <- colnames(covs)
      hrpWt <- appendMissingAssets(hrpWt, colnames(retSubset), last(index(selectedSubset)))
      hrpWts[[i]] <- hrpWt
    }
    
    
    # cash if any ws
    hrpWts <- do.call(rbind, hrpWts)
    # compute backtest returns
    hrpRets <- Return.portfolio(R = rets, weights = hrpWts)
    
    # result
    
    compare <- hrpRets
    colnames(compare)[1] <- c("HRP")
    
    perfser[[k]] <- compare
    
    #print()
  }
  
  sharpe <- function(x){
    ret <- prod(1 + x) ^ (252 / length(x)) - 1
    vol <- sd(x) * sqrt(252)
    return(ret / vol)
  }
  
  tabsdes <- lapply(perfser, sharpe)
  
  #IS
  
  return(cbind(grid,do.call("rbind",tabsdes)))
  
}    

tictoc::tic()
teste <- future_map(1:1000, sharpe_grid, .options = furrr_options(seed = TRUE))
tictoc::toc()

IS1 <- lapply(teste, function(x) x[,3, drop = FALSE])

IS1 <- do.call("cbind", IS1)

is_media <- cbind(grid, as.data.frame(apply(IS1[,1:1000], 1, mean)))
rownames(is_media) <- c()
colnames(is_media)[3] <- "HRP"

levelplot(HRP ~ meses*dias, data = is_media,
          xlab = "Meses - Correlação", ylab = "Dias - Volatilidade",
          main = paste("IS HRP - 100 ativos", "- 1000 repetições"),
          col.regions = terrain.colors(100)
)