gmm_variance <- function(coef, data){
  
  MERCADOS <- data[["MERCADOS"]]
  DELTA <- data[["DELTA"]]
  CURSOS <- data[["CURSOS"]]
  DISTANCIAS <- data[["DISTANCIAS"]]
  MARKET_SIZE <- data[["MARKET_SIZE"]]
  TEMP_MERCADOS <- data[["TEMP_MERCADOS"]]
  exogenous <- data[["exogenous"]]
  instruments <- data[["instruments"]]
  fixed_effects <- data[["fixed_effects"]]
  smm_fe_moments <- data[["smm_fe_moments"]]
  demean <- data[["functions"]]$demean
  find_delta <- data[["functions"]]$find_delta
  options <- data[["options"]]
  log <- data[["log"]]
  
  DELTAS <- data[["DELTAS"]]
  epsilon <- 0.001
  
  load("local\\cod04_Z_BASE.RData")
  load("local\\cod04_Z_ELSE.RData")
  load("local\\cod04_W.RData")
  
  CURSOS_BASE <- CURSOS %>%
    left_join(DELTAS, by = "CO_CURSO_N")
  
  ####ANTES, O Z estava multiplicado por (1/n). Agora não está mais
  
  xi_BASE <- as.vector(CURSOS_BASE$DELTA)
  xi_ENEM_1 <- as.vector(CURSOS_BASE$ENEM_SM1 - CURSOS_BASE$ENEM_DT1)
  xi_ENEM_2 <- as.vector(CURSOS_BASE$ENEM_SM2 - CURSOS_BASE$ENEM_DT2)
  xi_ENEM_3 <- as.vector(CURSOS_BASE$ENEM_SM3 - CURSOS_BASE$ENEM_DT3)
  xi_ENEM_4 <- as.vector(CURSOS_BASE$ENEM_SM4 - CURSOS_BASE$ENEM_DT4)
  xi_RDPC_1 <- as.vector(CURSOS_BASE$RDPC_SM1 - CURSOS_BASE$RDPC_DT1)
  xi_RDPC_2 <- as.vector(CURSOS_BASE$RDPC_SM2 - CURSOS_BASE$RDPC_DT2)
  xi_RDPC_3 <- as.vector(CURSOS_BASE$RDPC_SM3 - CURSOS_BASE$RDPC_DT3)
  xi_RDPC_4 <- as.vector(CURSOS_BASE$RDPC_SM4 - CURSOS_BASE$RDPC_DT4)
  xi_COTA_1 <- as.vector(CURSOS_BASE$COTA_SM1 - CURSOS_BASE$COTA_DT1)
  xi_COTA_2 <- as.vector(CURSOS_BASE$COTA_SM2 - CURSOS_BASE$COTA_DT2)
  xi_DIST_0 <- as.vector(CURSOS_BASE$DIST_SM0 - CURSOS_BASE$DIST_DT0)

  G <- cbind(diag(xi_BASE) %*% Z_BASE, 
             diag(xi_ENEM_1) %*% Z_ELSE, 
             diag(xi_ENEM_2) %*% Z_ELSE, 
             diag(xi_ENEM_3) %*% Z_ELSE, 
             diag(xi_ENEM_4) %*% Z_ELSE, 
             diag(xi_RDPC_1) %*% Z_ELSE, 
             diag(xi_RDPC_2) %*% Z_ELSE, 
             diag(xi_RDPC_3) %*% Z_ELSE, 
             diag(xi_RDPC_4) %*% Z_ELSE, 
             diag(xi_COTA_1) %*% Z_ELSE,
             diag(xi_COTA_2) %*% Z_ELSE,
             diag(xi_DIST_0) %*% Z_ELSE)
  
  base_meanG <- as.matrix(colMeans(G))
  
  myCluster <- makeCluster(8, type = "PSOCK")
  registerDoParallel(myCluster)
  DERIVADA <- foreach(j = 1:NROW(coef),
                      .combine = "rbind") %do% {
                        DELTA_DERIVADA <- foreach(i = 1:NROW(MERCADOS),
                                                  .packages = c("dplyr", "reshape2", "matrixcalc", "corpcor", "foreach", "data.table", "SQUAREM"),
                                                  .combine = "rbind") %dopar% {
                                                    
                                                    dt = list(MERCADO = MERCADOS[i],
                                                              FUN_DELTA = subset(DELTA, RG_UF_ALUNO == MERCADOS[i]), 
                                                              FUN_SIZE = subset(MARKET_SIZE, RG_UF_ALUNO == MERCADOS[i]), 
                                                              FUN_CURSOS = subset(CURSOS, RG_UF_CURSO == MERCADOS[i]), 
                                                              temp = TEMP_MERCADOS[i],
                                                              FUN_DISTANCIAS = DISTANCIAS,
                                                              find_shares = find_shares,
                                                              find_shares_convergence = find_shares_convergence)
                                                    
                                                    new <- find_delta(coef + diag(x = epsilon, nrow = NROW(coef), ncol = NROW(coef))[j,], dt)
                                                    
                                                    return(new)
                                                  }
                        
                        CURSOS_DER <- CURSOS %>%
                          left_join(DELTA_DERIVADA, by = "CO_CURSO_N")
                        
                        demanda_DER <- felm(as.formula(sprintf('%s~%s|%s|%s|%s', 
                                                               "DELTA", 
                                                               paste(exogenous, collapse = '+'), 
                                                               paste(fixed_effects, collapse = '+'), 
                                                               0,
                                                               0)), data = CURSOS_DER)
                        
                        xi_BASE <- as.vector(demanda_DER$residual)
                        xi_ENEM_1 <- as.vector(CURSOS_DER$ENEM_SM1 - CURSOS_DER$ENEM_DT1)
                        xi_ENEM_2 <- as.vector(CURSOS_DER$ENEM_SM2 - CURSOS_DER$ENEM_DT2)
                        xi_ENEM_3 <- as.vector(CURSOS_DER$ENEM_SM3 - CURSOS_DER$ENEM_DT3)
                        xi_ENEM_4 <- as.vector(CURSOS_DER$ENEM_SM4 - CURSOS_DER$ENEM_DT4)
                        xi_RDPC_1 <- as.vector(CURSOS_DER$RDPC_SM1 - CURSOS_DER$RDPC_DT1)
                        xi_RDPC_2 <- as.vector(CURSOS_DER$RDPC_SM2 - CURSOS_DER$RDPC_DT2)
                        xi_RDPC_3 <- as.vector(CURSOS_DER$RDPC_SM3 - CURSOS_DER$RDPC_DT3)
                        xi_RDPC_4 <- as.vector(CURSOS_DER$RDPC_SM4 - CURSOS_DER$RDPC_DT4)
                        xi_COTA_1 <- as.vector(CURSOS_DER$COTA_SM1 - CURSOS_DER$COTA_DT1)
                        xi_COTA_2 <- as.vector(CURSOS_DER$COTA_SM2 - CURSOS_DER$COTA_DT2)
                        xi_DIST_0 <- as.vector(CURSOS_DER$DIST_SM0 - CURSOS_DER$DIST_DT0)
                        
                        G <- cbind(diag(xi_BASE) %*% Z_BASE, 
                                   diag(xi_ENEM_1) %*% Z_ELSE,
                                   diag(xi_ENEM_2) %*% Z_ELSE, 
                                   diag(xi_ENEM_3) %*% Z_ELSE, 
                                   diag(xi_ENEM_4) %*% Z_ELSE, 
                                   diag(xi_RDPC_1) %*% Z_ELSE, 
                                   diag(xi_RDPC_2) %*% Z_ELSE, 
                                   diag(xi_RDPC_3) %*% Z_ELSE, 
                                   diag(xi_RDPC_4) %*% Z_ELSE, 
                                   diag(xi_COTA_1) %*% Z_ELSE,
                                   diag(xi_COTA_2) %*% Z_ELSE,
                                   diag(xi_DIST_0) %*% Z_ELSE)
                        
                        meanG <- t(as.matrix(colMeans(G)) - base_meanG) / epsilon
                        
                        log <- file_download("working papers/Affirmative action/logs/cod05.txt")
                        if (is.null(log) == FALSE){
                          cat(paste("Var:", j, collapse = " "), sep = "\n", file = log, append = TRUE)
                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
                          cat("-----------------", sep = "\n", file = log, append = TRUE)
                          cat("", sep = "\n", file = log, append = TRUE)
                          upload_log(log, "cod05.txt")
                        }
                        return(meanG)}
  stopCluster(myCluster)
  
  variancia <- solve(DERIVADA %*% W %*% t(DERIVADA)) / NROW(Z_BASE)
  erro_padrao <- sqrt(diag(variancia))
  t <- coef / erro_padrao
  pvalor <- 2 * pt(q = abs(t), df = (NROW(Z_BASE) - NROW(coef)), lower.tail = FALSE)
  
  RESULT = list(variancia = variancia, erro_padrao = erro_padrao, t = t, pvalor = pvalor, derivada = DERIVADA)
  return(RESULT)
}