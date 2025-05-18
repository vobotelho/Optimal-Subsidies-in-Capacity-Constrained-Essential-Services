gmm_criterion <- function(coef, data){
  
  MERCADOS <- data[["MERCADOS"]]
  DELTA <- data[["DELTA"]]
  CURSOS <- data[["CURSOS"]]
  DISTANCIAS <- data[["DISTANCIAS"]]
  MARKET_SIZE <- data[["MARKET_SIZE"]]
  TEMP_MERCADOS <- data[["TEMP_MERCADOS"]]
  log <- data[["LOG"]]
  exogenous <- data[["exogenous"]]
  fixed_effects <- data[["fixed_effects"]]
  demean <- data[["functions"]]$demean
  find_delta <- data[["functions"]]$find_delta
  options <- data[["options"]]
  
  myCluster <- makeCluster(8, type = "PSOCK")
  registerDoParallel(myCluster)
  DELTAS <- foreach(i = 1:NROW(MERCADOS),
                    .packages = c("dplyr", "reshape2", "matrixcalc", "corpcor", "foreach", "data.table", "SQUAREM")) %dopar% {
                      
                      load(paste0("local\\cod04_MK_", MERCADOS[i], ".RData"))
                      M <- foreach(j = 1:length(Mk_matrix),
                                   .combine = "+") %do% {
                        partial <- as.matrix(coef[j] * Mk_matrix[[j]])
                        return(partial)
                      }
                      rm("Mk_matrix")
                      new <- find_delta(MERCADOS[i], M, TEMP_MERCADOS[i])
                      return(new)
                    }
  stopCluster(myCluster)
  
  CURSOS <- CURSOS %>%
    left_join(DELTAS, by = "CO_CURSO_N")
  
  demanda <- felm(as.formula(sprintf('%s~%s|%s|%s|%s', 
                                     "DELTA", 
                                     paste(exogenous, collapse = '+'), 
                                     paste(fixed_effects, collapse = '+'), 
                                     0,
                                     0)), data = CURSOS)
  
  x_BASE <- rbind(as.matrix(demanda$residuals),
                  as.matrix(CURSOS$ENEM_SM1 - CURSOS$ENEM_DT1),
                  as.matrix(CURSOS$ENEM_SM2 - CURSOS$ENEM_DT2),
                  as.matrix(CURSOS$ENEM_SM3 - CURSOS$ENEM_DT3),
                  as.matrix(CURSOS$ENEM_SM4 - CURSOS$ENEM_DT4),
                  as.matrix(CURSOS$RDPC_SM1 - CURSOS$RDPC_DT1),
                  as.matrix(CURSOS$RDPC_SM2 - CURSOS$RDPC_DT2),
                  as.matrix(CURSOS$RDPC_SM3 - CURSOS$RDPC_DT3),
                  as.matrix(CURSOS$RDPC_SM4 - CURSOS$RDPC_DT4),
                  as.matrix(CURSOS$COTA_SM1 - CURSOS$COTA_DT1),
                  as.matrix(CURSOS$COTA_SM2 - CURSOS$COTA_DT2),
                  as.matrix(CURSOS$DIST_SM0 - CURSOS$DIST_DT0))
  
  meanG <- t(x_BASE) %*% Z
  
  J = meanG %*% W %*% t(meanG)
  
  if (options == "J"){
    RESULT = J
  } else {
    G <- NROW(CURSOS) * cbind(diag(xi_BASE) %*% Z_BASE, 
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
    
    demeanG <- foreach (i = 1:ncol(G),
                        .combine = "cbind") %do% {
                          new <- G[, i] - meanG[i]
                          return(new)}
    
    VAR <- foreach (i = 1:nrow(demeanG),
                    .combine = "+") %do% {
                      new <- (1 / nrow(demeanG)) * demeanG[i, ] %*% t(demeanG[i, ])
                      return(new)}
    
    W_update <- solve(VAR)
    
    RESULT = list(J = J, W_update = W_update, DELTAS = DELTAS, modelo = demanda)
  }
  
  return(RESULT)
}