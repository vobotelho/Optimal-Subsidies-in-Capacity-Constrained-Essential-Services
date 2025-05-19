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
  
  load("local\\cod04_Z.RData")
  
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
  
  DELTAS_PAR <- do.call(rbind, lapply(DELTAS, `[[`, 1))
  MOMENTS <- do.call(rbind, lapply(DELTAS, `[[`, 2))
  
  CURSOS <- CURSOS %>%
    left_join(DELTAS_PAR, by = "CO_CURSO_N")
  
  demanda <- felm(as.formula(sprintf('%s~%s|%s|%s|%s', 
                                     "DELTA", 
                                     paste(exogenous, collapse = '+'), 
                                     paste(fixed_effects, collapse = '+'), 
                                     0,
                                     0)), data = CURSOS)
  
  x_BASE <- rbind(as.matrix(demanda$residuals),
                  as.matrix(MOMENTS[,1]),
                  as.matrix(MOMENTS[,2]),
                  as.matrix(MOMENTS[,3]),
                  as.matrix(MOMENTS[,4]),
                  as.matrix(MOMENTS[,5]),
                  as.matrix(MOMENTS[,6]),
                  as.matrix(MOMENTS[,7]),
                  as.matrix(MOMENTS[,8]),
                  as.matrix(MOMENTS[,9]),
                  as.matrix(MOMENTS[,10]),
                  as.matrix(MOMENTS[,11]))
  
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