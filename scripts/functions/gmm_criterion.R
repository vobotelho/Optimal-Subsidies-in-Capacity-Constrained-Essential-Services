gmm_criterion <- function(coef, data){
  
  MERCADOS <- data[["MERCADOS"]]
  DELTA <- data[["DELTA"]]
  CURSOS <- data[["CURSOS"]]
  DISTANCIAS <- data[["DISTANCIAS"]]
  MARKET_SIZE <- data[["MARKET_SIZE"]]
  TEMP_MERCADOS <- data[["TEMP_MERCADOS"]]
  W <- data[["W"]]
  log <- data[["log"]]
  instruments <- data[["instruments"]]
  exogenous <- data[["exogenous"]]
  fixed_effects <- data[["fixed_effects"]]
  smm_fe_moments <- data[["smm_fe_moments"]]
  demean <- data[["functions"]]$demean
  find_delta <- data[["functions"]]$find_delta
  find_shares <- data[["functions"]]$find_shares
  find_shares_convergence <- data[["functions"]]$find_shares_convergence
  options <- data[["options"]]
  
  log <- file_download("working papers/Affirmative action/logs/cod04.txt")
  if (is.null(log) == FALSE){
    cat(paste("trying coefs:", paste(round(coef, 4), collapse = " ")), sep = "\n", file = log, append = TRUE)
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
    cat("-----------------", sep = "\n", file = log, append = TRUE)
    cat("", sep = "\n", file = log, append = TRUE)
    upload_log(log, "cod04.txt")
  }
  
  Z_BASE <- as.matrix(select(CURSOS, all_of(instruments)))
  Z_ELSE <- foreach(j = 1:NROW(smm_fe_moments),
                    .combine = "cbind") %do% {
                      
                      #REMOVE FIRST DUMMY (TO AVOID MULTICOLLINEARITY)
                      new <- dummy_columns(select(CURSOS, smm_fe_moments[j]) %>% rename("FE" = smm_fe_moments[j]), 
                                           remove_first_dummy = TRUE,
                                           remove_selected_columns = TRUE)
                      
                      new <- as.matrix(new)
                      return(new)
                    }
  #ADD CONSTANT TO Z_ELSE
  Z_ELSE <- cbind(Z_ELSE, matrix(1, nrow = nrow(Z_ELSE), ncol = 1))
  Z_ELSE <- cbind(Z_ELSE, as.matrix(select(CURSOS, all_of(exogenous))))
  
  #WEIGHTING MATRIX W (11 SMM CONDITIONS: ENEM1, ENEM2, ENEM3, ENEM4, RDPC1, RDPC2, RDPC3, RDPC4, AA-G1, AA-G2, DIST0)
  if (is.null(W) == TRUE){
    W <- diag(ncol(Z_BASE) + 11 * ncol(Z_ELSE))
    }
  
  myCluster <- makeCluster(2, type = "PSOCK")
  registerDoParallel(myCluster)
  DELTAS <- foreach(i = 1:NROW(MERCADOS),
                    .packages = c("dplyr", "reshape2", "matrixcalc", "corpcor", "foreach", "data.table", "SQUAREM"),
                    .combine = "rbind") %dopar% {
                      
                      dt <- list(MERCADO = MERCADOS[i],
                                 FUN_DELTA = subset(DELTA, RG_UF_ALUNO == MERCADOS[i]), 
                                 FUN_SIZE = subset(MARKET_SIZE, RG_UF_ALUNO == MERCADOS[i]), 
                                 FUN_CURSOS = subset(CURSOS, RG_UF_CURSO == MERCADOS[i]), 
                                 temp = TEMP_MERCADOS[i],
                                 FUN_DISTANCIAS = DISTANCIAS,
                                 find_shares = find_shares,
                                 find_shares_convergence = find_shares_convergence)
                      
                      new <- find_delta(coef, dt)
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
  
  xi_BASE <- as.vector(demanda$residual)
  xi_ENEM_1 <- as.vector(CURSOS$ENEM_SM1 - CURSOS$ENEM_DT1)
  xi_ENEM_2 <- as.vector(CURSOS$ENEM_SM2 - CURSOS$ENEM_DT2)
  xi_ENEM_3 <- as.vector(CURSOS$ENEM_SM3 - CURSOS$ENEM_DT3)
  xi_ENEM_4 <- as.vector(CURSOS$ENEM_SM4 - CURSOS$ENEM_DT4)
  xi_RDPC_1 <- as.vector(CURSOS$RDPC_SM1 - CURSOS$RDPC_DT1)
  xi_RDPC_2 <- as.vector(CURSOS$RDPC_SM2 - CURSOS$RDPC_DT2)
  xi_RDPC_3 <- as.vector(CURSOS$RDPC_SM3 - CURSOS$RDPC_DT3)
  xi_RDPC_4 <- as.vector(CURSOS$RDPC_SM4 - CURSOS$RDPC_DT4)
  xi_COTA_1 <- as.vector(CURSOS$COTA_SM1 - CURSOS$COTA_DT1)
  xi_COTA_2 <- as.vector(CURSOS$COTA_SM2 - CURSOS$COTA_DT2)
  xi_DIST_0 <- as.vector(CURSOS$DIST_SM0 - CURSOS$DIST_DT0)
  
  meanG <- t(cbind((1 / NROW(xi_BASE)) * t(xi_BASE) %*% Z_BASE, 
                   (1 / NROW(xi_ENEM_1)) * t(xi_ENEM_1) %*% Z_ELSE, 
                   (1 / NROW(xi_ENEM_2)) * t(xi_ENEM_2) %*% Z_ELSE, 
                   (1 / NROW(xi_ENEM_3)) * t(xi_ENEM_3) %*% Z_ELSE, 
                   (1 / NROW(xi_ENEM_4)) * t(xi_ENEM_4) %*% Z_ELSE, 
                   (1 / NROW(xi_RDPC_1)) * t(xi_RDPC_1) %*% Z_ELSE, 
                   (1 / NROW(xi_RDPC_2)) * t(xi_RDPC_2) %*% Z_ELSE, 
                   (1 / NROW(xi_RDPC_3)) * t(xi_RDPC_3) %*% Z_ELSE, 
                   (1 / NROW(xi_RDPC_4)) * t(xi_RDPC_4) %*% Z_ELSE, 
                   (1 / NROW(xi_COTA_1)) * t(xi_COTA_1) %*% Z_ELSE, 
                   (1 / NROW(xi_COTA_2)) * t(xi_COTA_2) %*% Z_ELSE, 
                   (1 / NROW(xi_DIST_0)) * t(xi_DIST_0) %*% Z_ELSE))
  
  J = t(meanG) %*% W %*% meanG
  
  log <- file_download("working papers/Affirmative action/logs/cod04.txt")
  if (is.null(log) == FALSE){
    cat(paste("coef:", paste(round(coef, 4), collapse = " ")), sep = "\n", file = log, append = TRUE)
    cat(paste("J:", paste(round(J, 8))), sep = "\n", file = log, append = TRUE)
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
    cat("-----------------", sep = "\n", file = log, append = TRUE)
    cat("", sep = "\n", file = log, append = TRUE)
    upload_log(log, "cod04.txt")
  }
  
  if (options == "J"){
    RESULT = J
  } else {
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