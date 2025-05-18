find_delta <- function(MERCADO, M, temp){
  
  load(paste0("local\\cod04_F_", MERCADO, ".RData"))
  load(paste0("local\\cod04_A_", MERCADO, ".RData"))
  load(paste0("local\\cod04_TARGET_", MERCADO, ".RData"))
  load(paste0("local\\cod04_OUT_", MERCADO, ".RData"))
  load(paste0("local\\cod04_CURSO_levels_", MERCADO, ".RData"))
  load(paste0("local\\cod04_Gind_", MERCADO, ".RData"))
  load(paste0("local\\cod04_Gcross_", MERCADO, ".RData"))
  load(paste0("local\\cod04_MOMENTS_", MERCADO, ".RData"))
  
  load(temp)
  if (RESULT[1] == "Start"){
    DELTA_START <- as.matrix(log(TARGET_matrix) - log(SHARE_OUT))
  } else {
    DELTA_START <- as.matrix(RESULT$par)
  }
  
  contraction <- function(DELTA_TRY){
    V <- A_matrix * exp(kronecker(matrix(1, nrow = nrow(F_matrix), ncol = 1), t(DELTA_TRY)) + M)
    VauxINV <- as.vector(V %*% matrix(1, nrow = nrow(DELTA_TRY), ncol = 1) + matrix(1, nrow = nrow(F_matrix), ncol = 1))
    S <- V / as.vector(VauxINV)
    SHARES <- t(S) %*% F_matrix
    NEW_DELTA <- DELTA_TRY + log(TARGET_matrix) - log(SHARES)
    return(as.matrix(NEW_DELTA))
  }
  
  RESULT <- squarem(DELTA_START, contraction, control = list(tol = 1e-12, trace = TRUE))
  save(RESULT, file = temp)
  
  #Calculating moments
  DELTA_TRY <- RESULT$par
  V <- A_matrix * exp(kronecker(matrix(1, nrow = nrow(F_matrix), ncol = 1), t(DELTA_TRY)) + M)
  VauxINV <- as.vector(V %*% matrix(1, nrow = nrow(DELTA_TRY), ncol = 1) + matrix(1, nrow = nrow(F_matrix), ncol = 1))
  S <- V / as.vector(VauxINV)
  
  PROP_matrix <- S * as.vector(F_matrix)
  PROP_matrix <- t(PROP_matrix) / as.vector(colSums(PROP_matrix))
  GTot <- length(Gind_matrix) + length(Gcross_matrix)
  MOMENTS_SMM <- foreach(j = 1:GTot,
                         .combine = "rbind") %do% {
    if (j <= length(Gind_matrix)){
      Matrix <- PROP_matrix %*% Gind_matrix[[j]]
    } else {
      Matrix <- rowSums(PROP_matrix * t(Gcross_matrix[[j - length(Gind_matrix)]]))
    }
    
    Matrix <- Matrix - MOMENTS[[j]]
    
    return(Matrix)
  }
  
  DELTA_PAR <- data.frame(CO_CURSO_N = CO_CURSO_N_levels, 
                          DELTA = as.double(RESULT$par))
  
  OUTPUT <- list(DELTA_PAR = DELTA_PAR,
                 MOMENTS_SMM = MOMENTS_SMM)
  
  return(OUTPUT)
}