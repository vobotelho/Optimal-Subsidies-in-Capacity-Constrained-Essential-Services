find_delta <- function(MERCADO, M, temp){
  
  load(paste0("test\\F_", MERCADO, ".RData"))
  load(paste0("test\\A_", MERCADO, ".RData"))
  load(paste0("test\\TARGET_", MERCADO, ".RData"))
  load(paste0("test\\OUT_", MERCADO, ".RData"))
  F_matrix <- F_matrix / sum(F_matrix)
  
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
  OUTPUT <- data.frame(DELTA = as.double(RESULT$par), CO_CURSO_N = as.double(rownames(RESULT$par)))
  
  return(OUTPUT)
}