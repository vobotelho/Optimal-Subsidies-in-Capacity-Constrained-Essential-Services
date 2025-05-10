find_delta <- function(coef, dt){
  
  MERCADO <- dt[["MERCADO"]]
  FUN_DELTA <- dt[["FUN_DELTA"]]
  FUN_SIZE = dt[["FUN_SIZE"]]
  FUN_CURSOS = dt[["FUN_CURSOS"]]
  FUN_DISTANCIAS = dt[["FUN_DISTANCIAS"]]
  find_shares <- dt[["find_shares"]]
  find_shares_convergence <- dt[["find_shares_convergence"]]
  temp <- dt[["temp"]]
  tol_erro = 10^(-12)
  
  TOTAL_ALUNOS <- sum(FUN_SIZE$SIZE)
  SHARE_OUT <- (TOTAL_ALUNOS - sum(FUN_CURSOS$ALUNOS)) / TOTAL_ALUNOS
  
  FUN_CURSOS <- FUN_CURSOS %>%
    mutate(TARGET = ALUNOS / TOTAL_ALUNOS)
  
  CHOICE_SET <- cross_join(FUN_SIZE, select(FUN_CURSOS, c("ENEM_MIN_G0", "ENEM_MIN_G1", "ENEM_MIN_G2", "CO_CURSO_N", "RG_INT_CURSO", "PRECO_1000", "PUBLICA", "PRIVADA"))) %>%
    subset((ENEM >= ENEM_MIN_G0 & COTA == "G0") | (ENEM >= ENEM_MIN_G1 & COTA == "G1") | (ENEM >= ENEM_MIN_G2 & COTA == "G2")) %>%
    mutate(ALPHA = coef[RDPC] + coef[6] * ENEM / 100 + coef[7] * RDPC * ENEM / 500,
           BETA_DIS = coef[8],
           BETA_PUB = coef[9] * ENEM / 100 + coef[10] * RDPC / 5,
           BETA_PRI = coef[11] * ENEM / 100 + coef[12] * RDPC / 5) %>%
    left_join(FUN_DISTANCIAS, by = c("RG_INT_ALUNO", "RG_INT_CURSO")) %>%
    mutate(INDIVIDUAL = exp(ALPHA * PRECO_1000 + BETA_DIS * distancia + BETA_PUB * PUBLICA + BETA_PRI * PRIVADA)) %>%
    select(-c(ENEM_MIN_G0, ENEM_MIN_G1, ENEM_MIN_G2, RG_INT_CURSO, RG_INT_ALUNO, RG_UF_ALUNO, ALPHA, BETA_DIS, BETA_PUB, BETA_PRI, PUBLICA, PRIVADA, PRECO_1000))
  
  load(temp)
  if (RESULT[1] == "Start"){
    DELTA_START <- as.vector(log(FUN_CURSOS$TARGET) - log(SHARE_OUT))
  } else {
    DELTA_START <- RESULT$par
  }
  
  contraction <- function(DELTA_TRY){
    FUN_CURSOS_D <- FUN_CURSOS %>%
      select("CO_CURSO_N") %>%
      mutate(DELTA = DELTA_TRY,
             DELTA_EXP = exp(DELTA_TRY))
    
    SHARES <- find_shares_convergence(select(CHOICE_SET, -c(ENEM, RDPC, COTA)) %>% left_join(FUN_CURSOS_D, by = "CO_CURSO_N"), TOTAL_ALUNOS) %>%
      left_join(select(FUN_CURSOS, c("CO_CURSO_N", "TARGET")), by = "CO_CURSO_N") %>%
      mutate(NEW_DELTA = DELTA + log(TARGET) - log(SHARE_0))
    
    FUN_CURSOS_D <- FUN_CURSOS_D %>%
      left_join(select(SHARES, c("CO_CURSO_N", "NEW_DELTA")), by = "CO_CURSO_N")
    
    RESULT <- as.vector(FUN_CURSOS_D$NEW_DELTA)
    return(RESULT)
  }
  
  RESULT <- squarem(DELTA_START, contraction, control = list(tol = 1e-12, trace = TRUE))
  save(RESULT, file = temp)
  
  FUN_CURSOS <- FUN_CURSOS %>%
    mutate(DELTA = RESULT$par,
           DELTA_EXP = exp(DELTA))
  
  SHARES <- find_shares(CHOICE_SET %>% 
                          left_join(select(FUN_CURSOS, c("CO_CURSO_N", "DELTA_EXP")), by = "CO_CURSO_N"), 
                        TOTAL_ALUNOS)
  
  FUN_CURSOS <- FUN_CURSOS %>%
    select(c(CO_CURSO_N, DELTA)) %>%
    left_join(select(SHARES, c("CO_CURSO_N", "ENEM_SMM", "ENEM_SM1", "ENEM_SM2", "ENEM_SM3", "ENEM_SM4", "ENEM_SM5", "RDPC_SM1", "RDPC_SM2", "RDPC_SM3", "RDPC_SM4", "RDPC_SM5", "COTA_SM0", "COTA_SM1", "COTA_SM2", "DIST_SM0", "DIST_SMM")), by = "CO_CURSO_N")
  
  return(FUN_CURSOS)
}