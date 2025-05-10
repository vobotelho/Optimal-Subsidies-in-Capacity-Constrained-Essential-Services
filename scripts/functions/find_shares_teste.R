find_shares <- function(CHOICE_SET, TOTAL_ALUNOS){
  
  SHARES <- CHOICE_SET %>%
    mutate(Vid = DELTA_EXP * INDIVIDUAL) %>%
    group_by(SEGMENT) %>%
    mutate(PROB = SIZE * Vid / (1 + sum(Vid))) %>%
    ungroup() %>%
    group_by(CO_CURSO_N) %>%
    summarise(ENEM_SMM = weighted.mean(x = ENEM, w = PROB) / 100,
              ENEM_SM1 = sum(PROB[ENEM > 0 & ENEM <= 20], na.rm = TRUE) / sum(PROB),
              ENEM_SM2 = sum(PROB[ENEM > 20 & ENEM <= 40], na.rm = TRUE) / sum(PROB),
              ENEM_SM3 = sum(PROB[ENEM > 40 & ENEM <= 60], na.rm = TRUE) / sum(PROB),
              ENEM_SM4 = sum(PROB[ENEM > 60 & ENEM <= 80], na.rm = TRUE) / sum(PROB),
              ENEM_SM5 = sum(PROB[ENEM > 80 & ENEM <= 100], na.rm = TRUE) / sum(PROB),
              RDPC_SM1 = sum(PROB[RDPC == 1], na.rm = TRUE) / sum(PROB),
              RDPC_SM2 = sum(PROB[RDPC == 2], na.rm = TRUE) / sum(PROB),
              RDPC_SM3 = sum(PROB[RDPC == 3], na.rm = TRUE) / sum(PROB),
              RDPC_SM4 = sum(PROB[RDPC == 4], na.rm = TRUE) / sum(PROB),
              RDPC_SM5 = sum(PROB[RDPC == 5], na.rm = TRUE) / sum(PROB),
              COTA_SM0 = sum(PROB[COTA == "G0"], na.rm = TRUE) / sum(PROB),
              COTA_SM1 = sum(PROB[COTA == "G1"], na.rm = TRUE) / sum(PROB),
              COTA_SM2 = sum(PROB[COTA == "G2"], na.rm = TRUE) / sum(PROB),
              DIST_SM0 = sum(PROB[distancia == 0], na.rm = TRUE) / sum(PROB),
              DIST_SMM = weighted.mean(x = distancia, w = PROB),
              SHARE_0 = sum(PROB)) %>%
    ungroup() %>%
    mutate(SHARE_0 = SHARE_0 / TOTAL_ALUNOS)
  
  return(SHARES)}