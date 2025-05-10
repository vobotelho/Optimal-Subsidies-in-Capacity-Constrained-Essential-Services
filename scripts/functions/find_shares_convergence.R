find_shares_convergence <- function(CHOICE_SET, TOTAL_ALUNOS){
  
  SHARES <- CHOICE_SET %>%
    mutate(Vid = DELTA_EXP * INDIVIDUAL) %>%
    group_by(SEGMENT) %>%
    mutate(PROB = SIZE * Vid / (1 + sum(Vid))) %>%
    ungroup() %>%
    group_by(CO_CURSO_N) %>%
    summarise(SHARE_0 = sum(PROB),
              DELTA = mean(DELTA)) %>%
    ungroup() %>%
    mutate(SHARE_0 = SHARE_0 / TOTAL_ALUNOS)
  
  return(SHARES)}
