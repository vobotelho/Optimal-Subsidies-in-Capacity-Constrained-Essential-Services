demand_starter <- function(data){
  
  MERCADOS <- data[["MERCADOS"]]
  CURSOS <- data[["CURSOS"]]
  MARKET_SIZE <- data[["MARKET_SIZE"]]
  DISTANCIAS <- data[["DISTANCIAS"]]
  instruments <- data[["instruments"]]
  smm_fe_moments <- data[["smm_fe_moments"]]
  
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
  
  Z <- (1 / NROW(CURSOS)) * as.matrix(bdiag(Z_BASE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE))
  save(Z, file = "test\\Z.RData")
  
  #WEIGHTING MATRIX W (11 SMM CONDITIONS: ENEM1, ENEM2, ENEM3, ENEM4, RDPC1, RDPC2, RDPC3, RDPC4, AA-G1, AA-G2, DIST0)
  W <- diag(ncol(Z))
  save(W, file = "test\\W.RData")
  rm("W")
  rm("Z")
  rm("Z_BASE")
  rm("Z_ELSE")
  
  #CREATING INITIAL MATRICES
  SIZE <- MARKET_SIZE %>%
    group_by(RG_UF_ALUNO) %>%
    summarise(TARGET = sum(SIZE)) %>%
    ungroup() %>%
    rename("RG_UF_CURSO" = "RG_UF_ALUNO") %>%
    mutate(RG_UF_CURSO = as.integer(RG_UF_CURSO))
  
  CURSOS <- CURSOS %>%
    left_join(SIZE, by = "RG_UF_CURSO") %>%
    mutate(TARGET = ALUNOS / TARGET)
  
  SHARE_OUT_ALL <- CURSOS %>%
    group_by(RG_UF_CURSO) %>%
    summarise(SHARE_OUT = (sum(SIZE) - sum(ALUNOS)) / sum(SIZE)) %>%
    ungroup()
  
  for(i in 1:NROW(MERCADOS)) {
    REGIOES <- unique(MARKET_SIZE$RG_INT_ALUNO[MARKET_SIZE$RG_UF_ALUNO == MERCADOS[i]])
    CHOICE_SET <- cross_join(subset(MARKET_SIZE, RG_UF_ALUNO == MERCADOS[i]), select(subset(CURSOS, RG_UF_CURSO == MERCADOS[i]), c("ENEM_MIN_G0", "ENEM_MIN_G1", "ENEM_MIN_G2", "CO_CURSO_N", "RG_INT_CURSO", "PRECO_1000", "PUBLICA", "TARGET"))) %>%
      mutate(CUTOFF = NA,
             CUTOFF = ifelse(COTA == "G0", ENEM_MIN_G0, CUTOFF),
             CUTOFF = ifelse(COTA == "G1", ENEM_MIN_G1, CUTOFF),
             CUTOFF = ifelse(COTA == "G2", ENEM_MIN_G2, CUTOFF)) %>%
      select(-c(RG_UF_ALUNO, ENEM_MIN_G0, ENEM_MIN_G1, ENEM_MIN_G2)) %>%
      left_join(subset(DISTANCIAS, RG_INT_ALUNO %in% REGIOES & RG_INT_CURSO %in% REGIOES), by = c("RG_INT_ALUNO", "RG_INT_CURSO")) %>%
      mutate(A = ifelse(ENEM >= CUTOFF, 1, 0),
             M_RDPC1_PRC = ifelse(RDPC == 1, PRECO_1000, 0),
             M_RPDC2_PRC = ifelse(RDPC == 2, PRECO_1000, 0),
             M_RDPC3_PRC = ifelse(RDPC == 3, PRECO_1000, 0),
             M_RDPC4_PRC = ifelse(RDPC == 4, PRECO_1000, 0),
             M_RDPC5_PRC = ifelse(RDPC == 5, PRECO_1000, 0),
             M_ENEM0_PRC = (ENEM / 100) * PRECO_1000,
             M_ENRD0_PRC = (ENEM / 100) * (RDPC / 5) * PRECO_1000,
             M_DIST0_DIS = distancia,
             M_ENEM0_PUB = (ENEM / 100) * PUBLICA,
             M_RDPC0_PUB = (RDPC / 5) * PUBLICA,
             M_ENEM0_PRI = (ENEM / 100) * (1 - PUBLICA),
             M_RDPC0_PRI = (RDPC / 5) * (1 - PUBLICA)) %>%
      select(-c(CUTOFF, RG_INT_CURSO, RG_INT_ALUNO, PUBLICA, PRECO_1000, distancia, ENEM, RDPC, COTA))
    print("1")
    gc()
    
    SHARE_OUT <- as.double(SHARE_OUT_ALL$SHARE_OUT[SHARE_OUT_ALL$RG_UF_CURSO == MERCADOS[i]])
    save(SHARE_OUT, file = paste0("test\\OUT_", MERCADOS[i], ".RData"))
    
    F_matrix <- dcast(CHOICE_SET, SEGMENT ~ CO_CURSO_N, value.var = "SIZE")
    rownames(F_matrix) <- F_matrix$SEGMENT
    F_matrix$SEGMENT <- NULL
    F_matrix <- Matrix(as.matrix(F_matrix[, 1]), sparse = TRUE)
    save(F_matrix, file = paste0("test\\F_", MERCADOS[i], ".RData"))
    rm("F_matrix")
    CHOICE_SET <- select(CHOICE_SET, -c(SIZE))
    print("2")
    gc()
    
    A_matrix <- dcast(CHOICE_SET, SEGMENT ~ CO_CURSO_N, value.var = "A")
    rownames(A_matrix) <- A_matrix$SEGMENT
    A_matrix$SEGMENT <- NULL
    A_matrix <- Matrix(as.matrix(A_matrix), sparse = TRUE)
    save(A_matrix, file = paste0("test\\A_", MERCADOS[i], ".RData"))
    rm("A_matrix")
    CHOICE_SET <- select(CHOICE_SET, -c(A))
    print("3")
    gc()
    
    TARGET_matrix <- dcast(CHOICE_SET, SEGMENT ~ CO_CURSO_N, value.var = "TARGET")
    TARGET_matrix$SEGMENT <- NULL
    TARGET_matrix <- Matrix(t(as.matrix(TARGET_matrix[1, ])), sparse = TRUE)
    save(TARGET_matrix, file = paste0("test\\TARGET_", MERCADOS[i], ".RData"))
    rm("TARGET_matrix")
    CHOICE_SET <- select(CHOICE_SET, -c(TARGET))
    print("4")
    gc()
    
    MKs <- grep("^M_", colnames(CHOICE_SET), value = TRUE)
    Mk_matrix <- foreach(j = 1:NROW(MKs)) %do% {
      Matrix <- dcast(select(CHOICE_SET, c(SEGMENT, CO_CURSO_N, all_of(MKs[j]))), SEGMENT ~ CO_CURSO_N, value.var = MKs[j])
      CHOICE_SET <- select(CHOICE_SET, -c(all_of(MKs[j])))
      rownames(Matrix) <- Matrix$SEGMENT
      Matrix$SEGMENT <- NULL
      Matrix <- Matrix(as.matrix(Matrix), sparse = TRUE)
      gc()
      return(Matrix)
    }
    save(Mk_matrix, file = paste0("test\\MK_", MERCADOS[i], ".RData"))
    rm("Mk_matrix")
    rm("CHOICE_SET")
    print("5")
    gc()
  }
}