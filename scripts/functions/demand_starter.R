demand_starter <- function(data){
  
  MERCADOS <- data[["MERCADOS"]]
  DELTA <- data[["DELTA"]]
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
  
  Z <- as.matrix(bdiag(Z_BASE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE, Z_ELSE))
  save(Z, file = "local\\cod04_Z.RData")
  save(Z_BASE, file = "local\\cod04_Z_BASE.RData")
  save(Z_ELSE, file = "local\\cod04_Z_ELSE.RData")
  
  #WEIGHTING MATRIX W (11 SMM CONDITIONS: ENEM1, ENEM2, ENEM3, ENEM4, RDPC1, RDPC2, RDPC3, RDPC4, AA-G1, AA-G2, DIST0)
  W <- diag(ncol(Z))
  save(W, file = "local\\cod04_W0.RData")
  save(W, file = "local\\cod04_W.RData")
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
    summarise(SHARE_OUT = 1 - sum(TARGET)) %>%
    ungroup()
  
  SEGMENT_master <- sort(unique(MARKET_SIZE$SEGMENT))
  CO_CURSO_master <- sort(unique(CURSOS$CO_CURSO_N))
  setDT(MARKET_SIZE)
  setDT(CURSOS)
  setDT(DISTANCIAS)
  setDT(DELTA)
  
  for(i in 1:NROW(MERCADOS)) {
    REGIOES <- MARKET_SIZE[
      RG_UF_ALUNO == MERCADOS[i],
      unique(RG_INT_ALUNO)
    ]
    
    SUB_MARKET_SIZE <- MARKET_SIZE[RG_UF_ALUNO == MERCADOS[i]]
    
    SUB_MARKET_SIZE <- SUB_MARKET_SIZE[, `:=`(
      Gind_01 = fifelse(ENEM > 0 & ENEM <= 20, 1, 0),
      Gind_02 = fifelse(ENEM > 20 & ENEM <= 40, 1, 0),
      Gind_03 = fifelse(ENEM > 40 & ENEM <= 60, 1, 0),
      Gind_04 = fifelse(ENEM > 60 & ENEM <= 80, 1, 0),
      Gind_05 = fifelse(RDPC == 1, 1, 0),
      Gind_06 = fifelse(RDPC == 2, 1, 0),
      Gind_07 = fifelse(RDPC == 3, 1, 0),
      Gind_08 = fifelse(RDPC == 4, 1, 0),
      Gind_09 = fifelse(COTA == "G1", 1, 0),
      Gind_10 = fifelse(COTA == "G2", 1, 0)
    )]
    
    SUB_CURSOS <- CURSOS[
      RG_UF_CURSO == MERCADOS[i],
      .(CO_CURSO_N,
        RG_INT_CURSO,
        PRECO_1000,
        PUBLICA,
        TARGET,
        ENEM_MIN_G0,
        ENEM_MIN_G1,
        ENEM_MIN_G2)
      ]
    
    SUB_MARKET_SIZE[, tmp_key := 1]
    SUB_CURSOS[, tmp_key := 1]
    
    CHOICE_SET <- merge(
      SUB_MARKET_SIZE,
      SUB_CURSOS,
      by = "tmp_key",
      allow.cartesian = TRUE
    )
    
    CHOICE_SET[, CUTOFF := fifelse(COTA == "G0", ENEM_MIN_G0,
                                   fifelse(COTA == "G1", ENEM_MIN_G1,
                                           fifelse(COTA == "G2", ENEM_MIN_G2,
                                                   NA_real_)))]
    
    CHOICE_SET[, c("RG_UF_ALUNO",
                   "ENEM_MIN_G0","ENEM_MIN_G1","ENEM_MIN_G2") := NULL]
    
    setkey(DISTANCIAS, RG_INT_ALUNO, RG_INT_CURSO)
    CHOICE_SET <- DISTANCIAS[
      CHOICE_SET,
      on = .(RG_INT_ALUNO, RG_INT_CURSO)
    ]
    
    SUB_DELTA <- DELTA[RG_INT_ALUNO %in% REGIOES, !"RG_UF_ALUNO"]
    setkey(SUB_DELTA, 
           COTA, RDPC, ENEM, RG_INT_ALUNO, CO_CURSO_N)
    
    CHOICE_SET <- SUB_DELTA[
      CHOICE_SET,
      on = .(COTA, RDPC, ENEM, RG_INT_ALUNO, CO_CURSO_N)
    ]
    
    CHOICE_SET[, `:=`(
      PROB_CHOICE = fifelse(is.na(N), 0, N / SIZE),
      A = as.integer(ENEM >= CUTOFF),
      M_RDPC1_PRC = fifelse(RDPC == 1, PRECO_1000, 0),
      M_RDPC2_PRC = fifelse(RDPC == 2, PRECO_1000, 0),
      M_RDPC3_PRC = fifelse(RDPC == 3, PRECO_1000, 0),
      M_RDPC4_PRC = fifelse(RDPC == 4, PRECO_1000, 0),
      M_RDPC5_PRC = fifelse(RDPC == 5, PRECO_1000, 0),
      M_ENEM0_PRC = (ENEM / 100) * PRECO_1000,
      M_DIST0_PRC = distancia,
      Gcross_01 = fifelse(distancia == 0, 1, 0)
    )]
    
    CHOICE_SET[, c("CUTOFF",
                   "RG_INT_CURSO",
                   "RG_INT_ALUNO",
                   "PUBLICA",
                   "PRECO_1000",
                   "distancia",
                   "ENEM",
                   "RDPC",
                   "COTA") := NULL]
    
    SEGMENT_levels <- SEGMENT_master[SEGMENT_master %in% CHOICE_SET$SEGMENT]
    CO_CURSO_N_levels <- CO_CURSO_master[CO_CURSO_master  %in% CHOICE_SET$CO_CURSO_N]
    n_i <- length(SEGMENT_levels)
    n_j <- length(CO_CURSO_N_levels)
    
    SHARE_OUT <- as.double(SHARE_OUT_ALL$SHARE_OUT[SHARE_OUT_ALL$RG_UF_CURSO == MERCADOS[i]])
    save(SHARE_OUT, file = paste0("local\\cod04_OUT_", MERCADOS[i], ".RData"))
    save(SEGMENT_levels, file = paste0("local\\cod04_SEGMENT_levels_", MERCADOS[i], ".RData"))
    save(CO_CURSO_N_levels, file = paste0("local\\cod04_CURSO_levels_", MERCADOS[i], ".RData"))
    
    CHOICE_SET[, `:=`(
      i = as.integer(factor(SEGMENT, levels = SEGMENT_levels)),
      j = as.integer(factor(CO_CURSO_N, levels = CO_CURSO_N_levels))
    )]
    
    SUB_MARKET_SIZE[, `:=`(
      i = as.integer(factor(SEGMENT, levels = SEGMENT_levels))
    )]
    
    SUB_CURSOS[, `:=`(
      i = as.integer(factor(CO_CURSO_N, levels = CO_CURSO_N_levels))
    )]
    
    F_matrix <- sparseMatrix(
      i = SUB_MARKET_SIZE[["i"]],
      j = rep(1L, n_i),
      x = SUB_MARKET_SIZE[["SIZE"]],
      dims = c(n_i, 1L),
      dimnames = list(
        levels(factor(SEGMENT_levels)),
        "value_I")
      )
    F_matrix <- F_matrix / sum(F_matrix)
    save(F_matrix, file = paste0("local\\cod04_F_", MERCADOS[i], ".RData"))
    
    A_matrix <- sparseMatrix(
      i        = CHOICE_SET[["i"]],
      j        = CHOICE_SET[["j"]],
      x        = CHOICE_SET[["A"]],
      dimnames = list(
        levels(factor(SEGMENT_levels)),
        levels(factor(CO_CURSO_N_levels))
      )
    )
    save(A_matrix, file = paste0("local\\cod04_A_", MERCADOS[i], ".RData"))
    rm("A_matrix")
    CHOICE_SET[, A := NULL]
    
    TARGET_matrix <- sparseMatrix(
      i = SUB_CURSOS[["i"]],
      j = rep(1L, n_j),
      x = SUB_CURSOS[["TARGET"]],
      dims = c(n_j, 1L),
      dimnames = list(
        levels(factor(CO_CURSO_N_levels)),
        levels("value_J")
      )
    )
    save(TARGET_matrix, file = paste0("local\\cod04_TARGET_", MERCADOS[i], ".RData"))
    rm("TARGET_matrix")
    
    Ginds <- grep("^Gind_", colnames(SUB_MARKET_SIZE), value = TRUE)
    Gind_matrix <- foreach(j = 1:NROW(Ginds)) %do% {
      Matrix <- sparseMatrix(
        i = SUB_MARKET_SIZE[["i"]],
        j = rep(1L, n_i),
        x = SUB_MARKET_SIZE[[Ginds[j]]],
        dims = c(n_i, 1L),
        dimnames = list(
          levels(factor(SEGMENT_levels)),
          "value_I"
        )
      )
      SUB_MARKET_SIZE[, (Ginds[j]) := NULL]
      return(Matrix)
    }
    save(Gind_matrix, file = paste0("local\\cod04_Gind_", MERCADOS[i], ".RData"))
    
    Gs <- grep("^Gcross_", colnames(CHOICE_SET), value = TRUE)
    Gcross_matrix <- foreach(j = 1:NROW(Gs)) %do% {
      Matrix <- sparseMatrix(
        i        = CHOICE_SET[["i"]],
        j        = CHOICE_SET[["j"]],
        x        = CHOICE_SET[[Gs[j]]],
        dims = c(n_i, n_j),
        dimnames = list(
          levels(factor(SEGMENT_levels)),
          levels(factor(CO_CURSO_N_levels))
        )
      )
      CHOICE_SET[, (Gs[j]) := NULL]
      return(Matrix)
    }
    save(Gcross_matrix, file = paste0("local\\cod04_Gcross_", MERCADOS[i], ".RData"))
    
    S_matrix <- sparseMatrix(
      i        = CHOICE_SET[["i"]],
      j        = CHOICE_SET[["j"]],
      x        = CHOICE_SET[["PROB_CHOICE"]],
      dimnames = list(
        levels(factor(SEGMENT_levels)),
        levels(factor(CO_CURSO_N_levels))
      )
    )
    CHOICE_SET[, PROB_CHOICE := NULL]
    
    PROP_matrix <- S_matrix * as.vector(F_matrix)
    PROP_matrix <- t(PROP_matrix) / as.vector(colSums(PROP_matrix))
    GTot <- NROW(Ginds) + NROW(Gs)
    MOMENTS <- foreach(j = 1:GTot) %do% {
      if (j <= NROW(Ginds)){
        Matrix <- PROP_matrix %*% Gind_matrix[[j]]
      } else {
        Matrix <- as.matrix(rowSums(PROP_matrix * t(Gcross_matrix[[j - NROW(Ginds)]])))
      }
      return(Matrix)
    }
    save(MOMENTS, file = paste0("local\\cod04_MOMENTS_", MERCADOS[i], ".RData"))
    rm("MOMENTS")
    rm("F_matrix")
    rm("S_matrix")
    rm("PROP_matrix")
    rm("Gind_matrix")
    rm("Gcross_matrix")
    
    MKs <- grep("^M_", colnames(CHOICE_SET), value = TRUE)
    Mk_matrix <- foreach(j = 1:NROW(MKs)) %do% {
      Matrix <- sparseMatrix(
        i = CHOICE_SET[["i"]],
        j = CHOICE_SET[["j"]],
        x = CHOICE_SET[[MKs[j]]],
        dims = c(n_i, n_j),
        dimnames = list(
          levels(factor(SEGMENT_levels)),
          levels(factor(CO_CURSO_N_levels))
        )
      )
      CHOICE_SET[, (MKs[j]) := NULL]
      gc()
      return(Matrix)
    }
    save(Mk_matrix, file = paste0("local\\cod04_MK_", MERCADOS[i], ".RData"))
    rm("Mk_matrix")
    
    rm("CHOICE_SET")
    gc()
  }
}