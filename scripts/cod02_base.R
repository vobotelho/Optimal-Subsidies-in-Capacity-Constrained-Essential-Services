"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")
log <- tempfile(fileext = ".txt")

"------------------------------------------------------------"
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod01_RAIS.RData")))

load(file_download(working_file("cod01_MAPAS.RData")))

cod02_MUNICIPIOS <- read_excel(file_download("data/GEO/RELATORIO_DTB_BRASIL_MUNICIPIO.xls"))

CES_COMPLEMENTAR <- data.frame(fread(file_download("data/CES/DM_CURSO_2016.csv"))) %>%
  select(CO_CURSO, QT_INGRESSO_CURSO, QT_VAGAS_TOTAIS)

IES_COMPLEMENTAR <- data.frame(fread(file_download("data/CES/DM_IES_2016.csv"))) %>%
  select(c(CO_IES, CO_MANTENEDORA))

all_files <- list_folder("data/INEP/2_extract")
arq_cursos <- subset(all_files, grepl("CURSOS", all_files))
cod02_CURSOS <- foreach (i = 1:NROW(arq_cursos), .combine = "rbind") %do% {
  return(data.frame(fread(file_download(paste0("data/INEP/2_extract/", arq_cursos[i])))))
}
cod02_CURSOS <- cod02_CURSOS %>%
  left_join(CES_COMPLEMENTAR, by = "CO_CURSO") %>%
  left_join(IES_COMPLEMENTAR, by = "CO_IES") %>%
  mutate(AREA = substr(CO_CINE_ROTULO, 1, 4),
         CO_CURSO_N = row_number())

arq_delta <- subset(all_files, grepl("LOGIT_DELTA",all_files))
cod02_DELTA <- foreach (i = 1:NROW(arq_delta), .combine = "rbind") %do% {
  return(data.frame(fread(file_download(paste0("data/INEP/2_extract/", arq_delta[i])))))
}

arq_grupos <- subset(all_files, grepl("GRUPOS_REGIONAIS", all_files))
GRUPOS <- foreach (i = 1:NROW(arq_grupos), .combine = "rbind") %do% {
  return(data.frame(fread(file_download(paste0("data/INEP/2_extract/", arq_grupos[i])))))
}
GRUPOS <- GRUPOS %>%
  mutate(UF = as.double(ifelse(UF == "X", NA, UF)),
         RG = as.double(ifelse(RG == "X", NA, RG)),
         BR = as.double(BR))

GRUPOS_AGREGADO <- fread(file_download("data/INEP/2_extract/LOGIT_GRUPOS_AGREGADO.csv"))

"------------------------------------------------------------"
#CLEANING TEMP FOLDER
"------------------------------------------------------------"
file.remove(paste0(tempdir(), "\\", list.files(tempdir())[grepl("file", list.files(tempdir()))]))

"------------------------------------------------------------"
#DATA ANALYSIS
"------------------------------------------------------------"
myCluster <- makeCluster(2, type = "PSOCK")
registerDoParallel(myCluster)

anos <- 2016
cotas <- unique(GRUPOS$COTA)
rendas <- 1:5
enem <- 1:100
UFs <- unique(GRUPOS$CO_UF_RESIDENCIA)

#REARRANGING DATA FROM ORIGINAL FILES: FILLING DATA FOR STATES
new <- foreach (i1 = 1:NROW(anos)) %do% {
  new_MERCADO <- foreach (jj = 1:(NROW(cotas) * NROW(rendas) * NROW(enem))) %dopar% {
    library(foreach)
    library(dplyr)
    i4 <- 1 + floor((jj - 1) / (NROW(cotas) * NROW(rendas)))
    i3 <- 1 + floor((jj - (i4 - 1) * (NROW(cotas) * NROW(rendas)) - 1) / (NROW(cotas)))
    i2 <- jj - (i3 - 1) * (NROW(cotas)) - (i4 - 1) * (NROW(cotas) * NROW(rendas))
    total_x <- subset(GRUPOS, NU_ANO == anos[i1] & COTA == cotas[i2] & RDPC == rendas[i3] & ENEM == enem[i4] & is.na(UF) == FALSE)
    conta_x <- subset(GRUPOS, NU_ANO == anos[i1] & COTA == cotas[i2] & RDPC == rendas[i3] & ENEM == enem[i4] & is.na(UF))
    if (NROW(conta_x) > 0){
      conta_x$UF <- (mean(total_x$BR) - sum(total_x$UF)) / NROW(conta_x)
    }
    return(rbind(total_x, conta_x))
  }
  MERCADO <- bind_rows(new_MERCADO)
  
  total_x <- subset(GRUPOS, NU_ANO == anos[i1] & COTA == "G0" & is.na(RDPC) & is.na(ENEM) & is.na(UF) == FALSE)
  conta_x <- subset(GRUPOS, NU_ANO == anos[i1] & COTA == "G0" & is.na(RDPC) & is.na(ENEM) & is.na(UF))
  if (NROW(conta_x) > 0){
    conta_x$UF <- (mean(total_x$BR) - sum(total_x$UF)) / NROW(conta_x)
  }
  new_MERCADO <- bind_rows(total_x, conta_x)
  
  cat(paste0("Anos: ", i1, " de ", NROW(anos)), sep = "\n", file = log, append = TRUE)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
  cat("PREPARA MERCADO", sep = "\n", file = log, append = TRUE)
  cat("-----------------", sep = "\n", file = log, append = TRUE)
  upload_log(log, "cod02.txt")
  
  return(rbind(MERCADO, new_MERCADO))
}
MERCADO <- bind_rows(new)

#REARRANGING DATA FROM ORIGINAL FILES: FILLING DATA FOR COMBINED STATISTICAL AREAS
new <- foreach (i1 = 1:NROW(anos)) %do% {
  new_UF <- foreach(i5 = 1:NROW(UFs)) %do% {
    new_MERCADO <- foreach (jj = 1:(NROW(cotas) * NROW(rendas) * NROW(enem))) %dopar% {
      library(foreach)
      library(dplyr)
      i4 <- 1 + floor((jj - 1) / (NROW(cotas) * NROW(rendas)))
      i3 <- 1 + floor((jj - (i4 - 1) * (NROW(cotas) * NROW(rendas)) - 1) / (NROW(cotas)))
      i2 <- jj - (i3 - 1) * (NROW(cotas)) - (i4 - 1) * (NROW(cotas) * NROW(rendas))
      
      total_x <- subset(MERCADO, NU_ANO == anos[i1] & COTA == cotas[i2] & RDPC == rendas[i3] & ENEM == enem[i4] & CO_UF_RESIDENCIA == UFs[i5] & is.na(RG) == FALSE)
      conta_x <- subset(MERCADO, NU_ANO == anos[i1] & COTA == cotas[i2] & RDPC == rendas[i3] & ENEM == enem[i4] & CO_UF_RESIDENCIA == UFs[i5] & is.na(RG))
      if (NROW(conta_x) > 0 & NROW(total_x) > 0){
        conta_x$RG <- (mean(total_x$UF) - sum(total_x$RG)) / NROW(conta_x)
      } else {
        if (NROW(conta_x) > 0){
          conta_x$RG <- conta_x$UF / NROW(conta_x)
        }
      }
      return(rbind(total_x, conta_x))
    }

    cat(paste0(i1, " de ", NROW(anos)), sep = "\n", file = log, append = TRUE)
    cat(paste0(i5, " de ", NROW(UFs)), sep = "\n", file = log, append = TRUE)
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
    cat("MERCADO FINAL", sep = "\n", file = log, append = TRUE)
    cat("-----------------", sep = "\n", file = log, append = TRUE)
    upload_log(log, "cod02.txt")
    
    MERCADO_FINAL <- bind_rows(new_MERCADO)
    total_x <- subset(MERCADO, NU_ANO == anos[i1] & COTA == "G0" & is.na(RDPC) & is.na(ENEM) & CO_UF_RESIDENCIA == UFs[i5] & is.na(RG) == FALSE)
    conta_x <- subset(MERCADO, NU_ANO == anos[i1] & COTA == "G0" & is.na(RDPC) & is.na(ENEM) & CO_UF_RESIDENCIA == UFs[i5] & is.na(RG))
    if (NROW(conta_x) > 0 & NROW(total_x) > 0){
      conta_x$RG <- (mean(total_x$UF) - sum(total_x$RG)) / NROW(conta_x)
    } else {
      if (NROW(conta_x) > 0){
        conta_x$RG <- conta_x$UF / NROW(conta_x)
      }
    }
    return(rbind(MERCADO_FINAL, total_x, conta_x))
  }
  return(bind_rows(new_UF))
}
stopCluster(myCluster)

MERCADO_FINAL <- bind_rows(new) %>%
  select(-c(UF, BR))

#RESTRICTING YEAR TO 2016 AND AGGREGATING DEGREES
cod02_DELTA <- cod02_DELTA %>%
  subset(NU_ANO == 2016) %>%
  mutate(share = exp(DELTA_RG)) %>%
  select(-c(DELTA_UF, DELTA_BR, DELTA_RG)) %>%
  left_join(MERCADO_FINAL, by = c("NU_ANO", "COTA", "RDPC", "ENEM", "CO_RG_INTERMEDIARIA_RESIDENCIA", "CO_UF_RESIDENCIA")) %>%
  mutate(N = round(share * RG, 0)) %>%
  left_join(select(cod02_CURSOS, c(NU_ANO, CO_CURSO, CO_CURSO_N)), by = c("NU_ANO", "CO_CURSO")) %>%
  mutate(CO_CURSO_N = ifelse(CO_CURSO == 0, 0, CO_CURSO_N)) %>%
  group_by(NU_ANO, COTA, RDPC, ENEM, CO_UF_RESIDENCIA, CO_RG_INTERMEDIARIA_RESIDENCIA, CO_CURSO_N) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  group_by(COTA, RDPC, ENEM, CO_UF_RESIDENCIA, CO_RG_INTERMEDIARIA_RESIDENCIA, CO_CURSO_N) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  rename("RG_INT_ALUNO" = "CO_RG_INTERMEDIARIA_RESIDENCIA",
         "RG_UF_ALUNO" = "CO_UF_RESIDENCIA")

cod02_CURSOS <- cod02_CURSOS %>%
  subset(NU_ANO == 2016 & CO_CURSO_N %in% cod02_DELTA$CO_CURSO_N) %>%
  group_by(CO_CURSO_N, 
           CO_IES, 
           CO_MANTENEDORA,
           AREA, 
           CO_MUNICIPIO, 
           TP_CATEGORIA_ADMINISTRATIVA, 
           TP_ORGANIZACAO_ACADEMICA,
           CO_UF,
           CO_RG_INTERMEDIARIA,
           CO_RG_IMEDIATA) %>%
  summarise(PRECO = mean(PRECO, na.rm = TRUE),
            NU_PROF = mean(NU_PROF, na.rm = TRUE),
            DOUTORADO = mean(DOUTORADO, na.rm = TRUE),
            MESTRADO = mean(MESTRADO, na.rm = TRUE),
            ESPECIALIZACAO = mean(ESPECIALIZACAO, na.rm = TRUE),
            GRADUACAO = mean(GRADUACAO, na.rm = TRUE),
            TEMPO_INTEGRAL_EXCLUSIVO = mean(TEMPO_INTEGRAL_EXCLUSIVO, na.rm = TRUE),
            TEMPO_INTEGRAL_NAOEXCLUSIVO = mean(TEMPO_INTEGRAL_NAOEXCLUSIVO, na.rm = TRUE),
            TEMPO_PARCIAL = mean(TEMPO_PARCIAL, na.rm = TRUE),
            NU_CARGA_HORARIA = mean(NU_CARGA_HORARIA, na.rm = TRUE),
            NU_INTEGRALIZACAO_INTEGRAL = mean(NU_INTEGRALIZACAO_INTEGRAL, na.rm = TRUE),
            NU_INTEGRALIZACAO_MATUTINO = mean(NU_INTEGRALIZACAO_MATUTINO, na.rm = TRUE),
            NU_INTEGRALIZACAO_VESPERTINO = mean(NU_INTEGRALIZACAO_VESPERTINO, na.rm = TRUE),
            NU_INTEGRALIZACAO_NOTURNO = mean(NU_INTEGRALIZACAO_NOTURNO, na.rm = TRUE),
            NU_INTEGRALIZACAO_EAD = mean(NU_INTEGRALIZACAO_EAD, na.rm = TRUE),
            IN_OFERECE_DISC_SEMI_PRES = mean(IN_OFERECE_DISC_SEMI_PRES, na.rm = TRUE),
            MATURIDADE = mean(MATURIDADE, na.rm = TRUE),
            NU_TOTAL = mean(NU_TOTAL, na.rm = TRUE),
            NU_IDADE = mean(NU_IDADE, na.rm = TRUE),
            INGRESSO = sum(QT_INGRESSO_CURSO, na.rm = TRUE),
            CAPACITY = sum(QT_VAGAS_TOTAIS, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(cod02_DELTA %>% group_by(CO_CURSO_N) %>% summarise(ALUNOS = sum(N)) %>% ungroup(), by = "CO_CURSO_N") %>%
  mutate(PUBLICA = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP1" | TP_CATEGORIA_ADMINISTRATIVA == "TP2" | TP_CATEGORIA_ADMINISTRATIVA == "TP3", 1, 0),
         PRIVADA = 1 - PUBLICA,
         TP_PUBLICA = ifelse(PUBLICA == 1, "PUBLICA", "PRIVADA"),
         PRECO_1000 = PRECO / 1000) %>%
  rename("RG_MUN_CURSO" = "CO_MUNICIPIO",
         "RG_IME_CURSO" = "CO_RG_IMEDIATA",
         "RG_INT_CURSO" = "CO_RG_INTERMEDIARIA",
         "RG_UF_CURSO" = "CO_UF")

#CAPACITY AND MINIMUM SCORES
CURSOS_NOTA <- cod02_DELTA %>%
  subset(CO_CURSO_N != "0") %>%
  group_by(CO_CURSO_N) %>%
  summarise(PROP_G0_NOTA = sum(N[(COTA == "G0") & is.na(ENEM) == FALSE]),
            PROP_G1_NOTA = sum(N[(COTA == "G0" | COTA == "G1") & is.na(ENEM) == FALSE]),
            PROP_G2_NOTA = sum(N[(COTA == "G0" | COTA == "G1" | COTA == "G2") & is.na(ENEM) == FALSE]),
            ENEM_G0 = ifelse(PROP_G0_NOTA > 0, min(ENEM[(COTA == "G0") & is.na(ENEM) == FALSE]), 1),
            ENEM_G1 = ifelse(PROP_G1_NOTA > 0, min(ENEM[(COTA == "G0" | COTA == "G1") & is.na(ENEM) == FALSE]), 1),
            ENEM_G2 = ifelse(PROP_G2_NOTA > 0, min(ENEM[(COTA == "G0" | COTA == "G1" | COTA == "G2") & is.na(ENEM) == FALSE]), 1)) %>%
  ungroup() %>%
  select(-c(PROP_G0_NOTA, PROP_G1_NOTA, PROP_G2_NOTA))

cod02_CURSOS <- cod02_CURSOS %>%
  left_join(CURSOS_NOTA, by = "CO_CURSO_N") %>%
  mutate(ENEM_MIN_G0 = NA,
         ENEM_MIN_G1 = NA,
         ENEM_MIN_G2 = NA) %>%
  mutate(ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP1", ENEM_G0, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP1", ENEM_G1, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP1", ENEM_G2, ENEM_MIN_G2),
         ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP2", ENEM_G2, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP2", ENEM_G2, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP2", ENEM_G2, ENEM_MIN_G2),
         ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP3", ENEM_G2, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP3", ENEM_G2, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP3", ENEM_G2, ENEM_MIN_G2),
         ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP4", 1, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP4", 1, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP4", 1, ENEM_MIN_G2),
         ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP5", 1, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP5", 1, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP5", 1, ENEM_MIN_G2),
         ENEM_MIN_G0 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP7", 1, ENEM_MIN_G0),
         ENEM_MIN_G1 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP7", 1, ENEM_MIN_G1),
         ENEM_MIN_G2 = ifelse(TP_CATEGORIA_ADMINISTRATIVA == "TP7", 1, ENEM_MIN_G2))

#GEO DATA
cod02_MUNICIPIOS <- cod02_MUNICIPIOS %>%
  select(c("UF", 
           "Nome_UF",
           "Código Município Completo",
           "Nome_Município",
           "Região Geográfica Imediata", 
           "Nome Região Geográfica Imediata",
           "Região Geográfica Intermediária",
           "Nome Região Geográfica Intermediária")) %>%
  rename(c("CO_UF" = "UF",
           "CO_MUNICIPIO" = "Código Município Completo",
           "CO_RG_IMEDIATA" = "Região Geográfica Imediata",
           "CO_RG_INTERMEDIARIA" = "Região Geográfica Intermediária",
           "NO_MUNICIPIO" = "Nome_Município",
           "NO_RG_IMEDIATA" = "Nome Região Geográfica Imediata",
           "NO_RG_INTERMEDIARIA" = "Nome Região Geográfica Intermediária",
           "NO_UF" = "Nome_UF")) %>%
  mutate(CO_RG_INTERMEDIARIA = as.integer(CO_RG_INTERMEDIARIA))

#DISTRIBUTION OF CONSUMERS
cod02_MARKET_SIZE <- data.frame(ENEM = rep(1:100),
                                RDPC = rep(1:5, each = 100),
                                COTA = rep(c("G0", "G1", "G2"), each = 500),
                                RG_INT_ALUNO = rep(unique(cod02_DELTA$RG_INT_ALUNO), each = 1500)) %>%
  mutate(SEGMENT = row_number()) %>%
  left_join(cod02_DELTA %>% group_by(ENEM, RDPC, COTA, RG_INT_ALUNO) %>% summarise(SIZE = sum(N)) %>% ungroup(), by = c("ENEM", "RDPC", "COTA", "RG_INT_ALUNO")) %>%
  left_join(unique(select(cod02_MUNICIPIOS, c("CO_RG_INTERMEDIARIA", "CO_UF"))) %>% rename(c("RG_INT_ALUNO" = "CO_RG_INTERMEDIARIA")), by = "RG_INT_ALUNO") %>%
  mutate(SIZE = ifelse(is.na(SIZE), 0, SIZE)) %>%
  rename("RG_UF_ALUNO" = "CO_UF")

#INCLUING GEO INFO ON LABOR MARKET DATA
cod02_RAIS <- cod01_RAIS %>%
  mutate(id_municipio = as.character(id_municipio),
         cbo_2002 = substr(cbo_2002, 1, 4)) %>%
  rename(c("CO_MUNICIPIO" = "id_municipio")) %>%
  left_join(cod02_MUNICIPIOS, by = "CO_MUNICIPIO")

#CALCULATING DISTANCES
myCluster <- makeCluster(2, type = "PSOCK")
registerDoParallel(myCluster)
cod02_DISTANCIAS <- foreach (i = 1:NROW(cod01_MAPAS),
                             .combine = "rbind") %:%
  foreach (j = 1:NROW(cod01_MAPAS),
           .combine = "rbind",
           .packages = c("dplyr", "sf", "foreach")) %dopar% {
             new <- data.frame(RG_INT_ALUNO = cod01_MAPAS$code_intermediate[i],
                               RG_INT_CURSO = cod01_MAPAS$code_intermediate[j],
                               distancia = as.double(st_distance(cod01_MAPAS$geom[i], cod01_MAPAS$geom[j]) / 1000000))
             return(new)
           }
stopCluster(myCluster)

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
unlink(log)
file_upload_RData(object = cod02_MARKET_SIZE, name = "cod02_MARKET_SIZE.RData")
file_upload_RData(object = cod02_CURSOS, name = "cod02_CURSOS.RData")
file_upload_RData(object = cod02_DELTA, name = "cod02_DELTA.RData")
file_upload_RData(object = cod02_DISTANCIAS, name = "cod02_DISTANCIAS.RData")
file_upload_RData(object = cod02_MUNICIPIOS, name = "cod02_MUNICIPIOS.RData")
file_upload_RData(object = cod02_RAIS, name = "cod02_RAIS.RData")