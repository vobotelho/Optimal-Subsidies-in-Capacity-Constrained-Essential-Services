"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")

"------------------------------------------------------------"
#PARAMETERS
"------------------------------------------------------------"
instruments <- c("CBO_2341", "CBO_2344", "CBO_2345", "CBO_2346", "CBO_2347", "CBO_2348")
exogenous <- c(0)
fixed_effects <- c("PUBLICA", "AREA")

"------------------------------------------------------------"
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod02_CURSOS.RData")))
load(file_download(working_file("cod02_DELTA.RData")))
load(file_download(working_file("cod02_DISTANCIAS.RData")))
load(file_download(working_file("cod02_RAIS.RData")))

"------------------------------------------------------------"
#DATA ANALYSIS
"------------------------------------------------------------"
COST_SHIFTER <- cod02_RAIS %>% 
  #RESTRICTING THE SAMPLE TO COLLEGE PROFESSORS
  subset(substr(cbo_2002, 1, 3) == "234") %>%
  mutate(cbo_2002 = paste0("CBO_", cbo_2002)) %>%
  group_by(CO_RG_INTERMEDIARIA, cbo_2002) %>% 
  summarise(wages = median(12 * valor_remuneracao_media / (52 * quantidade_horas_contratadas)) / 1000) %>%
  ungroup() %>%
  #AVERAGE WAGES BY DEPARTMENT
  as.data.table() %>%
  dcast(CO_RG_INTERMEDIARIA ~ cbo_2002, value.var = "wages") %>%
  as.data.frame() %>%
  left_join(cod02_RAIS %>% group_by(CO_RG_INTERMEDIARIA) %>% summarise(CBO_ALL = median(valor_remuneracao_media / quantidade_horas_contratadas) / 1000) %>% ungroup(), by = "CO_RG_INTERMEDIARIA") %>%  #AVERAGE WAGES FOR ALL COLLEGE EMPLOYEES
  left_join(cod02_RAIS %>% subset(substr(cbo_2002, 1, 3) == "234") %>% group_by(CO_RG_INTERMEDIARIA) %>% summarise(CBO_PROF = median(valor_remuneracao_media / quantidade_horas_contratadas) / 1000) %>% ungroup(), by = "CO_RG_INTERMEDIARIA") %>%   #AVERAGE WAGES FOR ALL PROFESSORS
  rename("RG_INT_CURSO" = "CO_RG_INTERMEDIARIA") %>%
  select(c("RG_INT_CURSO", all_of(instruments)))

cod03_CURSOS <- cod02_CURSOS %>%
  left_join(COST_SHIFTER, by = c("RG_INT_CURSO"))

cod03_Z <- select(cod03_CURSOS, which(colnames(cod03_CURSOS) == "CO_CURSO_N" | colnames(cod03_CURSOS) %in% instruments))

#SAMPLE RESTRICTIONS: STUDENTS
cod03_DELTA <- subset(cod02_DELTA, is.na(ENEM) == FALSE & is.na(RDPC) == FALSE) %>%                   #ONLY CHOICES WITH OBSERVED CHARACTERISTICS
  left_join(select(cod03_CURSOS, c(CO_CURSO_N, RG_INT_CURSO, RG_UF_CURSO)), by = "CO_CURSO_N") %>%
  subset((CO_CURSO_N == "0" | RG_UF_CURSO == RG_UF_ALUNO)) %>%                                        #ONLY SAME COMBINED STATISTICAL AREA
  select(-c(RG_UF_CURSO))

#UPDATE DEGREE CHARACTERISTICS: NUMBER OF STUDENTS AND MOMENTS
cod03_MOMENTOS <- cod03_DELTA %>% 
  subset(CO_CURSO_N != "0") %>%
  left_join(cod02_DISTANCIAS, by = c("RG_INT_ALUNO", "RG_INT_CURSO")) %>%
  group_by(CO_CURSO_N) %>% 
  summarise(ENEM_DTM = weighted.mean(x = ENEM, w = N) / 100,
            ENEM_DT1 = sum(N[ENEM > 0 & ENEM <= 20], na.rm = TRUE) / sum(N),
            ENEM_DT2 = sum(N[ENEM > 20 & ENEM <= 40], na.rm = TRUE) / sum(N),
            ENEM_DT3 = sum(N[ENEM > 40 & ENEM <= 60], na.rm = TRUE) / sum(N),
            ENEM_DT4 = sum(N[ENEM > 60 & ENEM <= 80], na.rm = TRUE) / sum(N),
            ENEM_DT5 = sum(N[ENEM > 80 & ENEM <= 100], na.rm = TRUE) / sum(N),
            RDPC_DT1 = sum(N[RDPC == 1], na.rm = TRUE) / sum(N),
            RDPC_DT2 = sum(N[RDPC == 2], na.rm = TRUE) / sum(N),
            RDPC_DT3 = sum(N[RDPC == 3], na.rm = TRUE) / sum(N),
            RDPC_DT4 = sum(N[RDPC == 4], na.rm = TRUE) / sum(N),
            RDPC_DT5 = sum(N[RDPC == 5], na.rm = TRUE) / sum(N),
            COTA_DT0 = sum(N[COTA == "G0"], na.rm = TRUE) / sum(N),
            COTA_DT1 = sum(N[COTA == "G1"], na.rm = TRUE) / sum(N),
            COTA_DT2 = sum(N[COTA == "G2"], na.rm = TRUE) / sum(N),
            DIST_DT0 = sum(N[distancia == 0], na.rm = TRUE) / sum(N),
            DIST_DTM = weighted.mean(x = distancia, w = N),
            ALUNOS = sum(N)) %>%
  ungroup()

#AGGREGATE MAJORS WITH LESS THAN 0.5% OF DEGREES (0.5% OF 21,232)
cod03_AGREGA_AREA <- cod03_DELTA %>%
  subset(CO_CURSO_N != "0") %>%
  left_join(select(cod03_CURSOS, c(CO_CURSO_N, AREA, RG_UF_CURSO)), by = "CO_CURSO_N") %>%
  group_by(AREA) %>%
  summarise(CURSOS = NROW(unique(CO_CURSO_N)),
            nUFs = NROW(unique(RG_UF_CURSO))) %>%
  ungroup() %>%
  mutate(NEW_AREA = ifelse(CURSOS <= 106, "9999", AREA))

cod03_CURSOS <- cod03_CURSOS %>%
  #UPDATE NUMBER OF STUDENTS
  select(-c(ALUNOS)) %>%
  left_join(cod03_MOMENTOS, by = "CO_CURSO_N") %>%
  #UPDATE MAJOR
  left_join(select(cod03_AGREGA_AREA, c(AREA, NEW_AREA)), by = "AREA") %>%
  mutate(AREA = NEW_AREA) %>%
  select(-c(NEW_AREA))

cod03_MOMENTOS_AREA <- cod03_DELTA %>%
  subset(CO_CURSO_N != "0") %>%
  left_join(select(cod03_CURSOS, c(CO_CURSO_N, AREA)), by = "CO_CURSO_N") %>%
  group_by(AREA) %>%
  summarise(ENEM_MEDIA_AREA = weighted.mean(x = ENEM, w = N),
            ENEM_SD_AREA = as.double(sqrt(weighted.var(x = ENEM, w = N))),
            N = sum(N),
            CURSOS = NROW(unique(CO_CURSO_N))) %>%
  ungroup()

cod03_CURSOS <- cod03_CURSOS %>%
  left_join(select(cod03_MOMENTOS_AREA, c(AREA, ENEM_MEDIA_AREA, ENEM_SD_AREA)), by = "AREA")

cod03_DELTA <- cod03_DELTA %>% select(-c(RG_INT_CURSO))

#SAMPLE RESTRICTIONS: DEGREES
KEEP <- cod03_CURSOS %>%
  select(c("CO_CURSO_N",
           "PRECO_1000",
           "ALUNOS",
           all_of(instruments),
           all_of(exogenous),
           all_of(fixed_effects))) %>%
  na.exclude() %>%
  subset(ALUNOS >= 5)

cod03_CURSOS <- cod03_CURSOS %>%
  subset(CO_CURSO_N %in% KEEP$CO_CURSO_N) %>%
  mutate(NU_CARGA_HORARIA = log(NU_CARGA_HORARIA),
         NU_CARGA_HORARIA = (NU_CARGA_HORARIA - mean(NU_CARGA_HORARIA)) / sd(NU_CARGA_HORARIA),
         DOUTORADO = (DOUTORADO - mean(DOUTORADO)) / sd(DOUTORADO),
         TEMPO_INTEGRAL_EXCLUSIVO = (TEMPO_INTEGRAL_EXCLUSIVO - mean(TEMPO_INTEGRAL_EXCLUSIVO)) / sd(TEMPO_INTEGRAL_EXCLUSIVO),
         NU_INTEGRALIZACAO_NOTURNO = (NU_INTEGRALIZACAO_NOTURNO - mean(NU_INTEGRALIZACAO_NOTURNO)) / sd(NU_INTEGRALIZACAO_NOTURNO),
         MATURIDADE = (MATURIDADE - mean(MATURIDADE)) / sd(MATURIDADE),
         IN_OFERECE_DISC_SEMI_PRES = (IN_OFERECE_DISC_SEMI_PRES - mean(IN_OFERECE_DISC_SEMI_PRES)) / sd(IN_OFERECE_DISC_SEMI_PRES))

cod03_DELTA <- cod03_DELTA %>%
  subset(CO_CURSO_N %in% KEEP$CO_CURSO_N | CO_CURSO_N == "0")

#NEW MARKET SIZE VECTOR (INCORPORATING CHANGES IN SAMPLE OF CONSUMERS)
cod03_MARKET_SIZE <- data.frame(ENEM = rep(1:100),
                                RDPC = rep(1:5, each = 100),
                                COTA = rep(c("G0", "G1", "G2"), each = 500),
                                RG_INT_ALUNO = rep(unique(cod03_DELTA$RG_INT_ALUNO), each = 1500)) %>%
  mutate(SEGMENT = row_number()) %>%
  left_join(cod03_DELTA %>% group_by(ENEM, RDPC, COTA, RG_INT_ALUNO, RG_UF_ALUNO) %>% summarise(SIZE = sum(N)) %>% ungroup(), by = c("ENEM", "RDPC", "COTA", "RG_INT_ALUNO")) %>%
  mutate(SIZE = ifelse(is.na(SIZE), 0, SIZE),
         RG_UF_ALUNO = ifelse(is.na(RG_UF_ALUNO), substr(RG_INT_ALUNO, 1, 2), RG_UF_ALUNO))

#TESTING INSTRUMENTS: NAIVE OLS AND 2SLS
SHARES <- cod03_DELTA %>%
  left_join(select(cod03_CURSOS, c(CO_CURSO_N, RG_UF_CURSO)), by = "CO_CURSO_N") %>%
  mutate(RG_UF_CURSO = ifelse(CO_CURSO_N == "0", RG_UF_ALUNO, RG_UF_CURSO)) %>%
  group_by(CO_CURSO_N, RG_UF_CURSO) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  group_by(RG_UF_CURSO) %>%
  mutate(SHARE = N / sum(N),
         SHARE_OUTSIDE = N[CO_CURSO_N == "0"] / sum(N)) %>%
  ungroup() %>%
  mutate(SHARE_LIQ = log(SHARE) - log(SHARE_OUTSIDE)) %>%
  subset(CO_CURSO_N != "0") %>%
  select(-c(RG_UF_CURSO, N, SHARE_OUTSIDE))

CURSOS_NAIVE <- cod03_CURSOS %>%
  left_join(SHARES, by = "CO_CURSO_N")

naive_ols_1 <- felm(SHARE_LIQ ~ PRECO_1000 | 0 | 0 | 0, data = CURSOS_NAIVE)
naive_ols_1_pri <- felm(SHARE_LIQ ~ PRECO_1000 | 0 | 0 | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))
naive_ols_2 <- felm(SHARE_LIQ ~ PRECO_1000 | PUBLICA | 0 | 0, data = CURSOS_NAIVE)
naive_ols_2_pri <- felm(SHARE_LIQ ~ PRECO_1000 | PUBLICA | 0 | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))
naive_ols_3 <- felm(SHARE_LIQ ~ PRECO_1000 | AREA + PUBLICA | 0 | 0, data = CURSOS_NAIVE)
naive_ols_3_pri <- felm(SHARE_LIQ ~ PRECO_1000 | AREA + PUBLICA | 0 | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))

naive_2sls_1 <- felm(SHARE_LIQ ~ 1 | 0 | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = CURSOS_NAIVE)
naive_2sls_1_pri <- felm(SHARE_LIQ ~ 1 | 0 | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))
naive_2sls_2 <- felm(SHARE_LIQ ~ 1 | PUBLICA | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = CURSOS_NAIVE)
naive_2sls_2_pri <- felm(SHARE_LIQ ~ 1 | PUBLICA | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))
naive_2sls_3 <- felm(SHARE_LIQ ~ 1 | AREA + PUBLICA | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = CURSOS_NAIVE)
naive_2sls_3_pri <- felm(SHARE_LIQ ~ 1 | AREA + PUBLICA | (PRECO_1000 ~ CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348) | 0, data = subset(CURSOS_NAIVE, PRIVADA == 1))

first_stage_1 <- felm(PRECO_1000 ~ 1 + CBO_2341 + CBO_2344 + CBO_2345 + CBO_2346 + CBO_2347 + CBO_2348 | AREA + PUBLICA | 0 | 0, data = subset(cod03_CURSOS, PRIVADA == 1))

cod03_naive_models <- list(
  "naive_ols_1" = naive_ols_1,
  "naive_ols_2" = naive_ols_2,
  "naive_ols_3" = naive_ols_3,
  "naive_ols_1_pri" = naive_ols_1_pri,
  "naive_ols_2_pri" = naive_ols_2_pri,
  "naive_ols_3_pri" = naive_ols_3_pri,
  "naive_2sls_1" = naive_2sls_1,
  "naive_2sls_2" = naive_2sls_2,
  "naive_2sls_3" = naive_2sls_3,
  "naive_2sls_1_pri" = naive_2sls_1_pri,
  "naive_2sls_2_pri" = naive_2sls_2_pri,
  "naive_2sls_3_pri" = naive_2sls_3_pri,
  "first_stage_1" = first_stage_1
)

cod03_parameters <- list(
  "exogenous" = exogenous,
  "instruments" = instruments,
  "fixed_effects" = fixed_effects
)

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
file_upload_RData(object = cod03_DELTA, name = "cod03_DELTA.RData")
file_upload_RData(object = cod03_CURSOS, name = "cod03_CURSOS.RData")
file_upload_RData(object = cod03_MARKET_SIZE, name = "cod03_MARKET.RData")
file_upload_RData(object = cod03_Z, name = "cod03_INSTRUMENTS.RData")
file_upload_RData(object = cod03_AGREGA_AREA, name = "cod03_AGREGA_AREA.RData")
file_upload_RData(object = cod03_MOMENTOS_AREA, name = "cod03_MOMENTOS_AREA.RData")
file_upload_RData(object = cod03_naive_models, name = "cod03_naive_models.RData")
file_upload_RData(object = cod03_parameters, name = "cod03_parameters.RData")