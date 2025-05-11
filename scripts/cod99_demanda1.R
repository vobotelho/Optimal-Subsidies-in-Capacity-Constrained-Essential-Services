"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")

"------------------------------------------------------------"
#PARAMETERS
"------------------------------------------------------------"
smm_fe_moments <- c("PUBLICA")
smm_moments <- 11
market_restrict <- c(11, 12, 13, 14, 15, 16, 17,
                     21, 22, 23, 24, 25, 26, 27, 28, 29,
                     31, 32, 33, 35,
                     41, 42, 43,
                     50, 51, 52, 53)

"------------------------------------------------------------"
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod03_CURSOS.RData")))
load(file_download(working_file("cod03_MARKET.RData")))
load(file_download(working_file("cod03_DELTA.RData")))
load(file_download(working_file("cod03_INSTRUMENTS.RData")))
load(file_download(working_file("cod03_parameters.RData")))
load(file_download(working_file("cod02_DISTANCIAS.RData")))

source("scripts\\functions\\find_shares_teste.R")
source("scripts\\functions\\find_shares_convergence_teste.R")
source("scripts\\functions\\find_delta_teste.R")
source("scripts\\functions\\gmm_criterion_teste.R")
source("scripts\\functions\\demand_starter.R")
source("scripts\\functions\\demean.R")

"------------------------------------------------------------"
#DATA ANALYSIS
"------------------------------------------------------------"
#BLP PREPARATION
exogenous <- cod03_parameters$exogenous
instruments <- cod03_parameters$instruments
fixed_effects <- cod03_parameters$fixed_effects

functions <- list("find_delta" = find_delta,
                  "find_shares" = find_shares,
                  "find_shares_convergence" = find_shares_convergence,
                  "demean" = demean)

DELTA <- cod03_DELTA %>%
  subset(RG_UF_ALUNO %in% market_restrict)

CURSOS <- select(cod03_CURSOS, c("CO_CURSO_N", 
                                 "ENEM_MEDIA_AREA",
                                 "ENEM_SD_AREA",
                                 "ENEM_MIN_G0", 
                                 "ENEM_MIN_G1",
                                 "ENEM_MIN_G2",
                                 "PUBLICA",
                                 "PRIVADA",
                                 "PRECO_1000", 
                                 "RG_INT_CURSO",
                                 "RG_UF_CURSO",
                                 "ALUNOS",
                                 "ENEM_DTM",
                                 "RDPC_DT1",
                                 "RDPC_DT2",
                                 "RDPC_DT3",
                                 "RDPC_DT4",
                                 "RDPC_DT5",
                                 "ENEM_DT1",
                                 "ENEM_DT2",
                                 "ENEM_DT3",
                                 "ENEM_DT4",
                                 "ENEM_DT5",
                                 "DIST_DT0",
                                 "DIST_DTM",
                                 "COTA_DT0",
                                 "COTA_DT1",
                                 "COTA_DT2",
                                 all_of(smm_fe_moments),
                                 all_of(fixed_effects), 
                                 all_of(exogenous),
                                 all_of(instruments))) %>%
  subset(RG_UF_CURSO %in% market_restrict)
                 
MARKET_SIZE <- cod03_MARKET_SIZE %>%
  subset(RG_UF_ALUNO %in% market_restrict)
                 
MERCADOS <- as.numeric(unique(MARKET_SIZE$RG_UF_ALUNO))

TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS),
                         .combine = "rbind") %do% {
  new <- tempfile(fileext = ".RData")
  RESULT <- "Start"
  save(RESULT, file = new)
  
  return(new)
}

#BLP
data_starter <- list(MERCADOS = MERCADOS,
                     MARKET_SIZE = MARKET_SIZE,
                     CURSOS = CURSOS,
                     DISTANCIAS = cod02_DISTANCIAS,
                     instruments = instruments,
                     smm_fe_moments = smm_fe_moments)

demand_starter(data_starter)

data <- list(MERCADOS = MERCADOS,
             DELTA = DELTA,
             CURSOS = CURSOS,
             DISTANCIAS = cod02_DISTANCIAS,
             MARKET_SIZE = MARKET_SIZE,
             TEMP_MERCADOS = TEMP_MERCADOS,
             LOG = log,
             exogenous = exogenous,
             fixed_effects = fixed_effects,
             functions = functions,
             options = "J")

system.time(gmm_criterion(coef = rep(0, times = 6), data = data))
valor <- gmm_criterion(coef = rep(0, times = 6), data = data)
print(valor)

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS), .combine = "rbind") %do% {unlink(TEMP_MERCADOS[i])}
