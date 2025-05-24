"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")
log <- tempfile(fileext = ".txt")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
cat("-----------------", sep = "\n", file = log, append = TRUE)
cat("", sep = "\n", file = log, append = TRUE)
upload_log(log, "cod04.txt")
unlink(log)

"------------------------------------------------------------"
#PARAMETERS
"------------------------------------------------------------"
smm_fe_moments <- c("PUBLICA", "AREA")
smm_moments <- 11

"------------------------------------------------------------"
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod03_CURSOS.RData")))
load(file_download(working_file("cod03_MARKET.RData")))
load(file_download(working_file("cod03_DELTA.RData")))
load(file_download(working_file("cod03_INSTRUMENTS.RData")))
load(file_download(working_file("cod03_parameters.RData")))
load(file_download(working_file("cod02_DISTANCIAS.RData")))

source("scripts\\functions\\find_delta.R")
source("scripts\\functions\\gmm_criterion.R")
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
                  "demean" = demean)

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
                                 all_of(instruments)))
                 
MERCADOS <- as.numeric(unique(cod03_MARKET_SIZE$RG_UF_ALUNO))

TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS),
                         .combine = "rbind") %do% {
  new <- tempfile(fileext = ".RData")
  RESULT <- "Start"
  save(RESULT, file = new)
  
  return(new)
}

#BLP
data_starter <- list(MERCADOS = MERCADOS,
                     MARKET_SIZE = cod03_MARKET_SIZE,
                     CURSOS = CURSOS,
                     DELTA = cod03_DELTA,
                     DISTANCIAS = cod02_DISTANCIAS,
                     instruments = instruments,
                     smm_fe_moments = smm_fe_moments)

demand_starter(data_starter)

data <- list(MERCADOS = MERCADOS,
             DELTA = cod03_DELTA,
             CURSOS = CURSOS,
             DISTANCIAS = cod02_DISTANCIAS,
             MARKET_SIZE = cod03_MARKET_SIZE,
             TEMP_MERCADOS = TEMP_MERCADOS,
             LOG = log,
             exogenous = exogenous,
             fixed_effects = fixed_effects,
             functions = functions,
             options = "J")

gmm_stage1 <- optim(par = rep(0, times = 7),
                    fn = gmm_criterion,
                    data = data,
                    method = "L-BFGS-B")

data[["options"]] <- "Full"
theta_stage1 <- gmm_stage1$par
simul_stage1 <- gmm_criterion(coef = theta_stage1, data = data)
W <- simul_stage1$W_update
save(W, file = "local\\cod04_W.RData")
save(W, file = "local\\cod04_W1.RData")

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS), .combine = "rbind") %do% {unlink(TEMP_MERCADOS[i])}

file_upload_RData(object = theta_stage1, name = "cod04_theta1.RData")
file_upload_RData(object = simul_stage1, name = "cod04_stage1.RData")
file_upload_RData(object = data, name = "cod04_data1.RData")
file_upload_RData(object = W, name = "cod04_W.RData")