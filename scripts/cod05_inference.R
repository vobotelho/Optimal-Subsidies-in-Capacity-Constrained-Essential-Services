"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")
log <- tempfile(fileext = ".txt")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep = "\n", file = log, append = TRUE)
cat("-----------------", sep = "\n", file = log, append = TRUE)
cat("", sep = "\n", file = log, append = TRUE)
upload_log(log, "cod05.txt")
unlink(log)

"------------------------------------------------------------"
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod03_CURSOS.RData")))
load(file_download(working_file("cod03_MARKET.RData")))
load(file_download(working_file("cod03_parameters.RData")))

load(file_download(working_file("cod04_theta2.RData")))
load(file_download(working_file("cod04_stage2.RData")))
load(file_download(working_file("cod04_data2.RData")))

source("scripts\\functions\\find_delta.R")
source("scripts\\functions\\gmm_variance.R")

"------------------------------------------------------------"
#DATA ANALYSIS
"------------------------------------------------------------"
data[["DELTAS"]] <- simul_stage2$DELTAS
inference <- gmm_variance(coef = theta_stage2, data = data)

cod05_CURSOS <- cod03_CURSOS %>%
  left_join(simul_stage2$DELTAS, by = c("CO_CURSO_N"))

cod05_MARKET_SIZE <- cod03_MARKET_SIZE %>%
  mutate(SEGMENT = paste0(RDPC, "_", ENEM, "_", RG_INT_ALUNO, "_", COTA)) %>%
  mutate(ALPHA = theta_stage2[RDPC] + theta_stage2[6] * ENEM / 100,
         BETA_DIS = theta_stage2[7])

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
file_upload_RData(object = inference, name = "cod05_inference.RData")
file_upload_RData(object = cod05_CURSOS, name = "cod05_CURSOS.RData")
file_upload_RData(object = cod05_MARKET_SIZE, name = "cod05_MARKET.RData")