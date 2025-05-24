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
#LOADING DATA
"------------------------------------------------------------"
load(file_download(working_file("cod04_theta1.RData")))
load(file_download(working_file("cod04_stage1.RData")))
load(file_download(working_file("cod04_data1.RData")))

source("scripts\\functions\\find_delta.R")
source("scripts\\functions\\gmm_criterion.R")

"------------------------------------------------------------"
#DATA ANALYSIS
"------------------------------------------------------------"
TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS),
                         .combine = "rbind") %do% {
                           new <- tempfile(fileext = ".RData")
                           RESULT <- "Start"
                           save(RESULT, file = new)
                           
                           return(new)
                         }

data[["options"]] <- "J"
data[["TEMP_MERCADOS"]] <- TEMP_MERCADOS
data[["LOG"]] <- log

gmm_stage2 <- optim(par = matrix(0, nrow = NROW(theta_stage1), ncol = 1),
                    fn = gmm_criterion,
                    data = data,
                    method = "L-BFGS-B")

data[["options"]] <- "Full"
theta_stage2 <- gmm_stage2$par
simul_stage2 <- gmm_criterion(coef = theta_stage2, data = data)

"------------------------------------------------------------"
#SAVE
"------------------------------------------------------------"
TEMP_MERCADOS <- foreach(i = 1:NROW(MERCADOS), .combine = "rbind") %do% {unlink(TEMP_MERCADOS[i])}

file_upload_RData(object = theta_stage2, name = "cod04_theta2.RData")
file_upload_RData(object = simul_stage2, name = "cod04_stage2.RData")
file_upload_RData(object = data, name = "cod04_data2.RData")