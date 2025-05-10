library(gitcreds)
library(httr)
library(keyring)
library(usethis)
library(dplyr)
library(readxl)
library(jsonlite)
library(stringr)
library(bigrquery)
library(httpuv)
library(sf)
library(geobr)
library(foreach)
library(doParallel)
library(data.table)
library(spatstat)
library(lfe)
library(reshape2)
library(matrixcalc)
library(corpcor)
library(SQUAREM)
library(fastDummies)
library(microbenchmark)

rm(list=ls())
gc()
renv::settings$vcs.ignore.library(FALSE)
renv::settings$vcs.ignore.cellar(FALSE)
renv::settings$vcs.ignore.local(FALSE)

source("keys\\keys.R")
key_set_with_value(service = 'ClientID', password = keys$ClientID)
key_set_with_value(service = 'ClientSecret', password = keys$ClientSecret)
key_set_with_value(service = 'RefreshToken', password = keys$RefreshToken)
key_set_with_value(service = 'project_id', password = keys$project_id)

retry_loop <- function(task, retries = 4) {
  for (i in seq_len(retries)) {
    tryCatch({
      return(task)
    }, error = function(e) {
      if (i == retries) {stop(e)}
      Sys.sleep(30 * i)
    })
  }
}

access_generator <- function(){
  tryCatch({
    response <- retry_loop(POST(
      url = "https://api.dropboxapi.com/oauth2/token",
      body = list(
        "grant_type" = "refresh_token",
        "refresh_token" = paste0(key_get('RefreshToken')),
        "client_id" = paste0(key_get('ClientID')),
        "client_secret" = paste0(key_get('ClientSecret'))),
      encode = "form"
    ))
    
    if (response$status_code == 200){
      key_set_with_value(service = 'AccessToken', password = fromJSON(rawToChar(response$content))$access_token)
      return(response$status_code)
    } else {
      message(sprintf("Error obtaining access token: error ", response$status_code))
      return(NULL)
    }
  }, error = function(e){
    message(sprintf("Error obtaining access token"))
  })
}

file_download <- function(path_consulta){
  token <- access_generator()
  temp_file <- NULL
  if (is.null(token) == FALSE){
    if (token == 200){
      headers <- add_headers(
        "Authorization" = paste0("Bearer ", key_get('AccessToken')),
        "Dropbox-API-Arg" = sprintf('{"path": "/%s"}', path_consulta)
      )
      
      tryCatch({
        response <- retry_loop(POST(
          url = "https://content.dropboxapi.com/2/files/download",
          config = headers
        ))
        
        if (response$status_code == 200){
          temp_file <- tempfile(fileext = paste0(".", strsplit(path_consulta, ".", fixed=T)[[1]][-1]))
          writeBin(response$content, temp_file)
        } else {
          stop("Failed to download file: HTTP ", response$status_code)
        }
      }, error = function(e){
        message(sprintf("Error downloading file: %s", e$message))
      })
    }
  }
  return(temp_file)
}

file_upload <- function(object, name){
  token <- access_generator()
  if (is.null(token) == FALSE){
    if (token == 200){
      headers <- add_headers(
        "Authorization" = paste0("Bearer ", key_get('AccessToken')),
        "Dropbox-API-Arg" = toJSON(list(
          autorename = FALSE,
          mode = "overwrite",
          mute = FALSE,
          path = paste0("/working papers/Optimal Subsidies in Capacity-Constrained Essential Services/working/", name),
          strict_conflict = FALSE
        ), auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream"
      )
      
      rds_content <- rawConnection(raw(0), open = "wb")
      saveRDS(object, file = rds_content)
      file_content <- rawConnectionValue(rds_content)
      close(rds_content)
      
      tryCatch({
        response <- retry_loop(POST(
          url = "https://content.dropboxapi.com/2/files/upload",
          config = headers,
          body = file_content,
          encode = "form"
        ))
        
        if (response$status_code == 200){
        } else {
          stop(sprintf("Failed to upload file '%s': HTTP %d", name, response$status_code))
        }
      }, error = function(e){
        message(sprintf("Error uploading file '%s': %s", name, e$message))
      })
    }
  }
}

upload_log <- function(object, name){
  token <- access_generator()
  if (is.null(token) == FALSE){
    if (token == 200){
      headers <- add_headers(
        "Authorization" = paste0("Bearer ", key_get('AccessToken')),
        "Dropbox-API-Arg" = toJSON(list(
          autorename = FALSE,
          mode = "overwrite",
          mute = FALSE,
          path = paste0("/working papers/Optimal Subsidies in Capacity-Constrained Essential Services/logs/", name),
          strict_conflict = FALSE
        ), auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream"
      )
      
      text <- readLines(object)
      tryCatch({
        response <- retry_loop(POST(
          url = "https://content.dropboxapi.com/2/files/upload",
          config = headers,
          body = text,
          encode = "form"
        ))
        
        if (response$status_code == 200){
        } else {
          stop(sprintf("Failed to upload file '%s': HTTP %d", name, response$status_code))
        }
      }, error = function(e){
        message(sprintf("Error uploading file '%s': %s", name, e$message))
      })
    }
  }
}

file_upload_RData <- function(object, name){
  token <- access_generator()
  
  if (is.null(token) == FALSE){
    if (token == 200){
      object_name <- deparse(substitute(object))
      assign(object_name, object)
      
      headers <- add_headers(
        "Authorization" = paste0("Bearer ", key_get('AccessToken')),
        "Dropbox-API-Arg" = toJSON(list(
          autorename = FALSE,
          mode = "overwrite",
          mute = FALSE,
          path = paste0("/working papers/Optimal Subsidies in Capacity-Constrained Essential Services/working/", name),
          strict_conflict = FALSE
        ), auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream"
      )
      
      temp_file <- tempfile(fileext = ".RData")
      save(list = object_name, file = temp_file)
      file_content <- readBin(temp_file, "raw", file.info(temp_file)$size)
      unlink(temp_file)
      
      tryCatch({
        response <- retry_loop(POST(
          url = "https://content.dropboxapi.com/2/files/upload",
          config = headers,
          body = file_content,
          encode = "form"
        ))
        
        if (response$status_code == 200){
        } else {
          stop(sprintf("Failed to upload file '%s': HTTP %d", name, response$status_code))
        }
      }, error = function(e){
        message(sprintf("Error uploading file '%s': %s", name, e$message))
      })
    }
  }
}

list_folder <- function(path_consulta){
  token <- access_generator()
  file_list <- NULL
  
  if (is.null(token) == FALSE){
    if (token == 200){
      headers <- add_headers(
        "Authorization" = paste0("Bearer ", key_get('AccessToken')),
        "Content-Type" = "application/json"
      )
      
      data <- toJSON(list(
        include_deleted = FALSE,
        include_has_explicit_shared_members = FALSE,
        include_media_info = FALSE,
        include_mounted_folders = FALSE,
        include_non_downloadable_files = FALSE,
        path = paste0("/", path_consulta),
        recursive = FALSE
      ), auto_unbox = TRUE)
      
      response <- retry_loop(POST(
        url = "https://api.dropboxapi.com/2/files/list_folder",
        headers,
        body = data,
        encode = "json"
      ))
      
      file_list <- fromJSON(rawToChar(response$content))[["entries"]]$name
    }
  }
  return(file_list)
}

working_file <- function(filename){
  basic_wd <- paste0("working papers/Optimal Subsidies in Capacity-Constrained Essential Services/working/", filename)
  return(basic_wd)
}