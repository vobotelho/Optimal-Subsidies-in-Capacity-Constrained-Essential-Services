"------------------------------------------------------------"
#INITIALIZE
"------------------------------------------------------------"
source("scripts\\load.R")

"------------------------------------------------------------"
#DOWNLOADS
"------------------------------------------------------------"
sql <- "SELECT ano, id_municipio, id_municipio_trabalho, cbo_2002, cnae_2, valor_remuneracao_media, quantidade_horas_contratadas, natureza_juridica FROM `basedosdados.br_me_rais.microdados_vinculos` WHERE (ano = 2016) AND LEFT(cnae_2, 3) = '853'"
tb <- bq_project_query(key_get('project_id'), sql)
cod01_RAIS <- bq_table_download(tb)
file_upload_RData(object = cod01_RAIS, name = "cod01_RAIS.RData")

cod01_MAPAS <- st_centroid(st_transform(read_intermediate_region("all", year = 2019), 29101))
file_upload_RData(object = cod01_MAPAS, name = "cod01_MAPAS.RData")