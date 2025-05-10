## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)



## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  # From CRAN
#  install.packages("geobr")
#  
#  # Development version
#  utils::remove.packages('geobr')
#  devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
#  

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
library(geobr)
library(sf)
library(dplyr)
library(ggplot2)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Available data sets
datasets <- list_geobr()

head(datasets)


## ----message=FALSE, warning=FALSE---------------------------------------------
# State of Sergige
state <- read_state(
  code_state="SE",
  year=2018,
  showProgress = FALSE
  )

# Municipality of Sao Paulo
muni <- read_municipality(
  code_muni = 3550308, 
  year=2010, 
  showProgress = FALSE
  )

ggplot() + 
  geom_sf(data = muni, color=NA, fill = '#1ba185') +
  theme_void()

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
# All municipalities in the state of Minas Gerais
muni <- read_municipality(code_muni = "MG", 
                          year = 2007,
                          showProgress = FALSE)

# All census tracts in the state of Rio de Janeiro
cntr <- read_census_tract(
  code_tract = "RJ", 
  year = 2010,
  showProgress = FALSE
  )

head(muni)

## ----message=FALSE, warning=FALSE---------------------------------------------
# read all intermediate regions
inter <- read_intermediate_region(
  year = 2017,
  showProgress = FALSE
  )

# read all states
states <- read_state(
  year = 2019, 
  showProgress = FALSE
  )

head(states)

## ----message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis


## ----message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----

# Download all municipalities of Rio
all_muni <- read_municipality(
  code_muni = "RJ", 
  year= 2010,
  showProgress = FALSE
  )

# plot
ggplot() +
  geom_sf(data=all_muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities of Rio de Janeiro, 2000", size=8) +
  theme_minimal() +
  no_axis


## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
# Read data.frame with life expectancy data
df <- utils::read.csv(system.file("extdata/br_states_lifexpect2017.csv", package = "geobr"), encoding = "UTF-8")

states$name_state <- tolower(states$name_state)
df$uf <- tolower(df$uf)

# join the databases
states <- dplyr::left_join(states, df, by = c("name_state" = "uf"))


## ----message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----
ggplot() +
  geom_sf(data=states, aes(fill=ESPVIDA2017), color= NA, size=.15) +
    labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
    scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
    theme_minimal() +
    no_axis


## -----------------------------------------------------------------------------
library(censobr)
library(arrow)

hs <- read_households(year = 2010, 
                      showProgress = FALSE)


## ----warning = FALSE----------------------------------------------------------
esg <- hs |> 
        collect() |>
        group_by(code_muni) |>                                             # (a)
        summarize(rede = sum(V0010[which(V0207=='1')]),                    # (b)
                  total = sum(V0010)) |>                                   # (b)
        mutate(cobertura = rede / total) |>                                # (c)
        collect()                                                          # (d)

head(esg)

## ----warning = FALSE----------------------------------------------------------
# download municipality geometries
muni_sf <- geobr::read_municipality(year = 2010,
                                    showProgress = FALSE)

# merge data
esg_sf <- left_join(muni_sf, esg, by = 'code_muni')

# plot map
ggplot() +
  geom_sf(data = esg_sf, aes(fill = cobertura), color=NA) +
  labs(title = "Share of households connected to a sewage network") +
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name='Share of\nhouseholds', 
                       labels = scales::percent) +
  theme_void()


