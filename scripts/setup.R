"PROJECT DEVELOPED FOR R 4.1.3 64bit version"
library(renv)
renv::init()

install.packages("gitcreds")
install.packages("usethis")
library(usethis)
library(gitcreds)
gitcreds_set()
use_git()
use_github()

renv::restore()
renv::activate()