rm(list = ls())

library(tidyverse)

load("data/sysdata.rda")

map(list.files("R/", full.names = T), source)
