source("dhondt_Chile.R")
library("electoral")

data <- read.csv("vote_candidate_2017.csv", fileEncoding = "UTF-8-BOM")

dhondt_Chile(data, 1, 3)