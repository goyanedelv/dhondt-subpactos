source('dhondt_Chile.R')
library('coalitions')

data <- read.csv('example_data.csv', fileEncoding = "UTF-8-BOM")

dhondt_Chile(data, 1, 2)
