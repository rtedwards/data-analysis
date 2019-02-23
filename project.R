library(ggplot2)
library(dplyr)
library(skimr)
library(kableExtra)
library(gridExtra)
library(plotly)
library(tidyr)


data <- read.table("student-mat.csv", sep=";", header=TRUE)
View(data)

student <- data %>%
  select(Pstatus, guardian, absences, G1, G2, G3) %>%
  glimpse()

student %>%
  summary()
