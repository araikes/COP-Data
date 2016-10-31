#### COP Data Analysis ####
# This is a collaborative project between Dr. Eadric Bressel, Adam Raikes, Talin
# Louder, and Devin Patterson. In this arm of the analysis, I will analyze the
# raw X and Y COP data to examine the multiscale complexity of single and dual
# task postural sway on land and water.

# The data was provided to me as a .xlsx file with 4 tabs. Tabs 4 and 5 are the
# COPx and Y. However, these are untidy data with blank columns to separate
# conditions. Prior to computing multiscale entropy, this dataset requires
# substantial cleaning.

# This script is the second phase of analysis. MSE has alreay been computed on
# the dataset. This analysis will employ a 2x2 repeated measures design. 

# To account for the possibility of nonstationarity in the data, the analysis
# will look at both raw and detrended COP data.

#### Load libraries ####
library(tidyverse)
library(nlme)

#### Load MSE data ####
mse.x <- read.csv("Data Files/MSE X.csv", header = FALSE)
mse.y <- read.csv("Data Files/MSE Y.csv", header = FALSE)
mse.x.detrended <- read.csv("Data Files/Detrended X MSE.csv", header = FALSE)
mse.y.detrended <- read.csv("Data Files/Detrended Y MSE.csv", header = FALSE)

#### Add subject indicators to each data frame
mse.x <- mutate(mse.x, Participant = subject.vec)
mse.y <- mutate(mse.y, Participant = subject.vec)
mse.x.detrended <- mutate(mse.x.detrended, Participant = subject.vec)
mse.y.detrended <- mutate(mse.y.detrended, Participant = subject.vec)

#### Reshape to long and reformat the scale value column
mse.x <- mse.x %>%
  select(-V1) %>%
  gather(Scale, MSE, -Participant) %>%
  mutate(Direction = "Medial-Lateral",
         Type = "Raw")
mse.x$Scale <- as.numeric(gsub("V","", mse.x$Scale))

mse.x.detrended <- mse.x.detrended %>%
  select(-V1) %>%
  gather(Scale, MSE, -Participant) %>%
  mutate(Direction = "Medial-Lateral",
         Type = "Detrended")
mse.x.detrended$Scale <- as.numeric(gsub("V","", mse.x.detrended$Scale))

mse.y <- mse.y %>%
  select(-V1) %>%
  gather(Scale, MSE, -Participant) %>%
  mutate(Direction = "Anterior-Posterior",
         Type = "Raw")
mse.y$Scale <- as.numeric(gsub("V","", mse.y$Scale))

mse.y.detrended <- mse.y.detrended %>%
  select(-V1) %>%
  gather(Scale, MSE, -Participant) %>%
  mutate(Direction = "Anterior-Posterior",
         Type = "Detrended")
mse.y.detrended$Scale <- as.numeric(gsub("V","", mse.y.detrended$Scale))

#### Join data frames in long format ####
mse.data <- bind_rows(list(mse.x, mse.x.detrended, mse.y, mse.y.detrended))

#### Split participant column ####
mse.data <- extract(mse.data, Participant, into = c("Subject", "Environment", "Task"),
                    "([0-2][0-9])([L|W])([S|D])")

str(mse.data)
mse.data <- mutate_at(mse.data, vars(Subject), funs(as.numeric))


