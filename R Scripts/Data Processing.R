#### COP Data Analysis ####
# This is a collaborative project between Dr. Eadric Bressel, Adam Raikes, Talin
# Louder, and Devin Patterson. In this arm of the analysis, I will analyze the
# raw X and Y COP data to examine the multiscale complexity of single and dual
# task postural sway on land and water.

# The data was provided to me as a .xlsx file with 4 tabs. Tabs 4 and 5 are the
# COPx and Y. However, these are untidy data with blank columns to separate
# conditions. Prior to computing multiscale entropy, this dataset requires
# substantial cleaning.

#### Load libraries ####
library(tidyverse)
library(openxlsx)

#### Read data ####
cop.x.data <- read.xlsx("./Data Files/Dual task analysis_Devin2.xlsx", sheet = 4,
                        startRow = 2, colNames = TRUE)
head(cop.x.data)

cop.y.data <- read.xlsx("./Data Files/Dual task analysis_Devin2.xlsx", sheet = 5,
                        startRow = 2, colNames = TRUE)
head(cop.y.data)

#### Reshape data ####
# For easier MSE analysis in Python, COP data need to be in rows not columns. I
# also need to drop the NA columns. To accomplish this, I'll gather into two
# columns (names, values), drop the NA rows, and the spread the values.
cop.x.data <- cop.x.data %>%
  gather(Participant, COP.X) %>%
  filter(complete.cases(.)) %>%
  group_by(Participant) %>%
  mutate(data.point = seq(1:n()))

# Plot subset for verification
ggplot(data = cop.x.data[cop.x.data$Participant == "01LD",], aes(x = data.point, y = COP.X)) +
  geom_line()

cop.x.data <- ungroup(cop.x.data) %>%
  spread(data.point, COP.X)

cop.y.data <- cop.y.data %>%
  gather(Participant, COP.Y) %>%
  filter(complete.cases(.)) %>%
  group_by(Participant) %>%
  mutate(data.point = seq(1:n()))
  
# Plot subset for verification
ggplot(data = cop.y.data[cop.y.data$Participant == "01LD",], aes(x = data.point, y = COP.Y)) +
  geom_line()

cop.y.data <-  ungroup(cop.y.data) %>%
  spread(data.point, COP.Y)

# Drop Participant column but store to object
participants <- cop.y.data$Participant
cop.x.data <- select(cop.x.data, -Participant)
cop.y.data <- select(cop.y.data, -Participant)

#### Write data for Python ####
write.csv(cop.x.data, file = "./Data Files/COP X.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.csv(cop.y.data, file = "./Data Files/COP Y.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#### Clean workspace ####
rm(list = c("cop.x.data", "cop.y.data"))
          