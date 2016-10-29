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
tail(cop.x.data)

cop.y.data <- read.xlsx("./Data Files/Dual task analysis_Devin2.xlsx", sheet = 5,
                        startRow = 2, colNames = TRUE)
head(cop.y.data)
tail(cop.y.data)

#### Reshape data ####
# For easier MSE analysis in Python, COP data need to be in rows not columns. I
# also need to drop the NA columns. To accomplish this, I'll gather into two
# columns (names, values), drop the NA rows, and the spread the values.
cop.x.data <- cop.x.data %>%
  gather(Participant, COP.X) %>%
  filter(complete.cases(.)) %>%
  group_by(Participant) %>%
  mutate(data.point = seq(1:n()))

# COP_Y data contained an extraneous row at the bottom.
cop.y.data <- cop.y.data[1:9000,]
cop.y.data <- cop.y.data %>%
  gather(Participant, COP.Y) %>%
  filter(complete.cases(.)) %>%
  group_by(Participant) %>%
  mutate(data.point = seq(1:n()))
  
# Prep data frames for plots to examine for nonstationarity
sway.path <- left_join(cop.x.data, cop.y.data) %>%
  ungroup()

subject.vec <- cop.x.data %>%
  ungroup() %>%
  distinct(Participant) %>%
  select(Participant) %>%
  collect %>%
  .[["Participant"]]

# Spread COP data frames
cop.x.data <- ungroup(cop.x.data) %>%
  spread(data.point, COP.X)

cop.y.data <-  ungroup(cop.y.data) %>%
  spread(data.point, COP.Y)

# Drop Participant column but store to object
cop.x.data <- select(cop.x.data, -Participant)
cop.y.data <- select(cop.y.data, -Participant)

#### Write data for Python ####
write.csv(cop.x.data, file = "./Data Files/COP X.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.csv(cop.y.data, file = "./Data Files/COP Y.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#### Plot COP trace per partcipant ####

# Create list for plot capture
plot_list = list()

# Make plots
for (i in 1:length(subject.vec)) {
  tmp <- filter(sway.path, Participant == subject.vec[i]) %>%
    select(-Participant)
  
  p <- ggplot(data = tmp, aes(x = COP.X, y = COP.Y)) +
    geom_line() +
    ggtitle(paste("Participant: ", subject.vec[i])) +
    theme_bw()
  
  plot_list[[i]] <- p
}

# Write plots to Plots folder
my.dir <- getwd()

setwd("Plots")

pdf("Sway Validation.pdf")
for (i in 1:length(subject.vec)){
  print(plot_list[[i]])
}

dev.off()

# Reset working directory
setwd(my.dir)

#### Clean workspace ####
rm(list = c("cop.x.data", "cop.y.data", "my.dir", "plot_list", "p", "tmp", "i",
            "sway.path"))
          