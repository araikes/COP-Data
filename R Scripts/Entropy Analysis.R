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
library(lsmeans)

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
mse.data <- extract(mse.data, Participant, into = c("Subject", "E", "T"),
                    "([0-2][0-9])([L|W])([S|D])")

str(mse.data)
mse.data <- mutate_at(mse.data, vars(Subject), funs(as.factor)) %>%
  mutate(Task = ifelse(T == "D", "Dual Task", "Single Task"),
            Environment = ifelse(E == "L", "Land", "Water")) %>%
  select(-E, -T) %>%
  mutate_if(is.character, as.factor)

str(mse.data)

#### Split into ML and AP directions ####
ap.mse <- filter(mse.data, Direction == "Anterior-Posterior")
ml.mse <- filter(mse.data, Direction == "Medial-Lateral")

#### Plot 2x2 frames for each direction ####
# Plots of single vs dual task per environment
ggplot(data = ap.mse, aes(x = Scale, y = MSE, group = interaction(Task, Subject),
                          color = Task)) +
  facet_grid(Type ~ Environment) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE, aes(group = Task, color = Task), size = 2)
  

ggplot(data = ml.mse, aes(x = Scale, y = MSE, group = interaction(Task, Subject),
                          color = Task)) +
  facet_grid(Type ~ Environment) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = Task, color = Task), se = FALSE, size = 2)

# Plots of Land vs Water for each task type
ggplot(data = ap.mse, aes(x = Scale, y = MSE, group = interaction(Environment, Subject),
                          color = Environment)) +
  facet_grid(Type ~ Task) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = Environment, color = Environment), se = FALSE, size = 2)

ggplot(data = ml.mse, aes(x = Scale, y = MSE, group = interaction(Environment, Subject),
                          color = Environment)) +
  facet_grid(Type ~ Task) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = Environment, color = Environment), se = FALSE, size = 2)

#### Compute CI ####
ap.complexity <- group_by(ap.mse, Subject, Environment, Task, Type) %>%
  summarise(CI = mean(MSE))

ml.complexity <- group_by(ml.mse, Subject, Environment, Task, Type) %>%
  summarise(CI = mean(MSE))

#### Separate dataframes for ease in subsetting for models ####
ml.raw <- filter(ml.complexity, Type == "Raw") %>%
  select(-Type)

ml.dt <- filter(ml.complexity, Type == "Detrended") %>%
  select(-Type)

ap.raw <- filter(ap.complexity, Type == "Raw") %>%
  select(-Type)

ap.dt <- filter(ap.complexity, Type == "Detrended") %>%
  select(-Type)

##### Repeated measures models with unstructured covariance ####
ap.raw.unstructured <- gls(CI ~ Environment + Task + Environment:Task,
                           cor = corSymm(form = ~1|Subject),
                           data = ap.raw,
                           method = "REML")
ap.dt.unstructured <- gls(CI ~ Environment + Task + Environment:Task,
                          cor = corSymm(form = ~1|Subject),
                          data = ap.dt,
                          method = "REML")
ml.raw.unstructured <- gls(CI ~ Environment + Task + Environment:Task,
                           cor = corSymm(form = ~1|Subject),
                           data = ml.raw,
                           method = "REML")
ml.dt.unstructured <- gls(CI ~ Environment + Task + Environment:Task,
                          cor = corSymm(form = ~1|Subject),
                          data = ml.dt,
                          method = "REML")

#### Repeated measures models with compound symmetry####
ap.raw.cs <- gls(CI ~ Environment + Task + Environment:Task,
                           cor = corCompSymm(form = ~1|Subject),
                           data = ap.raw,
                           method = "REML")
ap.dt.cs <- gls(CI ~ Environment + Task + Environment:Task,
                          cor = corCompSymm(form = ~1|Subject),
                          data = ap.dt,
                          method = "REML")
ml.raw.cs <- gls(CI ~ Environment + Task + Environment:Task,
                           cor = corCompSymm(form = ~1|Subject),
                           data = ml.raw,
                           method = "REML")
ml.dt.cs <- gls(CI ~ Environment + Task + Environment:Task,
                          cor = corCompSymm(form = ~1|Subject),
                          data = ml.dt,
                          method = "REML")

#### Model comparison and selection ####
anova(ap.raw.unstructured, ap.raw.cs)
anova(ap.dt.unstructured, ap.dt.cs)
anova(ml.raw.unstructured, ml.raw.cs)
anova(ml.dt.unstructured, ml.dt.cs)

#### Maximum likelihood models ####
ap.raw.maxlik <- gls(CI ~ Environment + Task + Environment:Task,
                     cor = corSymm(form = ~1|Subject),
                     data = ap.raw,
                     method = "ML")
ap.dt.maxlik <- gls(CI ~ Environment + Task + Environment:Task,
                    cor = corCompSymm(form = ~1|Subject),
                    data = ap.dt,
                    method = "ML")
ml.raw.maxlik <- gls(CI ~ Environment + Task + Environment:Task,
                     cor = corCompSymm(form = ~1|Subject),
                     data = ml.raw,
                     method = "ML")
ml.dt.maxlik <- gls(CI ~ Environment + Task + Environment:Task,
                    cor = corCompSymm(form = ~1|Subject),
                    data = ml.dt,
                    method = "ML")

#### Output summaries ####
summary(ap.raw.maxlik)
summary(ap.dt.maxlik)
summary(ml.dt.maxlik)
summary(ml.raw.maxlik)

plot(ap.raw.maxlik)
plot(ap.dt.maxlik)
plot(ml.dt.maxlik)
plot(ml.raw.maxlik)

anova(ap.raw.maxlik, type = "marginal")
anova(ap.dt.maxlik, type = "marginal")
anova(ml.dt.maxlik, type = "marginal")
anova(ml.raw.maxlik, type = "marginal")

contrast(lsmeans(ap.dt.maxlik, ~Task), method = "pairwise")

#### Boxplots with points ####
ggplot(data = ap.raw, aes(x = Environment, y = CI, color = Task)) +
  geom_boxplot()

#### Write output CSVs for Dr. Bressel ####
# Raw data
ap.raw.out <- unite(ap.raw, Condition, Environment, Task, sep = ": ") %>%
  spread(Condition, CI)

write.table(ap.raw.out, "Data Files/Anterior-Posterior Complexity.csv",
            col.names = TRUE, row.names = FALSE, sep = ",")

ml.raw.out <- unite(ml.raw, Condition, Environment, Task, sep = ": ") %>%
  spread(Condition, CI)

write.table(ml.raw.out, "Data Files/Medial-Lateral Complexity.csv",
            col.names = TRUE, row.names = FALSE, sep = ",")

# Detrended data
ap.dt.out <- unite(ap.dt, Condition, Environment, Task, sep = ": ") %>%
  spread(Condition, CI)

write.table(ap.dt.out, "Data Files/Detrended Anterior-Posterior Complexity.csv",
            col.names = TRUE, row.names = FALSE, sep = ",")

ml.dt.out <- unite(ml.dt, Condition, Environment, Task, sep = ": ") %>%
  spread(Condition, CI)

write.table(ml.dt.out, "Data Files/Detrended Medial-Lateral Complexity.csv",
            col.names = TRUE, row.names = FALSE, sep = ",")
