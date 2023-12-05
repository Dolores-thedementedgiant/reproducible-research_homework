virus <- read_csv("question-5-data/Cui_etal2014.csv")
nrow(virus)
ncol(virus)

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# change column name for x column print
colnames(virus)[10] <- "Virionvolume" 
colnames(virus)[12] <- "Genomelength"

head(virus)

virus <- virus %>%
  mutate(Virionvolume_log = log(Virionvolume),
         Genomelength_log = log(Genomelength))

str(virus)

model <- lm(Virionvolume_log ~ Genomelength_log, data=virus)
summary(model)

plot <- ggplot(virus, aes(x = Genomelength_log, y = Virionvolume_log)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  xlab("Log[Genome length (kb)]") +
  ylab("Log[Virion volume (nm3)]")
plot

ggsave("virus linear regression model.png")
