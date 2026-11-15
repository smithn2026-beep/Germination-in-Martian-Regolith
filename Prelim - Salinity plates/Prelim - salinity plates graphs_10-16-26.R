# csv with all of the germination rates found in Phytolith folder
germination <- read.csv("C:/Users/smith/OneDrive - Florida Institute of Technology/Palmer Lab/Phyto-regolith project/Prelim - Salinity plates/germination_counts.csv")


#call libraries
library(ggplot2)
library(dplyr)


#combining salt type and concentration into a new column
germination$Variables <- paste(germination$Salt, germination$concentration..mM., sep="_")


#sub setting the data set by the day the data was taken (1 and 6)
day1 <- subset(germination, Days_After_Germination == "1")
day6 <- subset(germination, Days_After_Germination == "6")


### Day 1 data ----
library(readr)
day1$germ_rate <- parse_number(as.character(day1$germ_rate))
sum(is.na(day1$germ_rate))  


ggplot(day1, aes(x = Variables, y = germ_rate, colour = factor(concentration..mM.))) +
  geom_point(size = 2.5, position = position_jitter(width = .3, height = .15)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Germination Rates in Salt 1 DAG", x = "Salt Type and Concentration", y = "Germination Rate") +
  theme(plot.title.position = "panel" ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


### Day 6 data----

library(readr)
day6$germ_rate <- parse_number(as.character(day6$germ_rate))
sum(is.na(day6$germ_rate))  



ggplot(day6, aes(x = Variables, y = germ_rate, colour = factor(concentration..mM.))) +
  geom_point(size = 2.5, position = position_jitter(width = .3, height = .15)) +
  scale_y_continuous(breaks = seq(45, 100, by = 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Germination Rates in Salt 6 DAG", x = "Salt Type and Concentration", y = "Germination Rate") +
  theme(plot.title.position = "panel" ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

