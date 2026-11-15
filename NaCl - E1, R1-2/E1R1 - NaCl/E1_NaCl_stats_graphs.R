library(ggplot2)
library(gridExtra)
library(car)
library(multcomp)
library(tidyverse)
library(dplyr)
library(dunn.test)
#### Germination Rates ####

germ_nacl1 <- read.csv("C:/Users/smith/OneDrive - Florida Institute of Technology/Palmer Lab/Phyto-regolith project/E1 - NaCl/NES_GERM_COUNT_EXP 1_10-26-26.csv")

day1 <- subset(germ_nacl1, Day == "1")
day2 <- subset(germ_nacl1, Day == "2")
day3 <- subset(germ_nacl1, Day == "3")
day5 <- subset(germ_nacl1, Day == "5")
day7 <- subset(germ_nacl1, Day == "7")

day2$germ_rate <- as.numeric(gsub("%", "", day2$germ_.))
day3$germ_rate <- as.numeric(gsub("%", "", day3$germ_.))
day5$germ_rate <- as.numeric(gsub("%", "", day5$germ_.))
day7$germ_rate <- as.numeric(gsub("%", "", day7$germ_.))



g2 <- ggplot(day2, aes(x = Concentration..mM., y = germ_rate)) +
  geom_count() +
  scale_y_continuous(breaks = seq(0, 100, by = 15),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_size_continuous(breaks = c(1, 2, 3), range = c(1,4)) +
  labs(title = "Day 2", x = "", y = "")

show(g2)

g3 <- ggplot(day3, aes(x = Concentration..mM., y = germ_rate)) +
  geom_count() +
  scale_y_continuous(breaks = seq(0, 100, by = 15),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_size_continuous(breaks = c(1, 2, 3), range = c(1,4)) +
  labs(title = "Day 3", x = "", y = "")

show(g3)

g5 <- ggplot(day5, aes(x = Concentration..mM., y = germ_rate)) +
  geom_count() +
  scale_y_continuous(breaks = seq(0, 100, by = 15),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_size_continuous(breaks = c(1, 2), range = c(1, 3)) +
  labs(title = "Day 5", x = "", y = "")

show(g5)

g7 <- ggplot(day7, aes(x = Concentration..mM., y = germ_rate)) +
  geom_count() +
  scale_y_continuous(breaks = seq(0, 100, by = 15),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_size_continuous(breaks = c(1, 2), range = c(1,3)) +
  labs(title = "Day 7", x = "", y = "") 

show(g7)

grid.arrange(g2, g3, g5,g7, ncol=2,
             left = "Germination Rate",
             bottom = "NaCl Concentration")



#### Qualitative Growth ####



g3.2 <- day3 %>%
  mutate(Normal = Germinated - Stunted,
         plate = fct_inorder(paste(Concentration..mM., Plate.ID, sep = "-"))) %>%
  pivot_longer(c(Stunted, Normal), names_to = "type", values_to = "count") %>%
  ggplot(aes(plate, count, fill = type)) +
  geom_col(width = 0.5) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  labs(title = "Day 3", x = "", y = "", fill = "") +
  guides(fill = "none")


g5.2 <- day5 %>%
  mutate(Normal = Germinated - Stunted,
         plate = fct_inorder(paste(Concentration..mM., Plate.ID, sep = "-"))) %>%
  pivot_longer(c(Stunted, Normal), names_to = "type", values_to = "count") %>%
  ggplot(aes(plate, count, fill = type)) +
  geom_col(width = 0.5) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  labs(title = "Day 5", x = "", y = "", fill = "") +
  guides(fill = "none")
 

g7.2 <- day7 %>%
  mutate(Normal = Germinated - Stunted,
         plate = fct_inorder(paste(Concentration..mM., Plate.ID, sep = "-"))) %>%
  pivot_longer(c(Stunted, Normal), names_to = "type", values_to = "count") %>%
  ggplot(aes(plate, count, fill = type)) +
  geom_col(width = 0.5) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  labs(title = "Day 7", x = "", y = "", fill = "") +
  guides(fill = "none")

grid.arrange(g3.2, g5.2, g7.2, ncol=2,nrow =2 ,
             left = "Total Seeds Germinated",
             bottom = "NaCl Concentration -- Replicate")


#### ANOVA (have to do by day)####


## DAY 1 ##

lm_day1 <- lm(Germinated ~ Concentration..mM., data = day1)

# normality #
qqnorm(residuals(lm_day1))
qqline(residuals(lm_day1))


# variance #
boxplot(residuals(lm_day1) ~ day1$Concentration..mM.)


# ANOVA TEST #

anova(lm_day1)


## DAY 2 ##

lm_day2 <- lm(Germinated ~ Concentration..mM., data = day2)

# normality #
qqnorm(residuals(lm_day2))
qqline(residuals(lm_day2))

shapiro.test(residuals(lm_day2)) # normal, p = 0.49

hist(residuals(lm_day2))

# variance #
boxplot(residuals(lm_day2) ~ day2$Concentration..mM.) # unequal variance

# ANOVA TEST #

kruskal.test(Germinated ~ Concentration..mM., data = day2)

# Dunn Post-hoc Test #

dunn.test(day2$Germinated, day2$Concentration..mM., method = "bonferroni", list = TRUE)


## DAY 3 ##

lm_day3 <- lm(Germinated ~ Concentration..mM., data = day3)

# normality #
qqnorm(residuals(lm_day3))
qqline(residuals(lm_day3))

shapiro.test(residuals(lm_day3)) # normal, p = 0.49

hist(residuals(lm_day3))

# variance #
boxplot(residuals(lm_day3) ~ day3$Concentration..mM.) # semi-equal variance

# ANOVA TEST #

anova(lm_day3)

# Tukey Post-hoc Test #

day3$Concentration..mM. <- as.factor(day3$Concentration..mM.)

obj1 <- aov(Germinated ~ Concentration..mM., data = day3)
tukey <- glht(obj1, linfct=mcp(Concentration..mM.="Tukey"))
cld(tukey)  

summary(tukey)
# 0   100   200   300
# a    a     b     b  


## DAY 5 ##

lm_day5 <- lm(Germinated ~ Concentration..mM., data = day5)

# normality #
qqnorm(residuals(lm_day5))
qqline(residuals(lm_day5))

shapiro.test(residuals(lm_day5)) # normal, p = 0.97

hist(residuals(lm_day5))

# variance #
boxplot(residuals(lm_day5) ~ day5$Concentration..mM.) # semi-equal variance

# ANOVA TEST #

anova(lm_day5)

# Tukey Post-hoc Test #

day5$Concentration..mM. <- as.factor(day5$Concentration..mM.)

obj2 <- aov(Germinated ~ Concentration..mM., data = day5)
tukey2 <- glht(obj2, linfct=mcp(Concentration..mM.="Tukey"))
cld(tukey2)  

# 0   100   200   300
# a    a     b     b  

summary(tukey2)



## DAY 7 ##

lm_day7 <- lm(Germinated ~ Concentration..mM., data = day7)

# normality #
qqnorm(residuals(lm_day7))
qqline(residuals(lm_day7))

shapiro.test(residuals(lm_day7)) # normal, p = 0.911

hist(residuals(lm_day7))

# variance #
boxplot(residuals(lm_day7) ~ day7$Concentration..mM.) # ehhhhhhhh

# ANOVA TEST #

anova(lm_day7)

# Tukey Post-hoc Test #

day7$Concentration..mM. <- as.factor(day7$Concentration..mM.)

obj3 <- aov(Germinated ~ Concentration..mM., data = day7)
tukey3 <- glht(obj3, linfct=mcp(Concentration..mM.="Tukey"))
cld(tukey3)  

# 0   100   200   300
# a    a     b     b  

summary(tukey3)
