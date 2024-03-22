#### One-way ANOVA / Kruskall Wallis###

#1 Installing packages 
install.packages("dplyr")
install.packages("RVAideMemoire") 
install.packages("car")   
install.packages("psych") 
install.packages("rstatix") 
install.packages("DescTools") 
install.packages("pacman")
install.packages("ggplot2")
install.packages("ggstatsplot")
install.packages("FSA")
install.packages("rstatix") 
install.packages("hrbrthemes")
install.packages("viridis")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(RVAideMemoire)
library(car)
library(psych)
library(rstatix)
library(DescTools)
library(ggplot2)
library(ggstatsplot)
library(FSA)
library(rstatix)


#2 loading data and mutating column as a factor

dataPup <- Day_Pupation
dataWeight <- Bee_Weight
dataMeasure <- Bee_Measure
dataDistance <- as.data.frame(unclass(Distance_Travel),
                              stringsAsFactors = TRUE) %>%
  mutate(Group = factor (Group))

dataWeight <- as.data.frame(unclass(Bee_Weight),
                            stringsAsFactors = TRUE) %>%
  mutate(Group = factor (Group))

dataPupInd <- Day_Pupation_Individual

#3 Summarize data

glimpse(dataPup)
glimpse(dataWeight)
glimpse(dataMeasure)
glimpse(dataDistance)
glimpse(dataPupInd)

#4 Verify normality with shapiro test (RVAIdeMemoire) P<0,05 abnormal distribution
### If confirmed the abnormal distribution go to step #10 - Kruskal Wallis

byf.shapiro(Day ~ Group, dataPup)
byf.shapiro (Weight ~ Group, dataWeight)
byf.shapiro (Abd_Leng ~ Group, dataMeasure)
byf.shapiro (Distance ~ Group, dataDistance)
byf.shapiro(Day ~ Group, dataPupInd)
byf.shapiro(Day ~ Id, DataPupAll)

#5 Verify outliers (dpplyr)

dataPup %>% 
  group_by(Group) %>%   
  identify_outliers(Day) 

dataWeight %>% 
  group_by(Group) %>% 
  identify_outliers(Weight)

dataMeasure %>% 
  group_by(Group) %>% 
  identify_outliers(Abd_Leng)

dataDistance %>% 
  group_by(Group) %>% 
  identify_outliers(Distance)

#6 ANOVA for normality data

anova_Day <- aov(Day ~ Group, dataPup)
summary(anova_Day)

anova_Weight <- aov(Weight ~ Group, dataWeight)
summary(anova_Weight)

anova_measure <- aov(Abd_Leng ~ Group, dataMeasure)
summary(anova_measure)

anova_distance <- aov(Distance ~ Group, dataDistance)
summary(anova_distance)

#7 Post-hoc analysis (Desctools)

# Bee weight (mg)

PostHocTest(anova_Weight, method = "duncan")

PostHocTest(anova_Weight, method = "hsd")

PostHocTest(anova_Weight, method = "bonf")


# Bee measurement (mm)

PostHocTest(anova_measure, method = "duncan")

PostHocTest(anova_measure, method = "hsd")

PostHocTest(anova_measure, method = "bonf")

##Distance cm/minute

PostHocTest(anova_distance, method = "duncan")

PostHocTest(anova_distance, method = "hsd")

PostHocTest(anova_distance, method = "bonf")


#8 Summarize postHoc

# Bee weight (mg)

round(
  cbind(duncan = PostHocTest(anova_Weight, method = "duncan")$Group [,"pval"],
        bonf = PostHocTest (anova_Weight, method = "hsd")$Group [, "pval"],
        hsd = PostHocTest (anova_Weight, method="bonf")$Group[,"pval"])
  ,6)


# Bee measurement (mm)

round(
  cbind(duncan = PostHocTest(anova_measure, method = "duncan")$Group [,"pval"],
        bonf = PostHocTest (anova_measure, method = "hsd")$Group [, "pval"],
        hsd = PostHocTest (anova_measure, method="bonf")$Group[,"pval"])
  ,6)

##Distance cm/minute

round(
  cbind(duncan = PostHocTest(anova_distance, method = "duncan")$Group [,"pval"],
        bonf = PostHocTest (anova_distance, method = "hsd")$Group [, "pval"],
        hsd = PostHocTest (anova_distance, method="bonf")$Group[,"pval"])
  ,6)


#9 Description 

describeBy(dataWeight$Weight, group = dataWeight$Group)

describeBy(dataMeasure$Abd_Leng, group = dataMeasure$Group)

describeBy(dataDistance$Distance, group = dataDistance$Group)

# 10 - Kruskall - Wallis test

kruskal.test(Day ~ Group, data = dataPup)
kruskal.test(Day ~ Group, data = dataPupInd)

# 11 Post-hoc tests

#Mean day of pupation
dunn_test(Day ~ Group, data = dataPup, p.adjust.method = "holm")


#Individual day of pupation
dunn_test(Day ~ Group, data = dataPupInd, p.adjust.method = "bonferroni")

#12 Description 

dataPup %>% group_by(Group) %>% 
  get_summary_stats(Day, type = "median_iqr")


dataPupInd %>% group_by(Group) %>% 
  get_summary_stats(Day, type = "median_iqr")


### -------- Graphics -------------###

### Day of pupation

ggbetweenstats(
  data = dataPup,
  x = Group,
  y = Day,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  violin.args = list(width = 0),
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = F,
  bf.message = FALSE,
  ggplot.component = list(ggplot2::scale_y_continuous(
    limits = (c(5, 10))))
)+
  labs (y = "Day of onset of pupation", x = "Percent Dietary Ethanol")+
  theme(
    text = element_text (size = 24),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )+
  ggplot2::scale_color_manual(values = c("darkgreen","darkorange", "blue", "darkred"))

### Day of pupation Individual

ggbetweenstats(
  data = dataPupInd,
  x = Group,
  y = Day,
  type = "non-parametric", # ANOVA or Kruskal-Wallis
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  ggplot.component = list(ggplot2::scale_y_continuous(
    limits = (c(5, 10))))
)+
  labs (y = "Day of onset of pupation", x = "Percent Dietary Ethanol")+
  theme(
    text = element_text (size = 24),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  ggplot2::scale_color_manual(values = c("darkgreen","darkorange", "blue", "darkred"))

### Weight

ggbetweenstats(
  data = dataWeight,
  x = Group,
  y = Weightmg,
  type = "p", # ANOVA (parametric) or Kruskal-Wallis (non-parametric)
  plot.type = "scatterplot",
  boxplot.args = list(width = 0),
  pairwise.comparisons = FALSE,
  centrality.plotting = T,
  bf.message = FALSE,
  ggplot.component = list(ggplot2::scale_y_continuous(
    limits = (c(4, 14)),
    breaks = seq(4, 14, by = 2)))
)+
  labs (y = "Distance travelled (cm)", x = "Percent Dietary Ethanol")+
  theme(
    text = element_text (size = 24),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )
  ggplot2::scale_color_manual(values = c("darkgreen","darkorange", "blue", "darkred"))


##Distance traveled

ggbetweenstats(
  data = dataDistance,
  x = Group,
  y = Distance,
  type = "parametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  boxplot.args = list(width = 0),
  violin.args = list(width = 0),
  pairwise.comparisons = FALSE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  ggplot.component = list(ggplot2::scale_y_continuous(
    limits = (c(0, 30)),
    breaks = seq(0, 30, by = 10)))
)+
  labs (y = "Distance travelled (cm)", x = "Percent Dietary Ethanol")+
  theme(
    text = element_text (size = 24),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

##Velocity

ggbetweenstats(
  data = dataDistance,
  x = Group,
  y = Velocity,
  type = "parametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  violin.args = list(width = 0),
  pairwise.comparisons = FALSE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  ggplot.component = list(ggplot2::scale_y_continuous(
    limits = (c(0, 2)),
    breaks = seq(0, 2, by = 0.5)))
)+
  labs (y = "Velocity (cm/s)", x = "Percent Dietary Ethanol")+
  theme(
    text = element_text (size = 15),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5)
  )


### Chi-sq test
chisq.test(dataWeight$Group, dataWeight$Weightmg)
chisq.test(control_6weight$Group, control_6weight$Weightmg)
chisq.test(dataPup$Group, dataPup$Day)

chisq.test

anova_wieght06 <- aov(Weightmg ~ Group, control_6weight)
summary(anova_wieght06)
