getwd()
## Clear console, load packages
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
options(scipen=999)

## Load three datasets: 1) educational attainment broke down by occupation, provided by BLS https://www.bls.gov/emp/ep_education_training_system.htm
# 2) salaries, median hourly/annual wages broke down by occupation, provided by BLS https://www.bls.gov/oes/current/oes_nat.htm#11-0000
# 3) risk of automation broken down by occupation, provided by Carl Benedikt Frey and Michael A. Osborne (but compiled at https://data.world/wnedds/occupations-by-state-and-likelihood-of-automation)
education <- read_excel("./education.xlsx", skip=1)
salary <- read_excel("./national_M2017_dl.xlsx")
automation <- read_excel("./raw_state_automation_data.xlsx")


#The three datasets are "education", "salary", and "automation"

## One thing I'm curious about is what amount each occupation contributes to the American economy (in $) so I'll create a new variable using mutate() to find that out
salary
salary1 <- salary %>% 
  group_by(OCC_TITLE) %>% 
  mutate(natlwage = TOT_EMP * as.numeric(A_MEAN)) %>%
  filter(!is.na(TOT_EMP)) %>%
  filter(!is.na(A_MEAN)) %>%
  filter(!is.na(A_MEDIAN))
  
salary1$A_MEDIAN = as.numeric(as.character(salary1$A_MEDIAN))

## Salary is still messy so I'm going to tidy up that dataset using some basic filtering (aka leave only what I care about)

salary2 <- select(salary1, OCC_TITLE, TOT_EMP, A_MEDIAN, natlwage)
View(salary2)
# Now I need to remove duplicate rows, BLS provided duplicates for different occupational groups but  I don't care about those
salary2 <- salary2 %>% distinct

## And now to find what I'm most curious about (overall contribution to U.S. economy)...
salary2 %>%
  arrange(desc(natlwage))
    #it seems as if management occupations contribute the most, followed by office and admin support, and healthcare practictioners and technical occupations

# I hate scientific notation so I'll convert the E^...'s to '000s

options(scipen=999)

# A quick lo-fi graph (not to be used anywhere) which represents the relationship between a job's median wage and the number of Americans which have that job
salary2 %>% filter(TOT_EMP < 15000000) %>%
  ggplot(mapping=aes(x=TOT_EMP, y=A_MEDIAN)) +
  geom_point(alpha=1/3, col="red") +
  theme_bw() +
  ggtitle("Median Wage vs Total Employment") +
  xlab("Number of Americans in a Given Job") +
  ylab("Median Wage of a Given Job")

##### TO-DO: LABEL THE TOP (EXTREME POINTS) AS WAS DONE BY FIREARM CONTROL GUY

### SO, we now have three data points: the name of an occupation, the annual median wage of that occupation, and the amount it contributes to the American economy (or, more specifically, the amount workers are paid as an entire occupation for their work, annually)

# Next, I want to cross-reference the salary data with the education data. Let's clean the edu. data up to make sure it's all good to go as well.

View(education)
education1 <- education %>%
  select(-X__1)
  # I just eliminated X__1, some indicator BLS used for where they got there data or something, unrelated to this work

View(education1)
library(plyr)
education1 <- rename(education1, c("2016 National Employment Matrix title and code" = "occupation", 
                     "Less than high school diploma" = "lessthanhs", 
                     "High school diploma or equivalent" = "hsdiploma",
                     "Some college, no degree" = "somecollege",
                     "Associate's degree" = "associates",
                     "Bachelor's degree" = "bachelors",
                     "Master's degree" = "masters",
                     "Doctoral or professional degree" = "professional"))
View(education1)
library(dplyr)

education2 <- education1 %>% 
  group_by(occupation) %>%
  mutate(hsorless = lessthanhs + hsdiploma,
         somecollegeorassociates = somecollege + associates,
         postgrad = masters + professional)
            #CONSIDER: OTHER classifications to be made? what else is relevant?
View(education2)
education2 <- education2 %>% drop_na()

education2
salary2
salary2 <- rename(salary2, c("OCC_TITLE" = "occupation"))
salary2$occupation <- tolower(salary2$occupation)
education2$occupation <- tolower(education2$occupation)
edsal <- merge(as.data.frame(education2), as.data.frame(salary2), by="occupation")

View(education2)
View(salary2)
View(edsal)
edsal <- edsal %>% drop_na()

## At this point I'm realizing that having the educational breakdown per job is interesting but may not be able to reveal a lot of key insights (at least, not with my lack of technical knowledge)
  # So, I'm going to add a fourth dataset: the typical education of a worker in a given occupation, also provided by BLS.

typicaleducation <- read_excel("~/Downloads/typicaleducation.xlsx")
View(typicaleducation)

typicaleducation2 <- typicaleducation %>% select(occupation,typicaled,workexp)
View(typicaleducation2)

typicaleducation2 <- typicaleducation2 %>% drop_na()
typicaleducation2$occupation <- tolower(typicaleducation2$occupation)

View(salary2)
View(education2)
View(typicaleducation2)
edsal2 <- merge(as.data.frame(edsal), as.data.frame(typicaleducation2), by="occupation")

View(edsal2)

edsal2 %>%
  group_by(typicaled) %>%
  summarise(medianwage = median(A_MEDIAN))
detach(package:plyr)
edsal3 <- edsal2 %>%
  group_by(typicaled) %>%
  mutate(medianwage = mean(A_MEDIAN, na.rm=TRUE)) %>% 
  filter(medianwage < 5000000) %>%
  arrange(medianwage) %>%
  select(medianwage) %>%
  ungroup(typicaled)

edsal3 <- edsal2 %>% 
  group_by(typicaled) %>% 
  summarise(medianwage = mean(A_MEDIAN))

edsal3

legend_ord <- levels(with(edsal3, reorder(typicaled, medianwage)))

ggplot(data=edsal3, aes(x = reorder(typicaled, medianwage), y = medianwage)) +
  geom_col(aes(fill=typicaled)) +
  ggtitle("Median Annual Income by Education Level") +
  xlab("Education Level")+
  ylab("Median Annual Income") +
  labs(fill="Education Level") +
  scale_fill_discrete(breaks=legend_ord) +
  theme_minimal() +
  theme(axis.text.x=element_blank())

#### LASTLY, let's bring in the automation data
  
View(automation)
automationwstates <- automation %>% select(-soc)
automation1 <- automationwstates %>% select(occupation,probability,total)

View(automation1)

# Merge automation with existing data
    # One problem is that the automation data uses semicolons in lists while the rest of the data uses commas

automation1$occupation <- str_replace_all(automation1$occupation, ";", ",")
# Next we lowercase it so we can merge...
automation1$occupation <- tolower(automation$occupation)
data <- merge(as.data.frame(edsal2), as.data.frame(automation1), by="occupation")
# And it's merged!
View(data)

autovsedu <- data %>% 
  group_by(typicaled) %>% 
  summarise(medianwage = mean(A_MEDIAN),
            averageprobability = mean(probability))

legend_ord2 <- levels(with(autovsedu, reorder(typicaled, -averageprobability)))
ggplot(data=autovsedu, aes(x = reorder(typicaled, -averageprobability), y = averageprobability)) +
  geom_col(aes(fill=typicaled)) +
  ggtitle("Likelihood of Job Automation by Education Level") +
  xlab("Education Level")+
  ylab("Likelihood of Job Automation") +
  labs(fill="Education Level") +
  scale_fill_discrete(breaks=legend_ord2) +
  theme_minimal() +
  theme(axis.text.x=element_blank())

ggplot(data=data) +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP, alpha=1/10, col=typicaled))+
  scale_size(range = c(1, 8)) +
  ylim(-.01,1) +
  xlab("Median Income") +
  ylab("Probability of Automation") +
  ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size="Total Employment", col="Education Level") +
  labs(alpha=NULL) +
  guides(alpha=FALSE) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  guides(col = guide_legend(override.aes = list(size=5))) +
  theme_minimal()

#above is a graph with no labels or anything. lo fidelity version. let's modify it some

data$occupation <- toTitleCase(data$occupation)

ggplot(data=data) +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP, alpha=0.05, col=typicaled))+
  geom_smooth(aes(x=A_MEDIAN, y=probability), method="lm", se=FALSE) +
  scale_size(range = c(1, 12)) +
  ylim(-.05,1.05) +
  xlim(25000,200000) +
  xlab("Median Income") +
  ylab("Probability of Automation") +
  ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size="Total Employment", col="Typical Education Level") +
  labs(alpha=NULL) +
  guides(alpha=FALSE) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  guides(col = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  geom_label_repel(aes(A_MEDIAN,probability,label=occupation),subset(data, A_MEDIAN > 175000 & probability < .05), label.size=.5, label.r=.05, size=2.5, nudge_y = .05, nudge_x= -10000) +
  geom_label_repel(aes(A_MEDIAN,probability,label=occupation),subset(data, A_MEDIAN == 21030), label.size=.1, label.r=.01, size=1, nudge_y = 0,nudge_x=0) +
  geom_label_repel(aes(A_MEDIAN,probability,label=occupation),subset(data, A_MEDIAN == 24540), label.size=.1, label.r=.01, size=1, nudge_y = 0,nudge_x=0) +
  geom_label_repel(aes(A_MEDIAN,probability,label=occupation),subset(data, A_MEDIAN > 100000 & probability > .90), label.size=.5, label.r=.05, size=2.5, nudge_y = -.05,nudge_x=10000) +
  annotate("text", x = 165000, y = 1.03, label = "Highest salary,\n highest automation risk", size=3, fontface=2) +
  annotate("text", x = 165000, y = -0.035, label = "Highest salary,\n lowest automation risk", size=3, fontface=2) +
  annotate("text", x = 45000, y = -0.035, label = "Lowest salary,\n lowest automation risk", size=3, fontface=2) +
  annotate("text", x = 45000, y = 1.03, label = "Lowest salary,\n highest automation risk", size=3, fontface=2)


### NICE! We have some alright data visualizations. Let's do some of the wonky linear regression stuff now. I don't know how this works but i'll try.

summary1 <- lm(probability~typicaled,data=data)
summary(summary1)

# Coefficients:
# Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                                 0.45680    0.04409  10.361 < 0.0000000000000002 ***
# typicaledBachelor's degree                 -0.25883    0.04950  -5.229       0.000000229908 ***
# typicaledDoctoral or professional degree   -0.34749    0.07777  -4.468       0.000009298698 ***
# typicaledHigh school diploma or equivalent  0.26152    0.04684   5.583       0.000000034807 ***
# typicaledMaster's degree                   -0.34050    0.06638  -5.130       0.000000383913 ***
# typicaledNo formal educational credential   0.34161    0.05304   6.441       0.000000000231 ***
# typicaledPostsecondary nondegree award      0.01652    0.06085   0.272               0.7861    
# typicaledSome college, no degree            0.30320    0.16299   1.860               0.0633 . 

## If I understand the data correctly: Having a Bachelor's degree decreases the likelihood of one's job being automated by .25 units, having a prof. degree decreases the likelihood by .347, etc...

cor(data$probability, data$A_MEDIAN)
## There is a correlation of -0.5485691 between the probability of job automation and median incomes.

?save
write.csv(data, file ="~/Downloads/automationproj.csv")
