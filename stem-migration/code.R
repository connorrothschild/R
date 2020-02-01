###### Trends in STEM Throughout College
##### How different demographics move in and out of STEM majors throughout their undergraduate years

rm(list=ls())

library(readxl)
library(tidyverse)
library(tidyr)
library(knitr)

#+ fig.width=10, fig.height=7

undergradenrollment <- read_excel("undergradenrollment.xlsx")

## This command collapses the "undergradenrollment" file into % growth year by year.
## It then uses the gather function to  put each "type" of % growth 

data <- undergradenrollment %>%
  mutate(propfemale = female/undergrads*100, firstyearpropfemale = firstyearfemale/firstyearundergrads*100) %>% 
  mutate(femalegrowth = propfemale-firstyearpropfemale) %>% 
  mutate(prophispanic = hispanic/undergrads*100, firstyearprophispanic = firstyearhispanic/firstyearundergrads*100) %>% 
  mutate(hispanicgrowth = prophispanic-firstyearprophispanic) %>% 
  mutate(propblack = black/undergrads*100, firstyearpropblack = firstyearblack/firstyearundergrads*100) %>% 
  mutate(blackgrowth = propblack-firstyearpropblack) %>% 
  select(year,femalegrowth,blackgrowth,hispanicgrowth,propfemale,prophispanic,propblack)  %>% 
  gather("type", "growth", 2:4) %>% 
  gather("proportiontype","proportion",2:4)

# plots the proportion of a given class's overall enrollment in STEM majors

ggplot(data=data, mapping = aes(x=year,y=proportion, colour=proportiontype, group=proportiontype)) +
  geom_point() +
  geom_line() +
  ggtitle("Demographic Proportion of Overall Enrollment in STEM Majors", subtitle="Over time") +
  xlab("Class") +
  ylab("Percent of Overall Class") +
  theme_minimal() +
  scale_color_discrete(name="Demographic",
                      breaks=c("propblack","propfemale","prophispanic"),
                      label=c("Black","Female","Hispanic"))

# plots the differences in growth/attrition by year

ggplot(data, aes(fill=type, y=growth, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Class") +
  ylab("Percent Change") +
  theme_minimal() +
  ggtitle("Percent Change in Proportion of Overall STEM Class", subtitle="Between time of matriculation and time of graduation") +
  scale_fill_discrete(name="Demographic",
                      label=c("Black","Female","Hispanic"))

# in focus: changes in black STEM enrollment as a proportion of overall class

data %>% filter(type=="blackgrowth") %>% 
  ggplot(mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) + 
  scale_fill_gradient(low = "darkred", high = "#F8766D") +
  ylab("Percent Attrition") +
  xlab("Class") +
  ggtitle("Percent Change in Black STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Attrition") +
  theme_minimal()

# in focus: changes in female STEM enrollment as a proportion of overall class

data %>% filter(type=="femalegrowth") %>% 
  ggplot(mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  scale_fill_gradient(low = "#00BA38", high = "darkgreen") +
  ylab("Percent Growth") +
  xlab("Class") +
  ggtitle("Percent Change in Hispanic STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Growth") +
  theme_minimal()

# in focus: changes in hispanic STEM enrollment as a proportion of overall class

data %>% filter(type=="hispanicgrowth") %>% 
  ggplot(mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  scale_fill_gradient(low = "#619CFF", high = "darkblue") +
  ylab("Percent Growth") +
  xlab("Class") +
  ggtitle("Percent Change in Hispanic STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Growth") +
  theme_minimal()

# previous graphs, faceted

labels <- c(blackgrowth = "Black", femalegrowth = "Female",hispanicgrowth="Hispanic") 
data %>% ggplot(mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  ylab("Percent Change") +
  xlab("Class") +
  ggtitle("Percent Change in Proportion of STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Percent\nChange") +
  theme_minimal() +
  facet_grid(. ~ type, labeller=labeller(type = labels)) +
  scale_x_discrete(labels = c("Class of 2006" = "2006", "Class of 2007" = "2007", "Class of 2008" = "2008", "Class of 2009" = "2009", "Class of 2010" = "2010", "Class of 2011" = "2011", "Class of 2012" = "2012", "Class of 2013" = "2013"))
  