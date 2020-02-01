### NOTES (read): presenting graphs with the x axis of "year" is confusing. 
# what the graphs are actually showing are the outcomes for the CLASS of "year"
# e.g. the bar graph shows the percent growth in female STEM students for the CLASS of 2006, 2007, etc.
# how can we make labels into "class of ____"?
# note: we want to maintain the year by year line chart so these edits should happen in a diff set?

library(readxl)
library(tidyverse)
library(tidyr)

undergradenrollment <- read_excel("undergradenrollment.xlsx")

data <- undergradenrollment %>%
  mutate(propfemale = female/undergrads*100, firstyearpropfemale = firstyearfemale/firstyearundergrads*100) %>% 
  mutate(growth = propfemale-firstyearpropfemale)

ggplot(data=data, mapping = aes(x=as.numeric(year),y=propfemale),group=year) +
  geom_line(colour="blue") +
  geom_text(aes(label=""),position = position_dodge(0.6)) +
  ggtitle("Percent Female Undergraduate Enrollment in STEM Fields") +
  xlab("Year") +
  ylab("Percent Female") +
  theme_minimal()

ggplot(data=data, mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  ylab("Percent Growth") +
  xlab("Graduating Year (Class of...)") +
  ggtitle("Percent Growth in Female STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Growth") +
  theme_minimal()

# what does the above mean? plot a line graph with prop in enrolling year and prop in graduating year:

legend_ord <- c("At Matriculation","At Graduation")

ggplot(data=data) +
  geom_point(mapping = aes(x=as.numeric(year),y=propfemale), colour = "Blue") +
  geom_point(mapping = aes(x=as.numeric(year),y=firstyearpropfemale),colour="Red") +
  geom_line(aes(x=as.numeric(year), y=propfemale), colour = "Blue") +
  geom_line(aes(x=as.numeric(year), y=firstyearpropfemale), colour = "Red") +
  ggtitle("Percent Female Undergraduate Enrollment in STEM Fields") +
  xlab("Year") +
  ylab("Percent Female") +
  theme_minimal() +
  annotate("text", x = 2010, y = 16.3, label = "Class of 2010, \n proportion female STEM \n at time of enrollment", size=3, fontface=2, colour="darkred") +
  annotate("text", x = 2010, y = 18.6, label = "Class of 2010, \n proportion female STEM \n at time of graduation", size=3, fontface=2, colour="darkblue") +
  labs(fill="Education Level") +
  scale_fill_discrete(breaks=legend_ord) #+
  ###### HOW DO I MAKE A LEGEND WITH BLUE AND RED?


  