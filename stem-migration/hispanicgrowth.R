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
  mutate(prophispanic = hispanic/undergrads*100, firstyearprophispanic = firstyearhispanic/firstyearundergrads*100) %>% 
  mutate(growth = prophispanic-firstyearprophispanic)

ggplot(data=data, mapping = aes(x=(year),y=prophispanic),group=year) +
  geom_point() +
  geom_line(colour="blue") +
  geom_text(aes(label=""),position = position_dodge(0.6)) +
  ggtitle("Percent Hispanic Undergraduate Enrollment in STEM Fields") +
  xlab("Year") +
  ylab("Percent Hispanic") +
  theme_minimal()

ggplot(data=data, mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  ylab("Percent Growth") +
  xlab("Graduating Year (Class of...)") +
  ggtitle("Percent Growth in Hispanic STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Growth") +
  theme_minimal()

# what does the above mean? plot a line graph with prop in enrolling year and prop in graduating year:

legend_ord <- c("At Matriculation","At Graduation")

ggplot(data=data) +
  geom_point(mapping = aes(x=(year),y=prophispanic), colour = "Blue") +
  geom_point(mapping = aes(x=(year),y=firstyearprophispanic),colour="Red") +
  geom_line(aes(x=(year), y=prophispanic), colour = "Blue") +
  geom_line(aes(x=(year), y=firstyearprophispanic), colour = "Red") +
  ggtitle("Percent hispanic Undergraduate Enrollment in STEM Fields") +
  xlab("Year") +
  ylab("Percent Hispanic") +
  annotate("text", x = 2010, y = 8.6, label = "Class of 2010, \n proportion Hispanic STEM \n at time of enrollment", size=3, fontface=2, colour="darkred") +
  annotate("text", x = 2010, y = 10.8, label = "Class of 2010, \n proportion Hispanic STEM \n at time of graduation", size=3, fontface=2, colour="darkblue") +
  theme_minimal() +
  labs(fill="Education Level") +
  scale_fill_discrete(breaks=legend_ord)

