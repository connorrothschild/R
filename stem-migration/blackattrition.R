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
  mutate(propblack = black/undergrads*100, firstyearpropblack = firstyearblack/firstyearundergrads*100) %>% 
  mutate(growth = propblack-firstyearpropblack)

ggplot(data=data, mapping = aes(x=year,y=propblack, group=year)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=""),position = position_dodge(0.6)) +
  ggtitle("Percent Black Undergraduate Enrollment in STEM Fields") +
  xlab("Year") +
  ylab("Percent Black") +
  theme_minimal()

ggplot(data=data, mapping=aes(x=year,y=growth)) +
  geom_col(aes(fill=growth)) +
  ylab("Percent Attrition") +
  xlab("Class") +
  ggtitle("Percent Change in Black STEM Undergrads",subtitle = "Between time of matriculation and time of graduation") +
  labs(fill="Attrition") +
  theme_minimal()

