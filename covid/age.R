library(ggplot2) 
library(ggalt)   
library(tidyverse)
# library(extrafont)
# font_import()

infected <- readr::read_csv("data/will-be-infected.csv")

infected <- infected %>% 
  mutate_if(is.numeric, function(x) {x/100}) %>% 
  mutate(diff = dem - rep)

infected_sum <- infected %>% 
  mutate(concerned = case_when(concerned == "Very concerned" ~ "Concerned",
                               concerned == "Somewhat concerned" ~ "Concerned",
                               concerned == "Not so concerned" ~ "Not concerned",
                               concerned == "Not concerned at all" ~ "Not concerned")) %>% 
  group_by(concerned) %>% 
  summarise_if(is.numeric, sum)

infected_age <- infected_sum %>% 
  pivot_longer(`18-34`:`65+`, names_to = "age") %>% 
  select(concerned, age, value)

# infected_age %>% 
#   group_by(age, concerned) %>% 
#   summarise(sum(value)) %>% 
#   pivot_wider(concerned)

percent_last <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[1:length(x)-1] <- sub("%$", "", x[1:length(x)-1])
  x
}

infected_age <- reshape2::dcast(infected_age, age ~ concerned)

infected_age$age <- factor(infected_age$age, levels=c("65+","50-64","35-49","18-34"))

infected_age$diff <- infected_age$`Concerned` - infected_age$`Not concerned`

purple <- "#9C6FD6"
brown <- "#836953"

ggplot() + geom_segment(data=infected_age, aes(y = age, yend = age, x=0, xend=.87), color="#b2b2b2", size=0.15) +
  ggalt::geom_dumbbell(data=infected_age, aes(y=age, x=`Not concerned`, xend=Concerned),
                                size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,
                                colour_x=brown, colour_xend =purple) + 
  geom_text(data=filter(infected_age, age=="65+"),
                     aes(x=`Not concerned`, y=age, label="Not concerned"),
                     color=brown, size=3, vjust=-1.5, fontface="bold", family="Lato") +
  geom_text(data=filter(infected_age, age=="65+"),
                     aes(x=`Concerned`, y=age, label="Concerned"),
                     color=purple, size=3, vjust=-1.5, fontface="bold", family="Lato") +
# text above points
  geom_text(data=infected_age, aes(x=`Concerned`, y=age, label=percent_last(Concerned)),
                     color=purple, size=2.75, vjust=2.5, family="Lato") + 
  geom_text(data=infected_age, aes(x=`Not concerned`, y=age, label=percent_last(`Not concerned`)),
                     color=brown, size=2.75, vjust=2.5, family="Lato") +
# difference column
geom_rect(data=infected_age, aes(xmin=.73, xmax=.87, ymin=-Inf, ymax=Inf), fill="grey") +
geom_text(data=infected_age, aes(label=paste0(diff*100, "%"), y=age, x=.8), fontface="bold", size=3, family="Lato") +
geom_text(data=filter(infected_age, age=="18-34"), aes(x=.8, y=age, label="Difference"),
          color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +
scale_x_continuous(expand=c(0,0), limits=c(0, .88)) +
scale_y_discrete(expand=c(0.2,0)) +
labs(x=NULL, y=NULL, title="Younger people are less worried about COVID-19",
     subtitle="How concerned are you that you or someone you know will be infected with the coronavirus?",
     caption="Source: Quinnipiac University Poll, March 9, 2020. Q28\n\nDesign: Connor Rothschild") +
theme_bw(base_family="Lato") +
theme(panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_blank(),
      axis.ticks=element_blank(),
      axis.text.x=element_blank(),
      plot.title=element_text(size = 16, face="bold"),
      plot.title.position = "plot",
      plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
      plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e"))

ggsave("outputs/age-worry.jpg")
