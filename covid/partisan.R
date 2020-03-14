library(ggplot2) 
library(ggalt)   
library(tidyverse)
# library(extrafont)
# font_import()

infected <- readr::read_csv("data/will-be-infected.csv")

infected <- infected %>% 
  mutate_if(is.numeric, function(x) {x/100}) %>% 
  mutate(diff = dem - rep)

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

blue <- "#0171CE"
red <- "#DE4433"

ggplot() +
# doing this vs y axis major grid line
  geom_segment(data=infected, aes(y=concerned, yend=concerned, x=0, xend=.5), color="#b2b2b2", size=0.15) +
  ggalt::geom_dumbbell(data=infected, aes(y=concerned, x=rep, xend=dem),
                           size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,
                           colour_x = red, colour_xend = blue) +
# text below points
  geom_text(data=filter(infected, concerned=="Very concerned"),
            aes(x=dem, y=concerned, label="Democrats"),
            color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
  geom_text(data=filter(infected, concerned=="Very concerned"),
            aes(x=rep, y=concerned, label="Republicans"),
            color=red, size=3, vjust=-1.5, fontface="bold", family="Lato") +
# text above points
  geom_text(data=infected, aes(x=rep, y=concerned, label=percent_first(rep)),
            color=red, size=2.75, vjust=2.5, family="Lato") +
  geom_text(data=infected, color=blue, size=2.75, vjust=2.5, family="Lato",
            aes(x=dem, y=concerned, label=percent_first(dem))) +
# difference column
  geom_rect(data=infected, aes(xmin=.5, xmax=.6, ymin=-Inf, ymax=Inf), fill="grey") +
  geom_text(data=infected, aes(label=paste0(diff*100, "%"), y=concerned, x=.55), fontface="bold", size=3, family="Lato") +
  geom_text(data=filter(infected, concerned=="Very concerned"), aes(x=.55, y=concerned, label="Difference"),
            color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +
  scale_x_continuous(expand=c(0,0), limits=c(0, .625)) +
  scale_y_discrete(expand=c(0.2,0)) +
  labs(x=NULL, y=NULL, title="Republicans are less worried about COVID-19",
       subtitle="How concerned are you that you or someone you know will be infected with the coronavirus?",
       caption="Source: Quinnipiac University Poll, March 9, 2020. Q27\n\nDesign: Connor Rothschild") +
  theme_bw(base_family="Lato") +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  )

ggsave("outputs/partisan-worry.jpg")
