## reference https://github.com/fivethirtyeight/data/tree/master/media-mentions-2020

library(httr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(viridis)
library(lubridate)
library(RColorBrewer)
library(devtools)

theme_cr <- function () {
  theme_bw(base_size=12, base_family="sans") %+replace% 
    theme(axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                          size = 0.2))
}

data <- read.csv(("https://raw.githubusercontent.com/fivethirtyeight/data/master/media-mentions-2020/cable_weekly.csv"), header=T)

data <- data %>% 
  select(name,date,pct,matched_clips,total_clips) %>% 
  mutate(date = as.Date(date))

data %>% 
  group_by(name) %>% 
  summarise(pct = mean(pct)) %>% 
  top_n(12, wt = pct) %>% 
  ggplot(aes(x=reorder(name,pct),y=pct, fill=name)) +
  geom_col(show.legend=FALSE) +
  coord_flip() +
  labs(x=element_blank(),
       y="Percent of Media Mentions",
       title="Average Proportion of Media Coverage on a Weekly Basis") +
  theme_cr()

data %>% 
  group_by(name) %>% 
  filter(mean(pct) > 0.1) %>% 
  ungroup() %>% 
ggplot(aes(x = pct, y = reorder(name,pct), fill=name, color=name)) +
  geom_density_ridges(aes(point_colour=name), 
                      show.legend = FALSE, 
                      alpha = .2, 
                      point_alpha = 1, 
                      jittered_points = TRUE) +
  labs(x = "Percent of Media Mentions",
       y=element_blank(),
       title="Media Mentions of Each Candidate",
       subtitle="With density ridges depicting average mentions on a weekly basis") +
  theme_cr() 

data %>% 
  group_by(name) %>% 
  filter(mean(pct) > .1) %>% 
  ungroup %>%
  ggplot(aes(x=date,y=pct,color=name)) +
  geom_point() +
  geom_line() +
  geom_label_repel(data=subset(data, pct>3),
           label = "Joe Biden announces candidacy",
           nudge_x = -40, nudge_y=-.3,
           show.legend = FALSE,
           color="black") +
  geom_label_repel(data=subset(data, pct > 2.5 & pct <3),
                   label = "Lucy Flores accuses Biden of \n inappropriate touching",
                   nudge_x = -50, nudge_y=-.3,
                   show.legend = FALSE,
                   color="black") +
  theme_cr() +
  labs(x=element_blank(),
       y="Percent of Media Mentions",
       title="Media Mentions of Candidates Over Time") +
  scale_color_discrete(name="Candidate")

data %>% 
  group_by(name) %>% 
  mutate(change = (pct-(dplyr::lag(pct, n=1, default=NA)))) %>% 
  filter(change>.6 | change < -.6) %>% 
  ggplot(aes(x=reorder(as.factor(date),change),y=change, fill=name)) +
  geom_col() +
  scale_fill_discrete(name="Candidate") +
  theme_cr() +
  theme(axis.text.x = element_text(angle = 30)) +
  labs(x=element_blank(),
       y="Percent Change",
       title="Largest Differences in Weekly Media Mentions",
       subtitle="Subtracting a given week's % mentions from the week prior")

nb.cols <- 22
mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(nb.cols)
show.top.n <- 10

data <- data %>% 
  group_by(date) %>% 
  arrange(date, desc(pct), name) %>%  
  mutate(rank = row_number())

recentdata <- data %>% filter(date > "2019-03-01")

finranking <- recentdata %>% 
  filter(date=="2019-05-19") %>% 
  select(date,name,rank) 

startranking <- recentdata %>% 
  filter(date=="2019-03-03") %>% 
  select(date,name,rank) 

recentdata %>% 
  group_by(date) %>% 
  arrange(date, desc(pct), name) %>%  
  mutate(rank = row_number()) %>% 
  ggplot(aes(x=date,y=rank,group=name, label=name)) +
  geom_line(aes(color=name, alpha = 1), size = 2) +
  geom_point(aes(color = name, alpha = 1), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
  scale_fill_manual(values = mycolors) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_date(expand = c(0,21)) +
  coord_cartesian(ylim = c(1,show.top.n)) +
  geom_text(data = subset(startranking), size=3, 
            aes(x = date, hjust = 1.2)) +
  geom_text(data = subset(finranking), size=3, 
            aes(x = date, hjust = -.2)) +
  theme_cr() +
  theme(legend.position = "none") +
  labs(x = "Date",
       y = "Rank",
       title = "The Race for Media Attention",
       subtitle = "Candidates ranked by weekly media mentions")
