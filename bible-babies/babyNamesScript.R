library(tidyverse)
library(babynames)
library(gganimate)
library(readxl)

theme_set(theme_cr())

biblenames <- read_excel("biblebabynames.xlsx")

boybible <- biblenames %>% 
  select(boynames) %>% 
  rename(names = boynames)

girlbible <- biblenames %>% 
  filter(!is.na(girlnames)) %>% 
  select(girlnames) %>% 
  rename(names = girlnames)

biblenamesbind <- rbind(boybible, girlbible)

babynames %>% 
  mutate(biblename = ifelse(name %in% biblenamesbind$names, n, 0)) %>% 
  group_by(year) %>% 
  mutate(totalnames = sum(n)) %>% 
  mutate(totalbible = sum(biblename)) %>% 
  mutate(percentbible = totalbible/totalnames) %>% 
  summarise(mean = mean(percentbible)) 

babynames

babynames %>% 
  mutate(biblename = ifelse(name %in% biblenamesbind$names, n, 0)) %>% 
  group_by(year, sex) %>% 
  mutate(totalnames = sum(n)) %>% 
  mutate(totalbible = sum(biblename)) %>% 
  mutate(percentbible = totalbible/totalnames) %>% 
  summarise(mean = mean(percentbible)) %>% 
  ungroup %>% 
  group_by(sex) %>% 
  ggplot(aes(x=year, y=mean, col=sex)) +
  geom_line()

# mutate baby names

babynames <- babynames %>%
  mutate(biblepercent = ifelse(name %in% biblenamesbind$names, prop*100, 0)) 

# MALES
# make male rank variable
malebabynames <- babynames %>%
  filter(sex=="M") %>% 
  group_by(year) %>%
  mutate(rank = min_rank(-biblepercent) * 1,
         Value_rel = biblepercent/biblepercent[rank==1],
         Value_lbl = paste0(" ",biblepercent)) %>%
  filter(rank <=10) %>%
  ungroup()

# plot male animation

maleanimation <- malebabynames %>% 
  filter(sex=="M") %>% 
  ggplot(aes(rank, group = name, 
                fill = as.factor(name), color = as.factor(name))) +
  geom_tile(aes(y = biblepercent/2,
                height = biblepercent,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title="Most Popular Biblical Baby Names for Males", subtitle='in {closest_state}', x = element_blank(), y = "Percent of Names") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        plot.subtitle = element_text(hjust = 0, size = 16),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(maleanimation, fps = 25, duration = 20, width = 800, height = 600)

## FEMALES
# make rank variable
femalebabynames <- babynames %>%
  filter(sex=="F") %>% 
  group_by(year) %>%
  mutate(rank = min_rank(-biblepercent) * 1,
         Value_rel = biblepercent/biblepercent[rank==1],
         Value_lbl = paste0(" ",biblepercent)) %>%
  filter(rank <=10) %>%
  ungroup()

# plot animation

femaleanimation <- femalebabynames %>% 
  filter(sex=="F") %>% 
  ggplot(aes(rank, group = name, 
             fill = as.factor(name), color = as.factor(name))) +
  geom_tile(aes(y = biblepercent/2,
                height = biblepercent,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title="Most Popular Biblical Baby Names for Females", subtitle='in {closest_state}', x = element_blank(), y = "Percent of Names") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        plot.subtitle = element_text(hjust = 0, size = 16),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(femaleanimation, duration = 25, width = 800, height = 600)

# make rank variable
babynamesrank <- babynames %>%
  group_by(year) %>%
  mutate(rank = min_rank(-biblepercent) * 1,
         Value_rel = biblepercent/biblepercent[rank==1],
         Value_lbl = paste0(" ",biblepercent)) %>%
  filter(rank <=10) %>%
  ungroup()

# plot animation

babyanimation <- babynamesrank %>% 
  ggplot(aes(rank, group = name, 
             fill = as.factor(sex), color = as.factor(sex))) +
  geom_tile(aes(y = biblepercent/2,
                height = biblepercent,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title="Most Popular Biblical Baby Names", subtitle='in {closest_state}', x = element_blank(), y = "Percent of Names") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        plot.subtitle = element_text(hjust = 0, size = 16),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(babyanimation, fps = 25, duration = 30, width = 800, height = 600)
