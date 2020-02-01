library(geofacet)
library(tidyverse)
library(cr)

set_cr_theme()

data <- readr::read_csv("./data/final/final_data.csv")

data <- data %>% 
  mutate(state = openintro::state2abbr(nat_definition4)) %>% 
  mutate(region_name_3 = as.numeric(region_name_3))

options(scipen = 999)

data %>% 
  group_by(region_name_3) %>% 
  summarise(textbox37 = sum(textbox37)) %>% 
  ggplot(aes(x = region_name_3, y = textbox37)) +
  geom_line() +
  labs(title = "Refugee Acceptance on the Decline",
       subtitle = "Number of refugees accepted annually, 2002 - 2019",
       x = element_blank(),
       y = element_blank()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = c(2002, 2019))

data %>% 
  group_by(region_name_3, state) %>% 
  summarise(textbox37 = sum(textbox37)) %>% 
  ggplot(aes(x = region_name_3, y = textbox37)) +
  geom_line(color = "black") +
  geom_rect(mapping=aes(xmin=2009, xmax=2017, ymin=0, ymax=12000), fill = "#ADD8E6", alpha = .05) +
  geom_rect(mapping=aes(xmin=2017, xmax=2019, ymin=0, ymax=12000), fill = "#FF9999", alpha = .05) +
  scale_x_continuous(breaks = c(2002,2019)) +
  scale_y_continuous(breaks = c(0,12000)) +
  facet_geo(~ state, grid = "us_state_grid1") +
  labs(title = "Refugee Acceptance on the Decline",
       subtitle = "Number of refugees accepted annually, 2002 - 2019",
       x = element_blank(),
       y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white")
  )

data %>% 
  group_by(region_name_3, state) %>% 
  summarise(textbox37 = sum(textbox37)) %>% 
  ggplot(aes(x = region_name_3, y = textbox37)) +
  geom_line(color = "black") +
  geom_rect(mapping=aes(xmin=2009, xmax=2017, ymin=0, ymax=12000), fill = "#ADD8E6", alpha = .05) +
  geom_rect(mapping=aes(xmin=2017, xmax=2019, ymin=0, ymax=12000), fill = "#FF9999", alpha = .05) +
  scale_x_continuous(breaks = c(2002,2019)) +
  scale_y_continuous(breaks = c(0,12000)) +
  facet_geo(~ state, grid = "us_state_grid1") +
  labs(title = "Refugee Acceptance on the Decline",
       subtitle = "Number of refugees accepted annually, 2002 - 2019",
       x = element_blank(),
       y = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white")
  )

ggsave("plot.svg", plot = last_plot(), device = "svg")

## FREE SCALES
# data %>% 
#   group_by(region_name_3, state) %>% 
#   summarise(textbox37 = sum(textbox37)) %>% 
#   ggplot(aes(x = region_name_3, y = textbox37)) +
#   geom_line() +
#   # geom_rect(mapping=aes(xmin=2017, xmax=2019, ymin=0, ymax=max(textbox37)), fill = "#FF9999", alpha = .05) +
#   scale_x_continuous(breaks = c(2002,2019)) +
#   # scale_y_continuous(breaks = c(0,12000)) +
#   facet_geo(~ state, grid = "us_state_grid1", scales = "free_y") +
#   labs(title = "Refugee Acceptance on the Decline",
#        subtitle = "Number of refugees accepted annually, 2002 - 2019",
#        x = element_blank(),
#        y = element_blank()) +
#   geom_text(x = 2019, y = 1000, label = "textbox37")
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#   )
