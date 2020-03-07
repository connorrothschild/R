library(readr)
library(tidyverse)
library(janitor)

results_2020 <- read_csv("data/results-2020.csv") %>% 
  clean_names() %>% 
  mutate(turnout = parse_number(turnout)) %>% 
  filter(county != "All COUNTIES")

results_2016 <- read_csv("data/results-2016.csv") %>% 
  clean_names() %>% 
  mutate(turnout = parse_number(turn_out)) %>% 
  filter(county != "ALL COUNTIES")

results_2020 <- results_2020 %>% pivot_longer(cols = michael_bennet:andrew_yang, names_to = "candidate")

results_2020_rank <- results_2020 %>% 
  select(county, year, candidate, value) %>% 
  group_by(county) %>% 
  mutate(rank = row_number(desc(value)))

results_2020_winner <- results_2020_rank %>% 
  filter(rank == 1)

results_2016 <- results_2016 %>% pivot_longer(cols = hillary_clinton:willie_l, names_to = "candidate")

results_2016_rank <- results_2016 %>% 
  select(county, year, candidate, value) %>% 
  group_by(county) %>% 
  mutate(rank = row_number(desc(value)))

results_2016_winner <- results_2016_rank %>% 
  filter(rank == 1)

results <- left_join(results_2016_winner, results_2020_winner, by = "county", suffix = c("2016", "2020"))

results_final <- results %>% 
  select(county, candidate2016, candidate2020)

flips <- results_final %>% 
  mutate(flip_status = case_when(candidate2016 == "hillary_clinton" && candidate2020 == "joseph_r_biden" ~ "Clinton to Biden",
                                 candidate2016 == "hillary_clinton" && candidate2020 == "bernie_sanders" ~ "Clinton to Sanders",
                                 candidate2016 == "bernie_sanders" && candidate2020 == "bernie_sanders" ~ "Sanders to Sanders",
                                 candidate2016 == "bernie_sanders" && candidate2020 == "joseph_r_biden" ~ "Sanders to Biden",
                                 TRUE ~ "Other"))

flips <- flips %>%
  ungroup() %>% 
  mutate(county = str_to_lower(county))

library(tpltheme)
set_tpl_theme(style = "Texas", font = "lato")

library(maps)
counties <- map_data("county")
texas <- counties %>% dplyr::filter(region == "texas")

final <- left_join(flips, texas, by = c("county" = "subregion")) 
  
ggplot(data = final, mapping = aes(x = long, y = lat, group = group, fill = flip_status)) +
  coord_fixed(1.3) +
  scale_fill_manual(values = c("#FF6961", "#B19BD9", "white", "#74BDEB", "#5C9B99")) +
  geom_polygon(color = "black") +
  labs(title = "Flipping Super Tuesday",
       subtitle = "County winners, 2016 - 2020",
       fill = "2016 to 2020",
       caption = "Source: Texas Secretary of State")

ggsave("outputs/texas_flipped.jpg")

final_no_coords <- final %>% 
  select(county:flip_status) %>% 
  unique()

table(final_no_coords$flip_status)

table(final_no_coords$candidate2016)