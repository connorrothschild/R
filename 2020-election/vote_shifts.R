library(maps)
data(state.fips)
regions <- state.fips %>% 
  select(region, polyname) %>% 
  mutate(state = str_to_title(polyname),
         state = str_replace_all(state, ":.*$", "")) %>% 
  distinct(state, region) %>% 
  mutate(region = case_when(region == 1 ~ "Northeast",
                            region == 2 ~ "Midwest",
                            region == 3 ~ "South",
                            region == 4 ~ "West"))

df_2020 <- readr::read_csv("https://raw.githubusercontent.com/kjhealy/us_elections_2020_csv/master/results_x2020_11_05_07_05_35.csv") %>% 
  filter(race == "President",
         id != "0") %>% 
  mutate(FIPS = as.numeric(id)) %>% 
  group_by(FIPS) %>% 
  mutate(percent = votes/sum(votes))
df_2016 <- readr::read_csv("./countypres_2000-2016.csv") %>% 
  filter(year == 2016,
         office == 'President') %>% 
  group_by(FIPS) %>% 
  mutate(percent = candidatevotes/sum(candidatevotes)) %>% 
  left_join(regions)

two_party_2020 <- 
  df_2020 %>% 
  select(place, FIPS, lname, votes, percent) %>% 
  filter(lname == "Trump" | lname == "Biden") %>% 
  pivot_wider(names_from = lname, values_from = c(votes, percent)) %>% 
  mutate(total_votes = votes_Trump + votes_Biden,
         biden_lead_pct = percent_Biden - percent_Trump)

two_party_2016 <- 
  df_2016 %>% 
  filter(!is.na(FIPS)) %>% 
  select(FIPS, county, state, region, candidate, votes = candidatevotes, percent) %>% 
  filter(candidate == "Donald Trump" | candidate == "Hillary Clinton") %>% 
  mutate(lname = ifelse(candidate == "Donald Trump", "Trump", "Clinton")) %>% 
  select(-candidate) %>% 
  pivot_wider(names_from = lname, values_from = c(votes, percent)) %>%
  mutate(total_votes = votes_Trump + votes_Clinton,
         clinton_lead_pct = percent_Clinton - percent_Trump)

joined <- left_join(two_party_2016, two_party_2020, by = "FIPS", suffix = c("_2016", "_2020")) %>% 
  mutate(biden_gain_pct = biden_lead_pct - clinton_lead_pct,
         place = paste0(county, " County, ", state))

library(cr)
cr::set_cr_theme()

# joined %>% 
#   ggplot(aes(x = clinton_lead_pct, y = biden_lead_pct, color = biden_gain_pct, size = total_votes_2020)) +
#   geom_point(alpha = .7) +
#   scale_color_viridis_c(guide = guide_legend(), labels = scales::percent_format()) +
#   geom_abline(slope = 1)

top_20_losses <- joined %>% 
  arrange(biden_gain_pct) %>% 
  head(20)

region_cols <- c("Northeast" = "#DE3C4B", 
  "South" = "#1089FF", 
  "Midwest" = "#012956", 
  "West" = "#FFA400")

top_20_losses %>% 
  ggplot(aes(x = fct_reorder(place, -biden_gain_pct), y = biden_gain_pct, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "Biden's Top Losses",
       subtitle = "Counties where Biden lost the greatest share of the vote, relative to Clinton's 2016 vote share",
       x = element_blank(), y = "\nBiden's Vote Share Loss Relative to 2016",
       fill = element_blank()) +
  theme(plot.title.position = 'panel',
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0)),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = region_cols) +
  drop_axis("y")
ggsave("Downloads/top_20_losses.svg")

top_20_gains <- joined %>% 
  arrange(desc(biden_gain_pct)) %>% 
  head(20)

top_20_gains %>% 
  ggplot(aes(x = fct_reorder(place, biden_gain_pct), y = biden_gain_pct, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "Biden's Top Gains",
       subtitle = "Counties where Biden gained the greatest share of the vote, relative to Clinton's 2016 vote share",
       x = element_blank(), y = "\nBiden's Vote Share Gain Relative to 2016",
  fill = element_blank()) +
  theme(plot.title.position = 'panel',
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.01)),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = region_cols) +
  drop_axis("y")
ggsave("Downloads/top_20_gains.svg")

joined %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = clinton_lead_pct, y = biden_lead_pct, color = region, size = total_votes_2020)) +
  scale_x_continuous(limits = c(-1,1), labels = scales::percent_format()) +
  scale_y_continuous(limits = c(-1,1), labels = scales::percent_format()) +
  geom_point(alpha = .7) +
  geom_abline(slope = 1) +
  scale_color_manual(values = region_cols) +
  labs(title = 'Gains and Losses',
       subtitle = 'Counties arranged by Clinton (2016) margins vs Biden (2020) margins',
       x = "Clinton's margin of victory",
       y = "Biden's margin of victory",
       color = element_blank()) +
  theme(plot.title.position = 'panel',
        legend.position = "top",
        legend.direction = 'horizontal') +
  guides(size = 'none') +
  drop_axis() +
  annotate("text", x = -.5, y = 0.6, label = "Biden overperformed relative to 2016") +
  annotate("text", x = .5, y = -0.6, label = "Biden underperformed relative to 2016")
ggsave("Downloads/over_under.svg")

joined %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = clinton_lead_pct, y = biden_lead_pct, color = region, size = total_votes_2020)) +
  scale_x_continuous(limits = c(-1,1), labels = scales::percent_format()) +
  scale_y_continuous(limits = c(-1,1), labels = scales::percent_format()) +
  geom_point(alpha = .7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = region_cols) +
  labs(title = 'Gains and Losses',
       subtitle = 'Counties arranged by Clinton (2016) margins vs Biden (2020) margins',
       x = "Clinton's margin of victory",
       y = "Biden's margin of victory",
       color = element_blank()) +
  theme(plot.title.position = 'panel',
        legend.position = "top",
        legend.direction = 'horizontal') +
  guides(size = 'none') +
  drop_axis() +
  annotate("text", x = .9, y = -0.15, label = "Clinton Victory,\nBiden Loss") +
  annotate("text", x = .9, y = 0.15, label = "Clinton Victory,\nBiden Victory") +
  annotate("text", x = -.9, y = 0.15, label = "Clinton Loss,\nBiden Victory") +
  annotate("text", x = -.9, y = -0.15, label = "Clinton Loss,\nBiden Loss")
ggsave("Downloads/four_quadrants.svg")
# joined %>% 
#   filter(clinton_lead_pct < 0 & biden_lead_pct > 0) %>% 
#   arrange(desc(biden_gain_pct)) %>% View()
# 
# joined %>% 
#   filter(clinton_lead_pct > 0 & biden_lead_pct < 0) %>% 
#   arrange(biden_gain_pct) %>% View()
