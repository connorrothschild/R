library(tidyverse)
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
library(viridis)
library(cr)
set_cr_theme()

cases_df <- readr::read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

month_maxes <- cases_df %>% 
  # Remove missing counties or 'entire state' rows
  filter(!is.na(fips)) %>% 
  # Arrange, per county, by daate
  arrange(fips, desc(date)) %>% 
  # Grab new cases 
  mutate(new_cases = cases - lead(cases, 1)) %>% 
  # Remove the first day, which would be a massive negative number
  filter(new_cases > -1) %>% 
  # Month variable
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  # Group by county & month
  group_by(month, fips) %>% 
  # Compute cases per month
  mutate(month_case = sum(new_cases, na.rm = T)) %>%
  ungroup() %>% 
  group_by(fips) %>% 
  # Highest month
  mutate(peak_month = max(month_case)) %>% 
  # Only incldue those that match peak (e.g. filter out all non-peak months)
  filter(peak_month == month_case) %>% 
  # Distinct, etc.
  distinct(fips, peak_month, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(fips, county, state, month, peak_month)

df <- left_join(month_maxes, counties, by = c('fips' = 'county_fips'))

ggplot(df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = month), colour = 'white', size = .05) +
  coord_fixed(1.3) +
  scale_fill_viridis(discrete = TRUE, option = 'plasma', direction = -1,
                     guide = guide_legend(label.position = "bottom", nrow = 1, keywidth = 3)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()
  ) +
  labs(title = 'When COVID-19 Peaked',
       subtitle = 'County shading corresponds to the month that saw the greatest sum of new cases',
       caption = 'Design: Connor Rothschild\nUpdated November 17, 2020',
       fill = element_blank(),
       x = element_blank(), 
       y = element_blank())

# ggsave("./map.svg")

#### SAME THING FOR DEATHS RATHER THAN CASES
month_maxes <- cases_df %>% 
  filter(!is.na(fips)) %>% 
  arrange(fips, desc(date)) %>% 
  mutate(new_deaths = deaths - lead(deaths, 1)) %>% 
  filter(new_deaths > -1) %>% 
  group_by(fips) %>% 
  mutate(peak = max(new_deaths)) %>% 
  ungroup() %>% 
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  group_by(month, fips) %>% 
  mutate(month_case = sum(new_deaths, na.rm = T)) %>%
  group_by(fips) %>% 
  mutate(peak_month = max(month_case)) %>% 
  filter(peak_month == month_case) %>% 
  distinct(fips, peak_month, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(fips, county, state, month, peak_month)

df <- left_join(month_maxes, counties, by = c('fips' = 'county_fips'))

ggplot(df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = month), colour = 'white', size = .05) +
  coord_fixed(1.3) +
  scale_fill_viridis(discrete = TRUE, option = 'plasma', direction = -1,
                     guide = guide_legend(label.position = "bottom", nrow = 1, keywidth = 3)) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()
  ) +
  labs(title = 'When COVID-19 Peaked',
       subtitle = 'County shading corresponds to the month that saw the greatest sum of new deaths',
       caption = 'Design: Connor Rothschild\nUpdated November 17, 2020',
       fill = element_blank(),
       x = element_blank(), 
       y = element_blank())

ggsave("./deaths.png")
