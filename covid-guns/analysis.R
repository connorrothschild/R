library(tidyverse)
library(geofacet)
library(scales)
library(cr)

set_cr_theme(font = "IBM Plex Sans")

options(scipen = 999)

# data is from BuzzFeed News https://github.com/BuzzFeedNews/nics-firearm-background-checks
data <- readr::read_csv('https://raw.githubusercontent.com/BuzzFeedNews/nics-firearm-background-checks/master/data/nics-firearm-background-checks.csv')

data <- data %>%
  mutate(month = as.Date(paste(month,"-01",sep="")),
         year = lubridate::year(month)) %>%
  filter(year != 1998)

data %>%
  group_by(month) %>%
  summarise(sum = sum(totals)) %>%
  ggplot(aes(x = month, y = sum)) +
  geom_line() +
  geom_area(alpha = .7, fill = 'red') +
  labs(y = element_blank(), x = element_blank(),
       title = 'Firearm background checks, over time') +
  scale_y_continuous(expand = expansion(c(0, 0.001)),
                     labels = scales::unit_format(unit = "M",
                                                  scale = 1e-6,
                                                  sep = "",
                                                  accuracy = 1))
# data %>%
#   group_by(month, state) %>%
#   summarise(sum = sum(totals)) %>%
#   ggplot(aes(x = month, y = sum)) +
#   geom_line() +
#   geom_area(alpha = .7) +
#   facet_geo(~state)

data %>%
  mutate(month = lubridate::month(month)) %>%
  filter(month < 7) %>%
  group_by(year) %>%
  summarise(sum = sum(totals)) %>%
  ungroup() %>%
  mutate(last_year = lag(sum),
         perc_change =  ((sum - last_year) / last_year)) %>%
  ggplot(aes(x = year, y = perc_change)) +
  geom_col(alpha = .7, fill = 'red') +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  drop_axis() +
  labs(x = element_blank(), y = element_blank(),
       title = "Year-on-year changes in firearm background checks, 1999-2020")

data %>%
  group_by(state, year) %>%
  summarise(sum = sum(totals)) %>%
  arrange(state) %>%
  mutate(perc_change = (sum/lead(sum) - 1)) %>%
  mutate(perc_change = ifelse(year == 1999, NA, perc_change)) %>%
  ggplot(aes(x = year, y = perc_change)) +
  geom_col() +
  drop_axis() +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(limits = c(-1, 1), labels = percent_format()) +
  labs(x = element_blank(), y = element_blank()) +
  facet_geo(~state) +
  theme(strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 10))

data %>%
  group_by(month, state) %>%
  summarise(sum = sum(totals)) %>%
  ggplot(aes(x = month, y = sum)) +
  # geom_line() +
  geom_area(alpha = .7) +
  drop_axis() +
  scale_x_date(breaks = c(as.Date("2000-01-01"), as.Date("2020-01-01")),
               labels = c("2000", "2020")) +
  scale_y_continuous(breaks = scales::extended_breaks(n = 2), labels = comma_format()) +
  facet_geo(~state, scales = 'free') +
  labs(x = element_blank(), y = element_blank()) +
  theme(strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 10))
#
#   data2020 <- data %>%
#     filter(month >= "2020-01-01")
#
# data2020 %>%
#     group_by(state, month) %>%
#     summarise(sum = sum(totals)) %>%
#   ggplot(aes(x = month, y = sum)) +
#     geom_line() +
#     geom_area(alpha = .7) +
#     # scale_x_continuous(breaks = c(2000, 2020)) +
#     # scale_y_continuous(limits = c(-1, 1), labels = percent_format()) +
#     labs(x = element_blank(), y = element_blank()) +
#     facet_geo(~state, scales = 'free') +
#     theme(strip.background = element_rect(fill = 'white'),
#           strip.text = element_text(size = 10))

## last year's first six months in red, this year's first six months in blue.
#
data %>%
  mutate(month = lubridate::month(month)) %>%
  filter(month < 7) %>%
  group_by(month, year) %>%
  summarise(sum = sum(totals)) %>%
  mutate(fill = ifelse(year == 2020, "2020", "")) %>%
  ggplot(aes(group = year)) +
  geom_line(aes(x = month, y = sum, color = fill, alpha = fill), show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 5000000), labels = unit_format(unit = "M", sep = "", scale = 1e-6, accuracy = 1)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Jan", "Feb", "Mar", "April", "May", "June")) +
  scale_color_manual(values = c("gray", "red")) +
  scale_alpha_manual(values = c(.7, 1)) +
  labs(x = element_blank(), y = element_blank())

# data %>%
#   mutate(month = lubridate::month(month)) %>%
#   filter(month < 7) %>%
#   group_by(month, year, state) %>%
#   summarise(sum = sum(totals)) %>%
#   mutate(fill = ifelse(year == 2020, "2020", "")) %>%
#   ggplot(aes(group = year)) +
#   geom_line(aes(x = month, y = sum, color = fill, alpha = fill), show.legend = FALSE) +
#   # scale_y_continuous(labels = unit_format(unit = "M", sep = "", scale = 1e-6, accuracy = 1)) +
#   # scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Jan", "Feb", "Mar", "April", "May", "June")) +
#   scale_color_manual(values = c("gray", "red")) +
#   scale_alpha_manual(values = c(.7, 1)) +
#   labs(x = element_blank(), y = element_blank()) +
#   facet_geo(~ state, scales = 'free') +
#   drop_axis() +
#   theme(axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank())
#
# library(usmap)
# populations <- statepop
#
# joined <- left_join(data, statepop, by = c('state' = 'full')) %>%
#   filter(!is.na(pop_2015))
#
# adjusted <- joined %>%
#   mutate(per_100k = ((totals/pop_2015)*100000))
#
# adjusted %>%
#   # filter(year > 2009) %>%
#   mutate(month = lubridate::month(month)) %>%
#   # filter(month < 7) %>%
#   filter(month < 7) %>%
#   group_by(month, year, state) %>%
#   summarise(sum = sum(per_100k)) %>%
#   mutate(fill = ifelse(year == 2020, "2020", "")) %>%
#   ggplot(aes(group = year)) +
#   geom_line(aes(x = month, y = sum, color = fill, alpha = fill, size = fill), show.legend = FALSE) +
#   scale_color_manual(values = c("gray", "red")) +
#   scale_alpha_manual(values = c(.7, 1)) +
#   scale_size_manual(values = c(1, 2)) +
#   labs(x = element_blank(), y = element_blank()) +
#   facet_geo(~ state, scales = 'free_y', grid = "us_state_grid1") +
#   drop_axis() +
#   theme(axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         strip.background = element_rect(fill = 'black'),
#         strip.text = element_text(size = 10, color = 'white'),
#         panel.background = element_rect(fill = 'black'),
#         plot.background = element_rect(fill = 'black'),
#         panel.grid = element_blank())

data %>%
  mutate(month = lubridate::month(month)) %>%
  filter(month < 7) %>%
  group_by(month, year, state) %>%
  summarise(sum = sum(totals)) %>%
  mutate(fill = ifelse(year == 2020, "2020", "")) %>%
  ggplot(aes(group = year)) +
  geom_line(aes(x = month, y = sum, color = fill, alpha = fill), size = .2, show.legend = FALSE) +
  scale_color_manual(values = c("gray", "red")) +
  scale_alpha_manual(values = c(.5, 1)) +
  facet_geo(~ state, scales = 'free_y', grid = "us_state_grid1") +
  drop_axis() +
  theme(text = element_text(color = "white",
                            family = "IBM Plex Sans"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(size = 10, color = 'white'),
        panel.background = element_rect(fill = 'black'),
        plot.background = element_rect(fill = 'black'),
        panel.grid = element_blank()) +
  labs(x = element_blank(),
       y = element_blank())
       # title = "Spikes in gun ownership during COVID-19",
       # subtitle = "How the first six months of 2020 compare to the last twenty years")

ggsave(here::here("outputs/map.svg"))

data %>%
  mutate(month = lubridate::month(month)) %>%
  filter(month < 7) %>%
  group_by(month, year) %>%
  summarise(sum = sum(totals)) %>%
  mutate(fill = ifelse(year == 2020, "2020", "")) %>%
  ggplot(aes(group = year, size = size)) +
  geom_line(aes(x = month, y = sum, color = fill, alpha = fill), size = .4, show.legend = FALSE) +
  scale_color_manual(values = c("gray", "red")) +
  scale_alpha_manual(values = c(0.35, 1)) +
  labs(x = element_blank(), y = element_blank()) +
  drop_axis() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid = element_blank())

ggsave(here::here("outputs/total.svg"))

data %>%
  mutate(month = lubridate::month(month)) %>%
  filter(month == 6,
         year != 2020) %>%
  group_by(year) %>%
  summarise(sum = sum(totals)) %>%
  summarise(mean(sum))


data %>%
  mutate(month = lubridate::month(month)) %>%
  group_by(year, month) %>%
  summarise(sum = sum(totals)) %>%
  arrange(desc(sum))
