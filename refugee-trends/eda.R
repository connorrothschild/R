library(tidyverse)

data <- readr::read_csv("./data/final/data.csv")

table(data$region_name_3)

grouped_data <- data %>% 
  mutate(region_name_3 = parse_number(region_name_3)) %>% 
  group_by(nat_definition4, region_name_3) %>% 
  slice(1) %>% 
  filter(!is.na(region_name_3))
  
grouped_data %>% 
  group_by(region_name_3) %>% 
  summarise(textbox37 = sum(textbox37)) %>% 
  ggplot(aes(x = region_name_3, y = textbox37)) +
  geom_line()

## output into the non-R folder
write.csv(grouped_data, "../data/data.csv")
