library(pdftools)
library(tidyverse)
library(janitor)
library(here)

test <- pdf_text(here('data/raw.pdf')) %>%
  strsplit(split = "\n") %>%
  as.data.frame() %>%
  slice(5:nrow(.)) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  # now each column has a unique index (its suffix number)
  rename(state_territory_jan_feb_mar_apr_may_jun_jul_aug_sep_oct_nov_dec_totals_1 = state_territory_jan_feb_mar_apr_may_jun_jul_aug_sep_oct_nov_dec_totals)

for (i in 1:ncol(test)) {
  # grab the year suffix
  index <- str_extract(colnames(test)[i], "([0-9].*)")
  # create the year, which goes from 1998 to 2020, meaning it 2021 - index (because index starts at 1)
  year <- 2021 - as.numeric(index)
  # recreate column names based on year
  colnames(test)[i] <- paste0('checks_', year)
}

# drop the bad rows
test <- test %>%
  slice(1:55) %>%
  mutate_all(as.character) %>%
  select(-checks_1998)

# for each column, split by the white space between numbers (have to include digit, otherwise will split on state names like South Dakota, West Virginia, etc.)
listed <- test[, 2] %>% str_replace_all(",", "") %>%
  # First, remove white space between each multi-word state (e.g. west virginia), so the eventual space split doesn't split them up
  str_replace(" ", "") %>%
  str_replace(" ", "") %>%
  str_split("\\s+") %>%
  as.data.frame() %>%
  row_to_names(row_number = 1)

rownames(listed) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Total")

transposed <- listed %>% t() %>% as.data.frame()
