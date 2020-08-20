library(tidyverse)
library(waffle)
library(patchwork)

data <- readxl::read_excel('data/aug18.xlsx')

data <- data %>% 
  mutate(total = negative + positive)

faculty <- c(`Negative` = 275, `Positive` = 1)
faculty_plot <- waffle(
  faculty, rows = 3, size = .05, 
  colors = c("#969696", "darkred"), legend_pos = "none"
)

ggsave("outputs/faculty_plot.svg", faculty_plot, device = "svg")

staff <- c(`Negative` = 1191, `Positive` = 7)
staff_plot <- waffle(
  staff, rows = 12, size = .05, 
  colors = c("#969696", "darkred"), legend_pos = "none"
)

ggsave("outputs/staff_plot.svg", staff_plot, device = "svg")

undergraduates <- c(`Negative` = 2098, `Positive` = 9)
undergraduates_plot <- waffle(
  undergraduates, rows = 22, size = .05, 
  colors = c("#969696", "darkred"), legend_pos = "none"
)

ggsave("outputs/undergraduates_plot.svg", undergraduates_plot, device = "svg")

graduates <- c(`Negative` = 1020, `Positive` = 2)
graduates_plot <- waffle(
  graduates, rows = 11, size = .05, 
  colors = c("#969696", "darkred"), legend_pos = "none"
)

ggsave("outputs/graduates_plot.svg", graduates_plot, device = "svg")

faculty_plot / staff_plot / undergraduates_plot / graduates_plot

totals <- c(`Negative` = 4584, `Positive` = 19)
totals_plot <- waffle(
  totals, rows = 105, size = .05, 
  colors = c("#969696", "darkred"), legend_pos = "none"
)

ggsave("outputs/totals_plot.svg", totals_plot, device = "svg")
