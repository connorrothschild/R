library(dplyr)
# install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)

parts <- c(550,12)
waffle(parts, rows = 16, size = 0.33)


parts <- c(`Non-Foreign-Language Films` = 552, `Foreign-Language Films` = 12)
nominee_plot <- waffle(
  parts, rows = 12, size = .67, 
  colors = c("#969696", "#1879bf"), legend_pos = "top"
)

nominee_plot

ggsave("outputs/nominee_plot.svg", plot, device = "svg")

parts <- c(`Non-Foreign-Language Films` = 91, `Foreign-Language Films` = 1)
winners_plot <- waffle(
  parts, rows = 4, size = .67, 
  colors = c("#969696", "#1879bf"), legend_pos = "top"
)

winners_plot

ggsave("outputs/winners_plot.svg", winners_plot, device = "svg")