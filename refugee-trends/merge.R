## take all individual year datasets and merge them
## data from this portal https://ireports.wrapsnet.org/Interactive-Reporting/EnumType/Report?ItemPath=/rpt_WebArrivalsReports/MX%20-%20Arrivals%20by%20Destination%20and%20Nationality
filenames <- list.files(path="./data/",pattern="*.csv")
fullpath=file.path("./data/",filenames)

dataset <- do.call("rbind", lapply(fullpath,FUN=function(files){ read.csv(files)}))

# the csv outputs include two dfs, one with sending country as the grouping var (nat_definition4)
# the other with receiving states as the grouping variable
# we only want the rows with states in nat_definition 4
states_and_countries <- unlist(unique(dataset$nat_definition4))
states <- states_and_countries[1:50]

dataset <- dataset %>% 
  filter(nat_definition4 %in% states)

# complete merged data
write.csv(dataset, "./data/final/data.csv")
