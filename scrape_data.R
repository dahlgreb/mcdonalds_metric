library(rvest)
library(stringr)

# Read in html file
query <- paste("https://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants")
mds <- read_html(query)

# Read table of countries with mcdonalds
table1_data <- (mds %>%
  html_nodes("tbody tr") %>%
  html_nodes("td") %>%
  html_text())[1:819]

# Parse list into columns 
num_countries <- length(table1_data)
Name <- table1_data[seq(1, num_countries, 7)] %>%
  str_replace("\\n", "")
First_opened <- table1_data[seq(2, num_countries, 7)] %>%
  str_replace("\\n", "")
First_location <- table1_data[seq(3, num_countries, 7)] %>%
  str_replace("\\n", "")
Count <- table1_data[seq(4, num_countries, 7)] %>%
  str_replace("\\n", "")
Source <- table1_data[seq(5, num_countries, 7)] %>%
  str_replace("\\n", "")
People_per_md <- table1_data[seq(6, num_countries, 7)] %>%
  str_replace("\\n", "")
Notes <- table1_data[seq(7, num_countries, 7)] %>%
  str_replace("\\n", "")

# Clean columns
Name <- Name %>%
  str_replace("\\(.*", "") %>%
  str_trim()
Name <- Name %>%
  str_replace("Vietnam", "Viet Nam")
Count <- str_replace(Count, ",", "")
Count <- str_extract(Count, "\\d+")[1:117]
People_per_md <- str_replace(People_per_md, ",", "")

# Construct Dataframe
table1 <- data.frame(Name,First_opened, First_location, Count, Source, People_per_md, Notes)

write.csv(table1,"McDonalds.csv", row.names = TRUE)
