message("Fuck you")

library(readr)
library(readxl)
library(dplyr)

csv_file <- "C:/Graduate-Studies/R/preprocessed_data.csv"

data <- read_csv(csv_file)

cat("CSV Data: \n")
head(data)

data <- data %>%
  filter(!is.na("1998 [YR1998]")) %>%
#   mutate(new = as.numeric()) %>%
  mutate(new_column = ifelse(is.na(as.numeric("1998 [YR1998]")), NA, as.numeric("1998 [YR1998]"))) %>%


head(data, n = 5)
