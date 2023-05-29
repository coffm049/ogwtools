library(tidyverse)


# simulate dataframe with unique observations for each combination of the following variables:
# - 5 rounds 
# - 10 sites 
# - 5 depths

data.frame(round = rep(1:5, each = 50),
           site = rep(1:10, each = 5, times = 5),
           depth = rep(1:5, times = 50)) %>%
  # add 5 new columns with random values
  mutate(
    col1 = sample(1:100, 250, replace = TRUE),
    col2 = sample(1:100, 250, replace = TRUE),
    col3 = sample(1:100, 250, replace = TRUE),
    col4 = sample(1:100, 250, replace = TRUE),
    col5 = sample(1:100, 250, replace = TRUE)
  ) %>%
    write.csv("simWaterData.csv", row.names= F)
