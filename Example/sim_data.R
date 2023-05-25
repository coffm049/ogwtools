library(dplyr)

data.frame(
  "Round" = rep(1:5, each = 40),
  "Site" = rep(1:10, 20),
  "Phyla" = rep(rep(c("A","B","C","D","E"), each = 10), 4),
  "Count" = rpois(200, 10)) %>%
    write.csv("sim_data.csv", row.names= F)
