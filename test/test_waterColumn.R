library(ggplot)
library(dplyr)

df <- read.csv("../Example/simWaterData.csv")


df %>%
  ggplot(aes(x = col1, y = depth, col = factor(round), group= round)) +
  geom_path() +
  facet_wrap(~site)

