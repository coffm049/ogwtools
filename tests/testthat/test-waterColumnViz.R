library(testthat)
library(dplyr)
library(ggplot2)
source("../../R/waterColumnViz.R")

# path <- test_path("testdata", "simWaterData.csv")
path <- "testdata/simWaterData.csv"
df <- read.csv(path)

test_that(desc = "Test waterColumnViz created a ggplot object", code = {
  p <- waterColumnViz(df, "col1")
  expect_no_error(p)
})
