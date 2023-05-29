library(testthat)
library(dplyr)
library(ggplot2)
source("../../R/waterColumnViz.R")


test_that(desc = "Test waterColumnViz created a ggplot object", code = {
  df <- read.csv("../data/sim_waterData.csv")
  p <- waterColumnViz(df, "col1")
  expect_no_error(p)
})
