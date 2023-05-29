library(testthat)
library(dplyr)
library(ggplot2)
source("../R/waterColumnViz.R")


test_that(desc = "Test waterColumnViz created a ggplot object", code = {
  df <- read.csv("../Example/sim_waterData.csv")
  p <- waterColumnViz(df, "col1")
  expect_that( object = p, condition = expect_no_error(p))
})
