library(testthat)
library(dplyr)
library(ggplot2)
source("../R/algalViz.R")


test_that(desc = "Test algalViz created a ggplot object", code = {
  df <- read.csv("../Example/algae.csv")
  p <- algalViz(df)
  expect_that( object = p, condition = expect_no_error(p))
})
