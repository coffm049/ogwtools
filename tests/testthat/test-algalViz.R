library(testthat)
library(dplyr)
library(ggplot2)
source("../../R/algalViz.R")



test_that(desc = "Test algalViz created a ggplot object", code = {
  path <- test_path("testdata", "algae.csv")
  df <- read.csv(path)
  p <- algalViz(df)
  expect_true("ggplot" %in% class(p))
})



