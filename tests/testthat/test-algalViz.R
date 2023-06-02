library(testthat)
library(dplyr)
library(ggplot2)
source("../../R/algalViz.R")

path <- test_path("testdata", "algae.csv")
# path <- "testdata/algae.csv"
df <- read.csv(path)


test_that(desc = "Test algalViz created a ggplot object", code = {
  p <- algalViz(df)
  expect_true("ggplot" %in% class(p))
})



