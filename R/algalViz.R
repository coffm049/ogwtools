#' algalViz
#'
#' # utility function to help in vizualizing algal distributions across sites 
#' @param data A dataframe
#' @param proportion boolean, wether to report proportions or total counts
#' @param count_column string, column name of count data
#' @examples algalViz(data= df, standardize = T, count_value = "Count")
#' @import dplyr
#' @import ggplot2
#' @export
algalViz <- function(data, proportion = T, count_column = "Count", facet_var= NULL) {
  # check types
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
    }
  if (!is.logical(proportion)) {
    stop("proportion must be True or False")
  }
  if (!is.character(count_column)) {
    stop("count_value must be a string")
  }

  # Check count_column is in colnames
  if (!(count_column %in% colnames(data))) {
    stop("count_column must be a column name in data")
  }

  data %>%
    mutate(Round = as.numeric(Round),
    #value = as.numeric({{count_column}})
    ) %>%
    ggplot(aes(x  = Round, y = Count, fill = Phyla)) +
      geom_bar(stat = "identity", position = "fill") +
      facet_wrap(~Site, ncol = 1)
}
