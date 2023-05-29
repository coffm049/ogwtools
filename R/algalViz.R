#' algalViz
#'
#' utility function to help in vizualizing algal distributions across sites 
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
  expand.grid(unique(df$Round), unique(df$Site), unique(df$Phyla)) %>%
    rename(Round = Var1 , Site = Var2, Phyla = Var3) %>%
    left_join(df, on = c("Round", "Site", "Phyla")) %>%
    # replace nas with 0
    mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
    group_by(Round, Site, Phyla) %>%
    summarize(Total = sum(Count)) %>%
    mutate(Percent = Total / sum(Total) * 100) %>%
    ggplot(aes(x= Round, y = Percent, fill = Phyla)) +
    geom_area(stat= "identity", position = "fill") +
    facet_wrap(~Site,  labeller = label_both)

}


