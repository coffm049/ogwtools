#' waterColumnViz
#'
#' Vizualize water quality variables along the water column for multiple time
#' points and multiple site
#' @param df dataframe with round, date, site, depth, followed by parameter columns
#' @param variable string specifyning which variable to vizualize
#' @examples myfunction(X,Y)
#' @import ggplot2, dplyr, tidyr
#' @export
#' @return ggplot plot
waterColumnViz <- function(df, parameter, parameter_name_plot) {
  df %>%
    dplyr::na_if("") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    ggplot2::ggplot(aes_string(x = parameter, y = "Depth..m.", col = "Date", group= "Date")) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~Site) +
    # set legend title
    ggplot2::labs(col = "Date") + 
  ylab("Depth (m)") + 
  xlab(parameter_name_plot)
}
