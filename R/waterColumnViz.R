#' waterColumnViz
#' 
#' @description
#' Vizualize water quality variables along the water column for multiple time
#' points and multiple site
#'
#' @details
#' A function that helps water quality data vizualization for a single water quality
#' parameter measured over multiple sites and depths and days
#'
#' @param df dataframe with round, date, site, depth, followed by parameter columns
#' @param variable string specifyning which variable to vizualize
#' @examples myfunction(X,Y)
#' @export
#' @return ggplot plot
waterColumnViz <- function(df, parameter, xlabel) {
  df %>%
    dplyr::na_if("") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    ggplot2::ggplot(aes_string(x = parameter, y = "Depth..m.", col = "Date", group= "Date")) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~Site) +
    ggplot2::labs(col = "Date") +
    ggplot2::ylab("Depth (m)") +
    ggplot2::xlab(xlabel)
}
