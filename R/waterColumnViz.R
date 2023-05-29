#' waterColumnViz
#'
#' Vizualize water quality variables along the water column for multiple time
#' points and multiple site
#' @param df dataframe with sites, rounds, depth, and water quality variables
#' of interes in long format
#' @param variable string specifyning which variable to vizualize
#' @examples myfunction(X,Y)
#' @import ggplot2
#' @export
waterColumnViz <- function(df, variable) {
  # ggplot where the x aesthetic is the variable of interest
  ggplot(data = df, aes(x = get(variable), y = depth, col = factor(round), group= round)) +
    geom_path() +
    facet_wrap(~site) +
    xlab("") +
    # set legend title
    labs(col = "Round")
}
