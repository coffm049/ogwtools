#' Nutrient data workup 
#'
#' Preprocessing of nutrient data for visualization. Specifically, it removes blanks, drop nas,
#' and blanks, specifies datatypes, renames variables, and finds the mean for each site, date,
#' parameter combination. Requires columns, Type, Date, Site, Round, Year in any order
#' @param data 
#' @examples cleanNuts(data)
#' @importFrom magrittr %>%
#' @return tall clean dataframe for vizualization 
cleanNuts <- function(data) {
  data %>%
    dplyr::filter(Type!="Blank") %>%
    dplyr::select(-c(Type, Round, Year)) %>%
    dplyr::na_if("ND") %>%
    tidyr::pivot_longer(-c(Date, Site),names_to="Parameter") %>%
    # recast value as numeric column
    dplyr::mutate(value=as.numeric(value)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y")) %>%
    dplyr::mutate(Parameter=recode(Parameter,
                            "Chl.A..ug.L."="Chlorphyll A (µg/L)", "Nitrate.nitrite..mg.L."="Nitrate + nitrite (mg/L)",
                            "Ortho.P..mg.L."="Ortho-P (mg/L)","TN..mg.L."="TN (mg/L)", "TP..mg.L."="TP (mg/L)", "TSS..mg.L."="TSS (mg/L)" )) %>%
    dplyr::group_by(Date, Site, Parameter) %>%
    dplyr::summarize(value=mean(value, na.rm = T)) %>% 
    # drop unobserved factors in Site
    dplyr::mutate(Site = factor(Site))
}


#' nutsThroughTime
#'
#' Creates a ggplot object of nutrient data through time. Specifically, it creates a ggplot object 
#' @param dataframe with site, date, and parameters in other columns 
#' @examples nutsThroughTime(data)
#' @export
#' @examples nutsThroughTime(df)
#' @importFrom magrittr %>%
#' @return ggplot object
nutsThroughTime <- function(data) {
  cleanNuts(data) %>%
      dplyr::mutate(day1=ifelse(as.numeric(Site)<8,"West","East"),
             day1 = factor(day1, levels = c("West", "East"))) %>%
    ggplot2::ggplot(aes(x=Date, y=value)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, span = 1) +
    ggplot2::facet_grid(row= vars(Parameter), cols=vars(Site), scales = "free_y",labeller = label_wrap_gen(width=10)) +
    ggplot2::labs(title = "Nutrient Data", x='2022 Dates', y = 'Parameter Value', subtitle = 'Site')+
    ggplot2::theme(
      plot.title = element_text(hjust=0.5, size = 20, margin = margin(0,0,0,0, 'cm')),
      plot.subtitle = element_text(hjust=0.5, size=20),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.title.y = element_blank(),
      axis.line = element_line(size=1),
      strip.text.y = element_text(angle=0),
      panel.grid.major = element_line(size=1),
      panel.border = element_rect(size=1, fill=NA)
    )
}


