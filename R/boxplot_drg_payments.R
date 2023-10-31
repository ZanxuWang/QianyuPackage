library(tidyverse)
library(ggplot2)

# Function to plot boxplot of payments per DRG code
boxplot_drg_payments <- function(df, payment_type){
  # make function only accept 3 options for payment type
  payment_types <- c("Average Medicare Payments",
                     "Average Total Payments",
                     "Average Covered Charges")
  # if payment type is one of the 3 options
  if (payment_type %in% payment_types){
    # plot payment type per DRG
    ggplot(df, aes(x = `DRG Definition`,
                   y = .data[[payment_type]])) +
      # make boxplot
      geom_boxplot() +
      # shorten x axis labels to only DRG code
      scale_x_discrete(labels = unique(substr(df$`DRG Definition`, 1, 3))) +
      # change to classic theme
      theme_classic() +
      # rotate x axis labels 90 degrees
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      # add title to plot
      ggtitle(paste("Plot of", payment_type, "by DRG code"))
  }
  # give warning if not one of the 3 payment types
  else stop("Wrong payment type")
}

## Function 2: Write a function that calculates statistics over all of the DRG codes
## for average Medicare payments. Make it an option in your function to calculate
## either the mean, median, or standard deviation of the DRG codes.

#' Title
#'
#' @param data dd
#' @param statistic dd
#'
#' @return xx
#' @export xxdo
#'
#' @examples xx
calculate_drg_stats <- function(data, statistic = "mean") {
  # Check if the input statistic is valid
  if (!(statistic %in% c("mean", "median", "sd"))) {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }
  # Define the calculations for each statistic
  calculations <- list(
    mean = ~mean(.data$Average.Medicare.Payments, na.rm = TRUE),
    median = ~median(.data$Average.Medicare.Payments, na.rm = TRUE),
    sd = ~sd(.data$Average.Medicare.Payments, na.rm = TRUE)
  )
  # Calculate the specified statistic for Average Medicare Payments
  data %>%
    summarise(result = calculations[[statistic]]) %>%
    pull(result)
}

