#' DRGboxplot: Display a boxplot of payments by DRG code
#'
#' @param df some data frame. Here, we choose DRG.
#'
#' @param vary the input for some payment. Here, we pick one from the average
#' Medicare payments, the average total payment, and the average covered charges.
#'
#' @return The boxplot of payments by DRG code
#'
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom dplyr
#'
#' @examples DRGboxplot(DRG, DRG$Average.Total.Payments)
#'
DRGboxplot <- function(df, vary) {
  treatedDRG <- df %>%
    mutate(DRG_num = substr(DRG.Definition, start = 0, stop = 3))
  ggplot(df, aes(x = DRG.Definition, y = vary)) +
    geom_boxplot(fill = 'lightblue',
                 color = "dodgerblue",
                 position = position_dodge(width = 1)) +
    theme(plot.title = element_text(
      face = "bold",
      # set the title to be bold
      hjust = 0.5,

      # set the title to be in the center
      vjust = 0
    )) +
    # give the plot title
    ggtitle("Boxplot of payments by DRG code") +
    xlab('DRG code') + # give the x-axis label
    ylab('Average payments/charges ($)') +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )) +
    scale_x_discrete(labels = treatedDRG$DRG_num) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 13),
      plot.title = element_text(size = 16)
    )

}
