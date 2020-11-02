#' DRGstatistics: Calculate statistics over all of the DRG codes for average Medicare payments
#'
#' @param df the input of some data frame. Here, we choose DRG.
#'
#' @param option the input of either 'mean', 'sd', or 'median'.
#'
#' @return Displays the mean, sd, or median corresponding to DRG codes
#'
#' @export
#'
#' @importFrom dplyr
#' @importFrom knitr kable
#'
#' @examples DRGstatistics(DRG, 'mean')
#'
DRGstatistics <-
  function(df, option) {
    treatedDRG <- df %>%
      group_by(DRG.Definition) %>%
      summarise(option = switch(
        option,
        mean = mean(Average.Medicare.Payments),
        median = median(Average.Medicare.Payments),
        sd = sd(Average.Medicare.Payments)
      ))

    names(treatedDRG) <- c("DRG", "Statistics")
    DRGstat <- data.frame(treatedDRG)
    kable(DRGstat, caption = "Medicare Payment Analysis")
  }

