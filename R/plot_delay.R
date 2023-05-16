#' Create plots for the delay multipliers
#'
#' Creates plots for the delay multipliers and their uncertainty intervals based
#' on their estimated standard errors. This is a basic
#' \code{\link[ggplot2]{ggplot}} with a few customizable parameters.
#'
#' @param multipliers A list returned from \code{\link{multipliers}},
#' in which \code{type} is a positive integer to return delay multipliers.
#' @param facets_ncol If a positive integer, it indicates the number of the
#' columns in the facet. If FALSE, each plot is created separately. The default
#' is 2.
#' @param interval If FALSE (default), no uncertainty intervals are drawn. If a
#' positive integer, the intervals are this number times the standard error. If
#' a number between 0 and 1 (e.g. 0.95), the equivalent confidence interval is
#' drawn (e.g. 95% CI). In case of the confidence intervals, they are based on
#' the Gaussian distribution.
#' @param interval_color The color of the uncertainty intervals. Default is
#' "blue".
#' @param show.legend A logical indicating whether the interval legend is shown.
#' Default is FALSE.
#' @param xlab,ylab Names displayed at the x and y axes respectively. Default is
#' "Period" and "Delay" respectively.
#' @param ... Currently unused argument.
#'
#' @return \code{plot_delay} returns a number of \code{\link[ggplot2]{ggplot}}
#' objects.
#'
#' @seealso \code{\link{multipliers}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords iplots
#' @export
#' @examples
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' delay_mult <- multipliers(ardl_3132, type = 12, se = TRUE)
#'
#' ## Simply plot the delay multipliers -----------------------------------
#'
#' plot_delay(delay_mult)
#'
#' ## Rearrange them ------------------------------------------------------
#'
#' plot_delay(delay_mult, facets_ncol = 1)
#'
#' ## Add 1 standard deviation uncertainty intervals ----------------------
#'
#' plot_delay(delay_mult, interval = 1)
#'
#' ## Add 95% confidence intervals, change color and add legend -----------
#'
#' plot_delay(delay_mult, interval = 0.95, interval_color = "darkgrey",
#'            show.legend = TRUE)

plot_delay <- function(multipliers, facets_ncol = 2,
                       interval = FALSE, interval_color = "blue", show.legend = FALSE,
                       xlab = "Period", ylab = "Delay", ...) {
    plot_data <- function(mult, .data) {
        plot <- ggplot2::ggplot(mult, ggplot2::aes(x = .data[["Period"]], y = .data[["Delay"]])) +
            ggplot2::geom_line() +
            ggplot2::labs(x = xlab, y = ylab) +
            ggplot2::theme_minimal()
        if (!isFALSE(interval)) {
            adjustment <- ifelse((interval %% 1) == 0, interval, stats::qnorm(1-(1-interval)/2))
            plot <- plot +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["Delay"]] - adjustment*.data[["Std. Error Delay"]],
                                                  ymax = .data[["Delay"]] + adjustment*.data[["Std. Error Delay"]],
                                                  fill = "Std. Error Delay"),
                                     alpha = 0.3, show.legend = show.legend) +
                ggplot2::scale_fill_manual(name = "", values = interval_color,
                                           labels = ifelse((interval %% 1) == 0, paste0(interval, " SE"),
                                                           paste0(100*interval, "%", " CI"))) +
                ggplot2::theme(legend.position = "bottom")
        }
        plot
    }

    plots <- lapply(names(multipliers), function(name) {
        plot_data(multipliers[[name]]) +
            ggplot2::labs(title = name) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    })

    if (!isFALSE(facets_ncol)) {
        gridExtra::grid.arrange(grobs = plots, ncol = facets_ncol)
    } else {
        plots
    }
}
