#' Create plot for the long-run (cointegrating) equation
#'
#' Creates a plot for the long-run relationship in comparison with the dependent
#' variable, and the fitted values of the model. This is a basic
#' \code{\link[ggplot2]{ggplot}} with a few customizable parameters.
#'
#' @param object An object of \code{\link[base]{class}} `ardl`.
#' @param coint_eq The objected returned from \code{\link{coint_eq}}.
#' @param facets A logical indicating whether the long-run relationship appears
#' in a separate plot. Default is FALSE.
#' @param show_fitted A logical indicating whether the fitted values are shown.
#' Default is FALSE.
#' @param show.legend A logical indicating whether the legend is shown.
#' Default is FALSE.
#' @param xlab Name displayed at the x axis. Default is "Time".
#' @param ... Currently unused argument.
#'
#' @return \code{plot_lr} returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link{coint_eq}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords iplots
#' @export
#'
#' @examples
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' ce2 <- coint_eq(ardl_3132, case = 2)
#'
#' plot_lr(ardl_3132, coint_eq = ce2)
#'
#' ## Compare fitted values and place long-run relationship separately ----
#'
#' ce3 <- coint_eq(ardl_3132, case = 3)
#' plot_lr(ardl_3132, coint_eq = ce3, facets = TRUE, show_fitted = TRUE,
#'         show.legend = TRUE)
#'

plot_lr <- function(object, coint_eq, facets = FALSE, show_fitted = FALSE,
                    show.legend = FALSE, xlab = "Time", ...) {
    plot_data <- zoo::cbind.zoo(object$data[,1], coint_eq, object$fitted.values)
    lr_name <- "long-run"
    fit_name <- "fitted"
    names(plot_data) <- c(object$parsed_formula$y_part$var, lr_name, fit_name)

    plot_function <- function(object, coint_eq, facets, show_fitted, show.legend, xlab, .data) {
        facet1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = zoo::index(plot_data))) +
            ggplot2::geom_line(ggplot2::aes(y = .data[[object$parsed_formula$y_part$var]],
                                            color = object$parsed_formula$y_part$var), show.legend = show.legend) +
            ggplot2::labs(x = xlab, y = object$parsed_formula$y_part$var, color = "") +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = "bottom")

        if (!facets) {
            facet1 <- facet1 +
                ggplot2::geom_line(ggplot2::aes(y = .data[[lr_name]], color = lr_name), show.legend = show.legend)
            if (show_fitted) {
                facet1 <- facet1 +
                    ggplot2::geom_line(ggplot2::aes(y = .data[[fit_name]], color = fit_name),
                              na.rm = TRUE, show.legend = show.legend) +
                    ggplot2::scale_color_manual(guide = "legend",
                                       values = c("black", "red", "blue"),
                                       breaks = c(object$parsed_formula$y_part$var, lr_name, fit_name),
                                       labels = c(object$parsed_formula$y_part$var, lr_name, fit_name))
            } else {
                facet1 <- facet1 +
                    ggplot2::scale_color_manual(guide = "legend",
                                       values = c("black", "red"),
                                       breaks = c(object$parsed_formula$y_part$var, lr_name),
                                       labels = c(object$parsed_formula$y_part$var, lr_name))
            }
        } else {
            if (show_fitted) {
                facet1 <- facet1 +
                    ggplot2::geom_line(ggplot2::aes(y = .data[[fit_name]], color = fit_name),
                              na.rm = TRUE, show.legend = show.legend) +
                    ggplot2::scale_color_manual(guide = "legend",
                                       labels = c(object$parsed_formula$y_part$var, fit_name),
                                       breaks = c(object$parsed_formula$y_part$var, fit_name),
                                       values = c("black", "blue"))
            } else {
                facet1 <- facet1 +
                    ggplot2::scale_color_manual(guide = "legend",
                                       values = "black",
                                       breaks = object$parsed_formula$y_part$var,
                                       labels = object$parsed_formula$y_part$var)
            }
        }

        if (facets) {
            facet2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = zoo::index(plot_data))) +
                ggplot2::geom_line(ggplot2::aes(y = .data[[lr_name]], color = lr_name), show.legend = show.legend) +
                ggplot2::labs(x = xlab, y = object$parsed_formula$y_part$var, color = "") +
                ggplot2::scale_color_manual(values = "red", labels = lr_name) +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "bottom")

            gridExtra::grid.arrange(facet1, facet2, ncol = 1)
        } else {
            facet1
        }
    }

    plot_function(object = object, coint_eq = coint_eq, facets = facets,
                  show_fitted = show_fitted, show.legend = show.legend, xlab = xlab)
}
