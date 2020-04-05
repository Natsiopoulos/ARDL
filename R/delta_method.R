#' Delta method
#'
#' An internal generic function, customized for approximating the standard
#' errors of the estimated multipliers.
#'
#' The function invokes two different \code{\link[utils]{methods}}, one for
#' objects of \code{\link[base]{class}} 'ardl' and one for objects of
#' \code{class} 'uecm'. This is because of the different (but equivalent)
#' transformation functions that are used for each class/model ('ardl' and
#' 'uecm') to estimate the multipliers.
#'
#' @inheritParams multipliers
#'
#' @return \code{delta_method} returns a numeric vector of the same length as
#'   the number of the independent variables (excluding the fixed ones) in the
#'   model.
#'
#' @seealso \code{\link{multipliers}}, \code{\link{ardl}}, \code{\link{uecm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

delta_method <- function(object, vcov_matrix = NULL) {
    UseMethod("delta_method")
}

#' @rdname delta_method
#'

delta_method.ardl <- function(object, vcov_matrix = NULL) {

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    estmean <- stats::coef(object)
    estvar <- vcov_matrix
    w_part <- object$parsed_formula$w_part$var
    kx <- object$parsed_formula$kx

    p <- object$order[1]
    q <- object$order[-1]
    m <- 1 : (sum(object$order) + length(object$order) - 1)
    # case 2, 3
    # keep first this case to avoid w_part[1] in case when length(w_part)==0
    if ((length(w_part) == 0) | (length(w_part) == 2) ) { # 2nd statement for the case with only trend
        place_y <- 2:(p+1)
        m <- c(m, m[length(m)] + 1)
        bins_w <- 1
    # case 1
    } else if ((w_part[1] == "- 1") & length(w_part) == 1) {
        place_y <- 1:p
        bins_w <- c()
    # case 4, 5
    } else if (length(w_part) == 1) {
        place_y <- 3:(p+2)
        m <- 1:(m[length(m)] + 2)
        bins_w <- c(1, 1)
    }

    place_x <- m[!(m %in% place_y)]
    bin_breaks <- c(0, cumsum(c(bins_w, q + 1)))
    pi_yx.x <- lapply(1:(length(bin_breaks) - 1), function(i) {
        paste0("~ (",
            paste0("x", place_x[(bin_breaks[i] + 1) : (bin_breaks[i + 1])], collapse = " + "),
            ")")
        })
    pi_yy <- paste0(" / (1 - (",
        paste0("x", place_y, collapse = " + "),
        "))")
    restrictions <- paste0(pi_yx.x, pi_yy)

    lr_se <- sapply(1:length(restrictions), function(i) {
        msm::deltamethod(stats::formula(restrictions[i]), estmean, estvar)
        })
    return(lr_se)
}

#' @rdname delta_method
#'

delta_method.uecm <- function(object, vcov_matrix = NULL) {
    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    estmean <- stats::coef(object)
    estvar <- vcov_matrix
    w_part <- object$parsed_formula$w_part$var
    kx <- object$parsed_formula$kx

    # case 2, 3
    # keep first this case to avoid w_part[1] in case when length(w_part)==0
    if ((length(w_part) == 0) | (length(w_part) == 2) ) { # 2nd statement for the case with only trend
        restrictions <- paste0("~ x", c(1, 3:(kx + 2)), " / x2")
    # case 1
    } else if ((w_part[1] == "- 1") & length(w_part) == 1) {
        restrictions <- paste0("~ x", c(2:(kx + 1)), " / x1")
    # case 4, 5
    } else if (length(w_part) == 1) {
        restrictions <- paste0("~ x", c(1, 2, 4:(kx + 3)), " / x3")
    }

    lr_se <- sapply(1:length(restrictions), function(i) {
        msm::deltamethod(stats::formula(restrictions[i]), estmean, estvar)
        })
    return(lr_se)
}
