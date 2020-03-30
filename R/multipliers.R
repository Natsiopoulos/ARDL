#' Multipliers estimation
#'
#' \code{multipliers} is a generic function used to estimate short-run (impact),
#' interim and long-run (total) multipliers, along with their corresponding
#' standard errors, t-statistics and p-values.
#'
#' The function invokes two different \code{\link[utils]{methods}}, one for
#' objects of \code{\link[base]{class}} 'ardl' and one for objects of
#' \code{class} 'uecm'. This is because of the different (but equivalent)
#' transformation functions that are used for each class/model ('ardl' and
#' 'uecm') to estiamte the multipliers.
#'
#' Currently only the long-run (total) multipliers are suported (\code{type =
#' "lr"}). Other choices, including short-run (impact) and interim multipliers
#' are going to be available in future versions.
#'
#' The delta method is used for approximating the standard errors (and thus the
#' t-statistics and p-values) of the estimated multipliers.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl' or 'uecm'.
#' @param type A character string describing the type of multipliers. The
#'   default is "lr" for long-run (total) multipliers (see 'Details').
#' @param vcov_matrix The estimated covariance matrix of the random variable
#'   that the transformation function uses to estimate the standard errors (and
#'   so the t-statistics and p-values) of the multipliers. The default is
#'   \code{vcov(object)} (when \code{vcov_matrix = NULL}), but other estimations
#'   of the covariance matrix of the regression's estimated coefficients can
#'   also be used (e.g., using \code{\link[sandwich]{vcovHC}} or
#'   \code{\link[sandwich]{vcovHAC}}).
#'
#' @return \code{multipliers} returns a data.frame containing the independent
#'   variables (including possibly existing intercept or trend and excluding the
#'   fixed variables) and their corresponding standard errors, t-statistics and
#'   p-values.
#'
#' @seealso \code{\link{ardl}}, \code{\link{uecm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords math
#' @export
#' @examples
#' data(denmark)
#' 
#' ## Estimate the long-run multipliers of an ARDL(3,1,3,2) model ---------
#'
#' # From an ARDL model
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' mult_ardl <- multipliers(ardl_3132)
#' mult_ardl
#'
#' # From an UECM
#' uecm_3132 <- uecm(ardl_3132)
#' mult_uecm <- multipliers(uecm_3132)
#' mult_uecm
#' 
#' all.equal(mult_ardl, mult_uecm)

multipliers <- function(object, type = "lr", vcov_matrix = NULL) {
    UseMethod("multipliers")
}

#' @rdname multipliers
#' @export
#'

multipliers.ardl <- function(object, type = "lr", vcov_matrix = NULL) {

    # no visible binding for global variable NOTE solution
    group_id <- coeff <- sums <- NULL; rm(group_id, coeff, sums)

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    kw <- object$parsed_formula$kw
    objcoef <- object$coefficients
    from <- kw + 1
    to <- kw + object$order[1]
    # create table without the y in levels and fixed
    x_table <- dplyr::tibble(name = names(objcoef), coeff = objcoef) %>%
        dplyr::slice(-(from:to)) %>%
        dplyr::slice(1:(dplyr::n() - object$parsed_formula$kfixed))
    # create table only with y in levels
    y_table <- dplyr::tibble(objcoef) %>% dplyr::slice(from:to)
    # create groups to sum by group
    x_table <- x_table %>%
        dplyr::mutate(group_id = if (kw != 0) {
            c(1:kw, rep(from:(from + object$parsed_formula$kx - 1), object$order[-1] + 1))
        } else {
            c(rep(from:(from + object$parsed_formula$kx - 1), object$order[-1] + 1))
        })
    # create the sums of levels of x and trends
    temp <- x_table %>%
        dplyr::group_by(group_id) %>%
        dplyr::summarise(sums = sum(coeff)) %>%
        dplyr::select(sums)

    # calculate long-run multipliers
    lr <- (temp / (1 - sum(y_table)))[ ,1]

    if (kw != 0) {
        names(lr) <- c(names(objcoef)[1:kw],
            object$parsed_formula$x_part$var)
    } else {
        names(lr) <- object$parsed_formula$x_part$var
    }
    rm(temp)

    lr_se <- delta_method(object, vcov_matrix = vcov_matrix)
    multipliers <- data.frame(estimate = lr, std.error = lr_se, t.statistic = lr/lr_se,
        p.value = 2 * stats::pt(-abs(lr/lr_se), df = stats::df.residual(object))) # df = n - # of estimated coefficients
    multipliers <- data.frame(term = rownames(multipliers), multipliers)
    rownames(multipliers) <- 1:nrow(multipliers)

    return(multipliers)
}

#' @rdname multipliers
#' @export
#'

multipliers.uecm <- function(object, type = "lr", vcov_matrix = NULL) {

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    kw <- object$parsed_formula$kw
    objcoef <- object$coefficients
    if (kw != 0) {
        lr <- c(
            -objcoef[1:kw] / objcoef[kw + 1],
            -objcoef[(kw + 2):(kw + object$parsed_formula$kx + 1)] / objcoef[kw + 1]
        )
    } else {
        lr <- -objcoef[2:(object$parsed_formula$kx + 1)] / objcoef[1]
    }

    if (kw != 0) {
        names(lr) <- c(names(objcoef)[1:kw],
            object$parsed_formula$x_part$var)
    } else {
        names(lr) <- object$parsed_formula$x_part$var
    }

    lr_se <- delta_method(object, vcov_matrix = vcov_matrix)
    multipliers <- data.frame(estimate = lr, std.error = lr_se, t.statistic = lr/lr_se,
        p.value = 2 * stats::pt(-abs(lr/lr_se), df = stats::df.residual(object))) # df = n - # of estimated coefficients
    multipliers <- data.frame(term = rownames(multipliers), multipliers)
    rownames(multipliers) <- 1:nrow(multipliers)

    return(multipliers)
}
