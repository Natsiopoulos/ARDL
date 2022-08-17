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
#' 'uecm') to estimate the multipliers.
#'
#' Note that \code{type = 0} is equivalent to \code{type = "sr"}. Also,
#' \code{type = s} will produce the same estimates as \code{type = "lr"} for
#' those particular variable for which s >= from their ARDL order.
#'
#' The delta method is used for approximating the standard errors (and thus the
#' t-statistics and p-values) of the estimated multipliers.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl' or 'uecm'.
#' @param type A character string describing the type of multipliers. Use "lr"
#' for long-run (total) multipliers (default), "sr" or 0 for short-run (impact)
#' multipliers or an integer between 1 and 100 for interim multipliers.
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
#' @section Mathematical Formula: \strong{Constant and Linear Trend:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\mu = \frac{c_{0}}{1-\sum_{i=1}^{p}b_{y,i}}}
#' \deqn{\delta = \frac{c_{1}}{1-\sum_{i=1}^{p}b_{y,i}}}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\mu = \frac{c_{0}}{-\pi_{y}}}
#' \deqn{\delta = \frac{c_{1}}{-\pi_{y}}}
#'
#' \strong{Short-Run Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\frac{\partial y_{t}}{\partial x_{j,t}} = \frac{b_{j,0}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\frac{\partial y_{t}}{\partial x_{j,t}} = \frac{\omega_{j}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k}
#'
#' \strong{Interim Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\frac{\partial y_{t+s}}{\partial x_{j,t}} = \frac{\sum_{l=1}^{s}b_{j,l}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k \;\;\;\;\; s \in \{0,\dots,q_{j}\}}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\frac{\partial y_{t+s}}{\partial x_{j,t}} = \frac{\pi_{j} + \psi_{j,s}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k \;\;\;\;\; s \in \{1,\dots,q_{j}-1\}}
#'
#' \strong{Long-Run Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\sum_{l=0}^{q_{j}}b_{j,l}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\pi_{j}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k}
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
#'
#'
#' ## Estimate the short-run multipliers of an ARDL(3,1,3,2) model --------
#'
#' mult_sr <- multipliers(uecm_3132, type = "sr")
#' mult_0 <- multipliers(uecm_3132, type = 0)
#' all.equal(mult_sr, mult_0)
#'
#'
#' ## Estimate the interim multipliers of an ARDL(3,1,3,2) model ----------
#'
#' # Note that the estimated interim multipliers match the long-run multipliers
#' # for those variables that their ARDL order equals or exceeds the interim step
#' mult_lr <- multipliers(uecm_3132, type = "lr")
#' mult_1 <- multipliers(uecm_3132, type = 1)
#' mult_2 <- multipliers(uecm_3132, type = 2)
#'
#' uecm_3132$order
#' mult_lr
#' mult_1
#' mult_2

multipliers <- function(object, type = "lr", vcov_matrix = NULL) {
    UseMethod("multipliers")
}

#' @rdname multipliers
#' @export
#'

multipliers.ardl <- function(object, type = "lr", vcov_matrix = NULL) {

    # no visible binding for global variable NOTE solution
    group_id <- coeff <- sums <- NULL; rm(group_id, coeff, sums)

    if (!(type %in% c("lr", "sr", 0:100))) {
        stop("'type' should be one of 'lr', 'sr' or a number between 0 and 100", call. = FALSE)
    }

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    kw <- object$parsed_formula$kw
    objcoef <- object$coefficients
    orders_x <- object$order[-1]
    from <- kw + 1
    to <- kw + object$order[1]
    if (type == "lr") {
        # create table without the y in levels and fixed
        x_table <- dplyr::tibble(name = names(objcoef), coeff = objcoef) %>%
            dplyr::slice(-(from:to)) %>%
            dplyr::slice(1:(dplyr::n() - object$parsed_formula$kfixed))
        # create table only with y in levels
        y_table <- dplyr::tibble(objcoef) %>% dplyr::slice(from:to)
        # create groups to sum by group
        x_table <- x_table %>%
            dplyr::mutate(group_id = if (kw != 0) {
                c(1:kw, rep(from:(from + object$parsed_formula$kx - 1), orders_x + 1))
            } else {
                c(rep(from:(from + object$parsed_formula$kx - 1), orders_x + 1))
            })
    } else if ((type == "sr") | (type == 0)) {
        b0 <- 1
        for (i in 1:(length(orders_x) -1)) {
            b0 <- c(b0, b0[length(b0)] + orders_x[i] +1)
        }
        b0 <- if (kw!=0) c(1:kw, b0+kw) else b0
        # create table without the y in levels and fixed
        x_table <- dplyr::tibble(name = names(objcoef), coeff = objcoef) %>%
            dplyr::slice(-(from:to)) %>% dplyr::slice(b0)
        # create table only with y in levels
        y_table <- dplyr::tibble(objcoef) %>% dplyr::slice(from:to)
        x_table <- x_table %>% dplyr::mutate(group_id = 1:dplyr::n())
    } else { # anything except 0:100 would have been stoped in the earlier check
        interim = type
        b0 <- 1:(1 + ifelse(orders_x[1] >= interim, interim, orders_x[1]))
        for (i in 1:(length(orders_x) -1)) {
            b0 <- c(b0, (sum(orders_x[1:i]) + length(orders_x[1:i])+1) : ((sum(orders_x[1:i]) +
                length(orders_x[1:i])+1) +
                ifelse(orders_x[i+1] >= interim, interim, orders_x[i+1])))
        }
        b0 <- if (kw!=0) c(1:kw, b0+kw) else b0
        # create table without the y in levels and fixed
        x_table <- dplyr::tibble(name = names(objcoef), coeff = objcoef) %>%
            dplyr::slice(-(from:to)) %>% dplyr::slice(b0)
        # create table only with y in levels
        y_table <- dplyr::tibble(objcoef) %>% dplyr::slice(from:to)
        # create groups to sum by group
        rep_pattern <- ifelse(orders_x[1:object$parsed_formula$kx] >= interim,
            interim, orders_x[1:object$parsed_formula$kx])+1
        x_table <- x_table %>%
            dplyr::mutate(group_id = if (kw != 0) {
                c(1:kw, rep(from:(from + object$parsed_formula$kx - 1), rep_pattern))
            } else {
                c(rep(from:(from + object$parsed_formula$kx - 1), rep_pattern))
            })
    }
    # create the sums of levels of x and trends
    temp <- x_table %>%
        dplyr::group_by(group_id) %>%

        dplyr::summarise(sums = sum(coeff)) %>%
        dplyr::select(sums)

    # calculate coefficients of multipliers
    multipliers_coef <- (temp / (1 - sum(y_table)))[ ,1]

    if (kw != 0) {
        names(multipliers_coef) <- c(names(objcoef)[1:kw],
            object$parsed_formula$x_part$var)
    } else {
        names(multipliers_coef) <- object$parsed_formula$x_part$var
    }
    rm(temp)

    multipliers_se <- delta_method(object, vcov_matrix = vcov_matrix)
    multipliers <- data.frame(estimate = multipliers_coef, std.error = multipliers_se, t.statistic = multipliers_coef/multipliers_se,
        p.value = 2 * stats::pt(-abs(multipliers_coef/multipliers_se), df = stats::df.residual(object))) # df = n - # of estimated coefficients
    multipliers <- data.frame(term = rownames(multipliers), multipliers)
    rownames(multipliers) <- 1:nrow(multipliers)

    return(multipliers)
}

#' @rdname multipliers
#' @export
#'

multipliers.uecm <- function(object, type = "lr", vcov_matrix = NULL) {

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)

    if (!(type %in% c("lr", "sr", 0:100))) {
        stop("'type' should be one of 'lr', 'sr' or a number between 0 and 100", call. = FALSE)
    }

    kw <- object$parsed_formula$kw
    objcoef <- object$coefficients
    orders_x <- object$order[-1]
    objnames_ws <- gsub(" ", "", names(objcoef), fixed = TRUE)
    objxvars <- object$parsed_formula$x_part$var
    if (type == "lr") {
        if (kw != 0) {
            multipliers_coef <- c(
                -objcoef[1:kw] / objcoef[kw + 1],
                -objcoef[(kw + 2):(kw + object$parsed_formula$kx + 1)] / objcoef[kw + 1]
            )
        } else {
            multipliers_coef <- -objcoef[2:(object$parsed_formula$kx + 1)] / objcoef[1]
        }
    } else if ((type == "sr") | (type == 0)) {
        # use gsub because sometimes the spacing goes weird
        Xt_1 <- c()
        Xt_1[orders_x != 0] <- gsub(" ", "", paste0("d(", objxvars, ")"),
            fixed = TRUE)[orders_x != 0]
        Xt_1[orders_x == 0] <- gsub(" ", "", objxvars[orders_x == 0], fixed = TRUE)
        if (kw != 0) {
            multipliers_coef <- c(
                -objcoef[1:kw] / objcoef[kw + 1],
                -objcoef[objnames_ws %in% Xt_1] / objcoef[kw + 1]
            )
        } else {
            multipliers_coef <- -objcoef[objnames_ws %in% Xt_1] / objcoef[1]
        }
    } else { # anything except 0:100 would have been stoped in the earlier check
        interim = type
        Xt_1 <- c()
        Xt_1[orders_x != 0] <- gsub(" ", "", paste0("L(", objxvars, ", 1)"),
            fixed = TRUE)[orders_x != 0]
        Xt_1[orders_x == 0] <- gsub(" ", "", objxvars[orders_x == 0], fixed = TRUE)
        Xt_1 <- objcoef[objnames_ws %in% Xt_1]
        dXt_interim <- rep(0, length(orders_x))
        dXt_interim[which(interim < orders_x)] <- objcoef[objnames_ws %in%
                    gsub(" ", "", paste0("d(L(", objxvars, ", ", interim, "))"), fixed = TRUE)]
        if (kw != 0) {
            multipliers_coef <- c(
                -objcoef[1:kw] / objcoef[kw + 1],
                -(Xt_1 + dXt_interim) / objcoef[kw + 1]
            )
        } else {
            multipliers_coef <- -(Xt_1 + dXt_interim) / objcoef[kw + 1]
        }
    }

    if (kw != 0) {
        names(multipliers_coef) <- c(names(objcoef)[1:kw], objxvars)
    } else {
        names(multipliers_coef) <- objxvars
    }

    multipliers_coef_se <- delta_method(object, vcov_matrix = vcov_matrix)
    multipliers <- data.frame(estimate = multipliers_coef, std.error = multipliers_coef_se, t.statistic = multipliers_coef/multipliers_coef_se,
        p.value = 2 * stats::pt(-abs(multipliers_coef/multipliers_coef_se), df = stats::df.residual(object))) # df = n - # of estimated coefficients
    multipliers <- data.frame(term = rownames(multipliers), multipliers)
    rownames(multipliers) <- 1:nrow(multipliers)

    return(multipliers)
}
