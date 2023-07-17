#' Multipliers estimation
#'
#' \code{multipliers} is a generic function used to estimate short-run (impact),
#' delay, interim and long-run (total) multipliers, accompanied by their
#' corresponding standard errors, t-statistics and p-values.
#'
#' The function invokes two different \code{\link[utils]{methods}}, one for
#' objects of \code{\link[base]{class}} 'ardl' and one for objects of
#' \code{class} 'uecm'. This is because of the different (but equivalent)
#' transformation functions that are used for each class/model ('ardl' and
#' 'uecm') to estimate the multipliers.
#'
#' \code{type = 0} is equivalent to \code{type = "sr"}.
#'
#' Note that the interim multipliers are the cumulative sum of the delays, and
#' that the sum of the interim multipliers (for long enough periods) and thus
#' a distant enough interim multiplier match the long-run multipliers.
#'
#' The delay (interim) multiplier can be interpreted as the effect in period
#' t+s, of an instant (sustained) shock in period t.
#'
#' The delta method is used for approximating the standard errors (and thus the
#' t-statistics and p-values) of the estimated long-run and delay multipliers.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl' or 'uecm'.
#' @param type A character string describing the type of multipliers. Use "lr"
#' for long-run (total) multipliers (default), "sr" or 0 for short-run (impact)
#' multipliers or an integer between 1 and 200 for delay and interim multipliers.
#' @param vcov_matrix The estimated covariance matrix of the random variable
#'   that the transformation function uses to estimate the standard errors (and
#'   so the t-statistics and p-values) of the multipliers. The default is
#'   \code{vcov(object)} (when \code{vcov_matrix = NULL}), but other estimations
#'   of the covariance matrix of the regression's estimated coefficients can
#'   also be used (e.g., using \code{\link[sandwich]{vcovHC}} or
#'   \code{\link[sandwich]{vcovHAC}}).
#' @param se A logical indicating whether you want standard errors for delay
#' multipliers to be provided. The default is FALSE. Note that this parameter
#' does not refer to the standard errors for the long-run multipliers, which are
#' always calculated. IMPORTANT: Calculating standard errors for long periods of
#' delays may cause your computer to run out of memory and terminate your R
#' session, losing important unsaved work. As a rule of thumb, try not to exceed
#' \code{type = 19} when \code{se = TRUE}.
#'
#' @return \code{multipliers} returns (for long and short run multipliers) a
#'   data.frame containing the independent variables (including possibly
#'   existing intercept or trend and excluding the fixed variables) and their
#'   corresponding standard errors, t-statistics and p-values. For delay and
#'   interim multipliers it returns a list with a data.frame for each variable,
#'   containing the delay and interim multipliers for each period.
#'
#' @section Mathematical Formula:
#'
#' \strong{Short-Run Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\frac{\partial y_{t}}{\partial x_{j,t}} = b_{j,0} \;\;\;\;\; j \in \{1,\dots,k\}}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\frac{\partial y_{t}}{\partial x_{j,t}} = \omega_{j} \;\;\;\;\; j \in \{1,\dots,k\}}
#'
#' \describe{
#'   \item{Constant and Linear Trend:}{}
#' }
#' \deqn{c_{0}}
#' \deqn{c_{1}}
#'
#' \strong{Delay & Interim Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{Delay_{x_{j},s} = \frac{\partial y_{t+s}}{\partial x_{j,t}} = b_{j,s} + \sum_{i=1}^{min\{p,s\}} b_{y,i} \frac{\partial y_{t+(s-i)}}{\partial x_{j,t}} \;\;\;\;\; b_{j,s} = 0 \;\; \forall \;\; s > q}
#' \deqn{Interim_{x_{j},s} = \sum_{i=0}^{s} Delay_{x_{j},s}}
#'
#' \describe{
#'   \item{Constant and Linear Trend:}{}
#' }
#' \deqn{Delay_{intercept,s} = c_{0} + \sum_{i=1}^{min\{p,s\}} b_{y,i} Delay_{intercept,s-i} \;\;\;\;\; c_{0} = 0 \;\; \forall \;\; s \neq 0}
#' \deqn{Interim_{intercept,s} = \sum_{i=0}^{s} Delay_{intercept,s}}
#' \deqn{Delay_{trend,s} = c_{1} + \sum_{i=1}^{min\{p,s\}} b_{y,i} Delay_{trend,s-i} \;\;\;\;\; c_{1} = 0 \;\; \forall \;\; s \neq 0}
#' \deqn{Interim_{trend,s} = \sum_{i=0}^{s} Delay_{trend,s}}
#'
#' \strong{Long-Run Multipliers:}
#' \describe{
#'   \item{As derived from an ARDL:}{}
#' }
#' \deqn{\frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\sum_{l=0}^{q_{j}}b_{j,l}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; j \in \{1,\dots,k\}}
#' \describe{
#'   \item{Constant and Linear Trend:}{}
#' }
#' \deqn{\mu = \frac{c_{0}}{1-\sum_{i=1}^{p}b_{y,i}}}
#' \deqn{\delta = \frac{c_{1}}{1-\sum_{i=1}^{p}b_{y,i}}}
#'
#' \describe{
#'   \item{As derived from an Unrestricted ECM:}{}
#' }
#' \deqn{\frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\pi_{j}}{-\pi_{y}} \;\;\;\;\; j \in \{1,\dots,k\}}
#' \describe{
#'   \item{Constant and Linear Trend:}{}
#' }
#' \deqn{\mu = \frac{c_{0}}{-\pi_{y}}}
#' \deqn{\delta = \frac{c_{1}}{-\pi_{y}}}
#'
#' @seealso \code{\link{ardl}}, \code{\link{uecm}}, \code{\link{plot_delay}}
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
#' ## Estimate the delay & interim multipliers of an ARDL(3,1,3,2) model --
#'
#' mult_lr <- multipliers(uecm_3132, type = "lr")
#' mult_inter80 <- multipliers(uecm_3132, type = 80)
#'
#' mult_lr
#' sum(mult_inter80$`(Intercept)`$Delay)
#' mult_inter80$`(Intercept)`$Interim[nrow(mult_inter80$`(Intercept)`)]
#' sum(mult_inter80$LRY$Delay)
#' mult_inter80$LRY$Interim[nrow(mult_inter80$LRY)]
#' sum(mult_inter80$IBO$Delay)
#' mult_inter80$IBO$Interim[nrow(mult_inter80$IBO)]
#' sum(mult_inter80$IDE$Delay)
#' mult_inter80$IDE$Interim[nrow(mult_inter80$IDE)]
#' plot(mult_inter80$LRY$Delay, type='l')
#' plot(mult_inter80$LRY$Interim, type='l')
#'
#' mult_inter12 <- multipliers(uecm_3132, type = 12, se = TRUE)
#' plot_delay(mult_inter12, interval = 0.95)

multipliers <- function(object, type = "lr", vcov_matrix = NULL, se = FALSE) {
    UseMethod("multipliers")
}

#' @rdname multipliers
#' @export
#'

multipliers.ardl <- function(object, type = "lr", vcov_matrix = NULL, se = FALSE) {

    # no visible binding for global variable NOTE solution
    group_id <- coeff <- sums <- NULL; rm(group_id, coeff, sums)

    if (!(type %in% c("lr", "sr", 0:200))) {
        stop("'type' should be one of 'lr', 'sr' or a number between 0 and 200", call. = FALSE)
    }

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)
    kw <- object$parsed_formula$kw
    kx <- object$parsed_formula$kx
    kfixed <- object$parsed_formula$kfixed
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

        multipliers_se <- delta_method(object, vcov_matrix = vcov_matrix)
        multipliers <- data.frame(multipliers_coef, multipliers_se, multipliers_coef/multipliers_se,
                                  2 * stats::pt(-abs(multipliers_coef/multipliers_se), df = stats::df.residual(object))) # df = n - # of estimated coefficients
        multipliers <- data.frame(rownames(multipliers), multipliers)
        names(multipliers) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
        rownames(multipliers) <- 1:nrow(multipliers)

        return(multipliers)
    } else {
        b0 <- 1
        for (i in 1:(length(orders_x) -1)) {
            b0 <- c(b0, b0[length(b0)] + orders_x[i] +1)
        }
        b0 <- if (kw!=0) c(1:kw, b0+kw) else b0
        # create table without the y in levels and fixed
        sr_mult <- as.data.frame(summary(object)$coefficients) %>%
            dplyr::slice(-(from:to)) %>% dplyr::slice(b0)
        sr_mult <- cbind(Term = rownames(sr_mult), sr_mult)
        rownames(sr_mult) <- 1:nrow(sr_mult)
        if (type %in% 1:200) { # anything except 0:200 would have been stopped in the earlier check
            interim = type
            delays_table <- as.data.frame(summary(object)$coefficients) %>%
                dplyr::slice(-(from:to))
            if (kfixed != 0) {
                delays_table <- delays_table %>% dplyr::slice(-((nrow(delays_table)-kfixed+1):nrow(delays_table)))
            }
            delay <- list()
            int_mult <- list()
            if (se) xpressions <- list()
            orders_wx <- c(rep(0, kw), orders_x)
            if (kw != 0) {
                delay_names <- c(rownames(delays_table)[1:kw], object$parsed_formula$x_part$var)
            } else {
                delay_names <- object$parsed_formula$x_part$var
            }
            y_table <- dplyr::tibble(objcoef) %>% dplyr::slice(from:to)
            for (k in 1:(kx+kw)) {
                delay[[k]] <- delays_table$Estimate[1:(orders_wx[k]+1)]
                names(delay)[k] <- delay_names[k]
                int_mult[[k]] <- data.frame()
                names(int_mult)[[k]] <- delay_names[k]
                if (se) {
                    xpressions[[k]] <- data.frame()
                    names(xpressions)[[k]] <- delay_names[k]
                }
                for (ss in 0:interim) {
                    weights_n <- min(object$order[1], ss)
                    direct <- ifelse(ss <= orders_wx[k], delay[[k]][ss+1], 0)
                    if (se) {
                        skip_w_y <- ifelse((kw != 0) & k %in% 1:kw, 0 + k-1, kw + object$order[1])
                        if (direct == 0) {
                            direct_xpression <- NULL
                        } else {
                            direct_xpression <- paste0("x", skip_w_y + ifelse(!k %in% 1:kw, sum(orders_wx[kw:(k-1)]) + k-kw-1, 0) + (ss + 1))
                        }
                    }
                    if (ss == 0) {
                        int_mult[[k]] <- data.frame(Period = ss, Delay = direct)
                        if (se) xpressions[[k]] <- data.frame(Period = ss, xpression = direct_xpression)
                    } else {
                        int_mult[[k]] <- rbind(int_mult[[k]],
                                               data.frame(Period = ss,
                                                          Delay = direct +
                                                              sum(y_table[1:weights_n,] *
                                                                      rev(int_mult[[k]][(ss-(weights_n-1)):ss,"Delay"]))))
                        if (se) {
                            xpressions[[k]] <- rbind(xpressions[[k]],
                                                     data.frame(Period = ss,
                                                                xpression = paste0(c(direct_xpression,
                                                                                     paste0("x", (1:weights_n)+kw, "*(",
                                                                                            rev(xpressions[[k]][(ss-(weights_n-1)):ss,"xpression"]),
                                                                                            ")")), collapse = "+"
                                                                )))
                        }
                    }
                }
                delays_table <- delays_table %>% dplyr::slice(-(1:(orders_wx[k] + 1)))
            }
            if (se) xpressions <- lapply(xpressions, FUN = function(x) {data.frame(Period = x$Period, xpression = paste0("~ ", x$xpression))})
            if (se) xpressions <- lapply(xpressions, FUN = function(x) {msm::deltamethod(lapply(x$xpression, stats::formula), stats::coef(object), vcov_matrix)})
            for (i in 1:length(int_mult)) {
                if (se) int_mult[[i]] <- cbind(int_mult[[i]], "Std. Error Delay" = xpressions[[i]])
                rownames(int_mult[[i]]) <- NULL
            }
            int_mult <- lapply(int_mult, FUN = function(x) {cbind(x, Interim = cumsum(x$Delay))})
            return(int_mult)
        } else {
            return(sr_mult)
        }
    }
}

#' @rdname multipliers
#' @export
#'

multipliers.uecm <- function(object, type = "lr", vcov_matrix = NULL, se = FALSE) {

    if (!(type %in% c("lr", "sr", 0:200))) {
        stop("'type' should be one of 'lr', 'sr' or a number between 0 and 200", call. = FALSE)
    }

    if (is.null(vcov_matrix)) vcov_matrix <- stats::vcov(object)

    if (type %in% 1:200) {
        return(multipliers(object = ardl(object), type = type, vcov_matrix = vcov_matrix, se = se))
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

        multipliers_coef_se <- delta_method(object, vcov_matrix = vcov_matrix)
        multipliers <- data.frame(multipliers_coef, multipliers_coef_se, multipliers_coef/multipliers_coef_se,
                                  2 * stats::pt(-abs(multipliers_coef/multipliers_coef_se), stats::df.residual(object))) # df = n - # of estimated coefficients
    } else {
        # use gsub because sometimes the spacing goes weird
        Xt_1 <- c()
        Xt_1[orders_x != 0] <- gsub(" ", "", paste0("d(", objxvars, ")"),
            fixed = TRUE)[orders_x != 0]
        Xt_1[orders_x == 0] <- gsub(" ", "", objxvars[orders_x == 0], fixed = TRUE)
        if (kw != 0) {
            srm <- c(1:kw, which(objnames_ws %in% Xt_1))
            multipliers <- summary(object)$coefficients[srm,]
        } else {
            srm <- objnames_ws %in% Xt_1
            multipliers <- summary(object)$coefficients[srm,]
        }
    }

    if (kw != 0) {
        multipliers <- data.frame(c(names(objcoef)[1:kw], objxvars), multipliers)
    } else {
        multipliers <- data.frame(objxvars, multipliers)
    }
    names(multipliers) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
    rownames(multipliers) <- 1:nrow(multipliers)

    return(multipliers)
}
