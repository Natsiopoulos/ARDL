#' Bounds Wald-test for no cointegration
#'
#' \code{bounds_f_test} performs the Wald bounds-test for no cointegration
#' \cite{Pesaran et al. (2001)}. It is a Wald test on the parameters of a UECM
#' (Unrestricted Error Correction Model) expressed either as a Chisq-statistic
#' or as an F-statistic.
#'
#' @param alpha A numeric value between 0 and 1 indicating the significance
#'   level of the critical value bounds. If \code{NULL} (default), no critical
#'   value bounds for a specific level of significance are provide, only the
#'   p-value. See section 'alpha, bounds and p-value' below for details.
#' @param pvalue A logical indicating whether you want the p-value to be
#'   provided. The default is \code{TRUE}. See section 'alpha, bounds and
#'   p-value' below for details.
#' @param exact A logical indicating whether you want asymptotic (T = 1000) or
#'   exact sample size critical value bounds and p-value. The default is
#'   \code{FALSE} for asymptotic. See section 'alpha, bounds and p-value' below
#'   for details.
#' @param R An integer indicating how many iterations will be used if
#'   \code{exact = TRUE}. Default is 40000.
#' @param test A character vector indicating whether you want the Wald test to
#'   be expressed as 'F' or as 'Chisq' statistic. Default is "F".
#' @param vcov_matrix The estimated covariance matrix of the random variable
#'   that the test uses to estimate the test statistic. The default is
#'   \code{vcov(object)} (when \code{vcov_matrix = NULL}), but other estimations
#'   of the covariance matrix of the regression's estimated coefficients can
#'   also be used (e.g., using \code{\link[sandwich]{vcovHC}} or
#'   \code{\link[sandwich]{vcovHAC}}). Only applicable if the input object is of
#'   class "uecm".
#' @inheritParams recm
#'
#' @return A list with class "htest" containing the following components:
#'   \item{\code{method}}{a character string indicating what type of test was
#'      performed.}
#'   \item{\code{alternative}}{a character string describing the alternative
#'      hypothesis.}
#'   \item{\code{statistic}}{the value of the test statistic.}
#'   \item{\code{null.value}}{the value of the population parameters \code{k}
#'      (the number of independent variables) and \code{T} (the number of
#'      observations) specified by the null hypothesis.}
#'   \item{\code{data.name}}{a character string giving the name(s) of the data.}
#'   \item{\code{parameters}}{numeric vector containing the critical value
#'      bounds.}
#'   \item{\code{p.value}}{the p-value of the test.}
#'   \item{\code{PSS2001parameters}}{numeric vector containing the critical
#'      value bounds as presented by \cite{Pesaran et al. (2001)}. See section
#'      'alpha, bounds and p-value' below for details.}
#'   \item{\code{tab}}{data.frame containing the statistic, the critical value
#'      bounds, the alpha level of significance and the p-value.}
#'
#' @section Hypothesis testing:
#'   \deqn{\Delta y_{t} = c_{0} + c_{1}t +
#'   \pi_{y}y_{t-1} + \sum_{j=1}^{k}\pi_{j}x_{j,t-1} +
#'   \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} +
#'   \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} +
#'   \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \epsilon_{t}}
#'
#' \describe{
#'   \item{Cases 1, 3, 5:}{}
#' }
#'   \deqn{\mathbf{H_{0}:} \pi_{y} = \pi_{1} = \dots = \pi_{k} = 0}
#'   \deqn{\mathbf{H_{1}:} \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq 0}
#'
#' \describe{
#'   \item{Case 2:}{}
#' }
#'   \deqn{\mathbf{H_{0}:} \pi_{y} = \pi_{1} = \dots = \pi_{k} = c_{0} = 0}
#'   \deqn{\mathbf{H_{1}:} \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq c_{0} \neq 0}
#'
#' \describe{
#'   \item{Case 4:}{}
#' }
#'   \deqn{\mathbf{H_{0}:} \pi_{y} = \pi_{1} = \dots = \pi_{k} = c_{1} = 0}
#'   \deqn{\mathbf{H_{1}:} \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq c_{1} \neq 0}
#'
#' @section alpha, bounds and p-value: In this section it is explained how the
#'   critical value bounds and p-values are obtained.
#'   \itemize{
#'      \item If \code{exact = FALSE}, then the asymptotic (T = 1000) critical
#'          value bounds and p-value are provided.
#'      \item Only the asymptotic critical value bounds and p-values, and only
#'          for k <= 10 are precalculated, everything else has to be computed.
#'      \item Precalculated critical value bounds and p-values were simulated
#'          using \code{set.seed(2020)} and \code{R = 70000}.
#'      \item Precalculated critical value bounds exist only for \code{alpha}
#'          being one of the 0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.15 or 0.2,
#'          everything else has to be computed.
#'      \item If \code{alpha} is one of the 0.1, 0.05, 0.025 or 0.01 (and
#'          \code{exact = FALSE} and k <= 10), \code{PSS2001parameters} shows
#'          the critical value bounds presented in \cite{Pesaran et al. (2001)}
#'          (less precise).
#'   }
#'
#' @inheritSection recm Cases
#' @inheritSection recm References
#' @seealso \code{\link{bounds_t_test}} \code{\link{ardl}} \code{\link{uecm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords htest ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## How to use cases under different models (regarding deterministic terms)
#'
#' ## Construct an ARDL(3,1,3,2) model with different deterministic terms -
#'
#' # Without constant
#' ardl_3132_n <- ardl(LRM ~ LRY + IBO + IDE -1, data = denmark, order = c(3,1,3,2))
#'
#' # With constant
#' ardl_3132_c <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#'
#' # With constant and trend
#' ardl_3132_ct <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM), data = denmark, order = c(3,1,3,2))
#'
#' ## F-bounds test for no level relationship (no cointegration) ----------
#'
#' # For the model without a constant
#' bounds_f_test(ardl_3132_n, case = 1)
#' # or
#' bounds_f_test(ardl_3132_n, case = "n")
#'
#' # For the model with a constant
#' # Including the constant term in the long-run relationship (restricted constant)
#' bounds_f_test(ardl_3132_c, case = 2)
#' # or
#' bounds_f_test(ardl_3132_c, case = "rc")
#'
#' # Including the constant term in the short-run relationship (unrestricted constant)
#' bounds_f_test(ardl_3132_c, case = "uc")
#' # or
#' bounds_f_test(ardl_3132_c, case = 3)
#'
#' # For the model with constant and trend
#' # Including the constant term in the short-run and the trend in the long-run relationship
#' # (unrestricted constant and restricted trend)
#' bounds_f_test(ardl_3132_ct, case = "ucrt")
#' # or
#' bounds_f_test(ardl_3132_ct, case = 4)
#'
#' # For the model with constant and trend
#' # Including the constant term and the trend in the short-run relationship
#' # (unrestricted constant and unrestricted trend)
#' bounds_f_test(ardl_3132_ct, case = "ucut")
#' # or
#' bounds_f_test(ardl_3132_ct, case = 5)
#'
#' ## Note that you can't restrict a deterministic term that doesn't exist
#'
#' # For example, the following tests will produce an error:
#' \dontrun{
#' bounds_f_test(ardl_3132_c, case = 1)
#' bounds_f_test(ardl_3132_ct, case = 3)
#' bounds_f_test(ardl_3132_c, case = 4)
#' }
#'
#' ## Asymptotic p-value and critical value bounds (assuming T = 1000) ----
#'
#' # Include critical value bounds for a certain level of significance
#'
#' # F-statistic is larger than the I(1) bound (for a=0.05) as expected (p-value < 0.05)
#' bft <- bounds_f_test(ardl_3132_c, case = 2, alpha = 0.05)
#' bft
#' bft$tab
#'
#' # Traditional but less precise critical value bounds, as presented in Pesaran et al. (2001)
#' bft$PSS2001parameters
#'
#' # F-statistic is slightly larger than the I(1) bound (for a=0.005)
#' # as p-value is slightly smaller than 0.005
#' bounds_f_test(ardl_3132_c, case = 2, alpha = 0.005)
#'
#' ## Exact sample size p-value and critical value bounds -----------------
#'
#' # Setting a seed is suggested to allow the replication of results
#' # 'R' can be increased for more accurate resutls
#'
#' # F-statistic is smaller than the I(1) bound (for a=0.01) as expected (p-value > 0.01)
#' # Note that the exact sample p-value (0.01285) is very different than the asymptotic (0.004418)
#' # It can take more than 30 seconds
#' \dontrun{
#' set.seed(2020)
#' bounds_f_test(ardl_3132_c, case = 2, alpha = 0.01, exact = TRUE)
#' }
#'
#' ## "F" and "Chisq" statistics ------------------------------------------
#'
#' # The p-value is the same, the test-statistic and critical value bounds are different but analogous
#' bounds_f_test(ardl_3132_c, case = 2, alpha = 0.01)
#' bounds_f_test(ardl_3132_c, case = 2, alpha = 0.01, test = "Chisq")

bounds_f_test <- function(object, case, alpha = NULL, pvalue = TRUE, exact = FALSE,
    R = 40000, test = c("F", "Chisq"), vcov_matrix = NULL) {
    # no visible binding for global variable NOTE solution
    k <- I0 <- fI0 <- fI1 <- NULL; rm(k, I0, fI0, fI1)

    if (isTRUE(all.equal(c("dynlm", "lm", "ardl"), class(object)))) {
        object <- uecm(object)
        vcov_matrix <- stats::vcov(object)
    }

    if (is.null(vcov_matrix)) {
        vcov_matrix <- stats::vcov(object)
    }
    alpha_long_seq <- c(seq(0, 0.1, by = 0.001), seq(0.11, 1, by = 0.01))
    alpha_short_seq <- c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.15, 0.2)
    if (is.null(alpha) & (pvalue == FALSE)) {
        stop("Specify an 'alpha' level, set 'pvalue' = TRUE or both", call. = FALSE)
    }
    if ((!is.null(alpha) & (length(alpha) != 1)) | (!is.null(alpha) & !any(dplyr::near(alpha, alpha_long_seq)))) {
        stop("alpha must be either 'NULL' or one of the numbers produced by the following code:\n",
            "c(seq(0.001, 0.1, by = 0.001), seq(0.11, 0.99, by = 0.01))", call. = FALSE)
    }
    case <- parse_case(parsed_formula = object$parsed_formula, case = case)
    test <- match.arg(test)
    kx <- object$parsed_formula$kx
    T <- length(object$residuals)
    dname <- deparse(object$full_formula)
    if (exact == FALSE) {
        nullvalue <- c(kx, 1000)
    } else {
        nullvalue <- c(kx, T)
    }
    names(nullvalue) <- c("k", "T")

    indep_vars <- object$parsed_formula$x_part$var
    restricted_coef <- sapply(1:length(indep_vars), function(i) {
        ifelse(object$order[i + 1] == 0, indep_vars[i], paste0("L(", indep_vars[i], ", 1)"))
    })
    restricted_names <- c(paste0("L(", object$parsed_formula$y_part$var, ", 1)"), restricted_coef)
    restricted_coef <- which(names(stats::coef(object)) %in% restricted_names)

    if (case == 2) {
        restricted_names <- c(restricted_names, names(stats::coef(object))[1])
        restricted_coef <- which(names(stats::coef(object)) %in% restricted_names)
    } else if (case == 4) {
        restricted_names <- c(restricted_names, names(stats::coef(object))[2])
        restricted_coef <- which(names(stats::coef(object)) %in% restricted_names)
    }
    chi2_statistic <- aod::wald.test(b = stats::coef(object), Sigma = vcov_matrix, Terms = restricted_coef)$result
    if (test == "F") {
        statistic <- chi2_statistic$chi2[1] / chi2_statistic$chi2[2]
    } else {
        statistic <- chi2_statistic$chi2[1]
    }
    names(statistic) <- test
    if ((exact == FALSE) & (kx <= 10) & any(c(is.null(alpha), alpha %in% alpha_short_seq))) {
        caselatin <- ifelse(case == 1, "i",
                     ifelse(case == 2, "ii",
                     ifelse(case == 3, "iii",
                     ifelse(case == 4, "iv", "v"))))
        if (!is.null(alpha)) {
            if (alpha %in% c(0.1, 0.05, 0.025, 0.01)) {
                bounds_pss2001 <- eval(parse(text = paste0("crit_val_bounds_pss2001$f$", caselatin))) %>%
                    dplyr::filter(alpha %in% !!alpha, k == kx) %>%
                    dplyr::select(I0, I1)
                if (test != "F") {
                    if (case %in% c(1, 3, 5)) {
                        bounds_pss2001 <- bounds_pss2001*(kx+1)
                    } else {
                        bounds_pss2001 <- bounds_pss2001*(kx+2)
                    }
                }
            }
            if (alpha %in% alpha_short_seq) {
                bounds <- eval(parse(text = paste0("crit_val_bounds$I0$", caselatin))) %>%
                    dplyr::filter(alpha == !!alpha, k == kx) %>%
                    dplyr::select(fI0) %>%
                    dplyr::rename(I0 = fI0)
                bounds <- cbind(bounds,
                    eval(parse(text = paste0("crit_val_bounds$I1$", caselatin))) %>%
                        dplyr::filter(alpha == !!alpha, k == kx) %>%
                        dplyr::select(fI1) %>%
                        dplyr::rename(I1 = fI1))
                if (test != "F") {
                    if (case %in% c(1, 3, 5)) {
                        bounds <- bounds*(kx+1)
                    } else {
                        bounds <- bounds*(kx+2)
                    }
                }
            }
        }
        if (pvalue == TRUE) {
            critvalbounds <- eval(parse(text = paste0("crit_val_bounds$I1$", caselatin))) %>%
                             dplyr::mutate(I1 = if (test == "F") {
                                    fI1
                                } else if (case %in% c(1, 3, 5)){
                                    fI1*(kx+1)
                                } else {
                                    fI1*(kx+2)
                                })
            if (statistic %in% critvalbounds$I1) {
                p_value <- critvalbounds %>%
                    dplyr::filter(k == kx) %>%
                    dplyr::filter(I1 == statistic) %>%
                    dplyr::select(alpha) %>%
                    unlist()
            } else { # linear interpolation
                if (max(dplyr::filter(critvalbounds, k == kx)$I1) < statistic) {
                    p_value <- 0.000001
                } else if (min(dplyr::filter(critvalbounds, k == kx)$I1) > statistic) {
                    p_value <- 0.999999
                } else {
                    p_value <- critvalbounds %>%
                        dplyr::filter(k == kx) %>%
                        dplyr::filter(I1 %in% c(max(I1[which(I1 < statistic)]),
                            min(I1[which(I1 > statistic)]))) %>%
                        dplyr::mutate(p_value = min(alpha) + (max(alpha)-min(alpha)) * ((max(I1) - statistic) /
                            (max(I1)-min(I1)))) %>%
                        dplyr::select(p_value) %>% unlist() %>% .[1]
                }
            }
        }
    } else {
        T_asy_or_exact <- ifelse(exact == FALSE, 1000, T)
        wb <- f_bounds_sim(case = case, k = kx, alpha = alpha_long_seq,
                              T = T_asy_or_exact, R = R)
        if (!is.null(alpha)) {
            if (test == "F") {
                bounds <- wb$f_bounds[which(dplyr::near(alpha, alpha_long_seq)),]
            } else {
                bounds <- wb$chisq_bounds[which(dplyr::near(alpha, alpha_long_seq)),]
            }
        }
        if (pvalue == TRUE) {
            len <- length(alpha_long_seq)
            if (test == "F") {
                I1 <- wb$f_bounds$I1[1:len]
            } else {
                I1 <- wb$chisq_bounds$I1[1:len]
            }
            if (statistic %in% I1) {
                p_value <- alpha_long_seq[statistic == I1]
            } else { # linear interpolation
                if (max(I1) < statistic) {
                    p_value <- 0.000001
                } else if (min(I1) > statistic) {
                    p_value <- 0.999999
                } else {
                    interp_cond <- I1 %in% c(max(I1[which(I1 < statistic)]),
                                              min(I1[which(I1 > statistic)]))
                    interp_I1 <- I1[interp_cond]
                    interp_alpha <- alpha_long_seq[interp_cond]
                    p_value <- min(interp_alpha) + (max(interp_alpha)-min(interp_alpha)) *
                               ((max(interp_I1) - statistic) / (max(interp_I1)-min(interp_I1)))
                }
            }
        }
    }

    method <- paste0("Bounds ", test, "-test (Wald) for no cointegration")
    alternative <- "Possible cointegration"
    rval <- list(method = method, alternative = alternative, statistic = statistic,
                 null.value = nullvalue, data.name = dname)
    tab <- data.frame(statistic = statistic)
    if (!is.null(alpha)) {
        parameters <- c(bounds$I0, bounds$I1)
        names(parameters) <- c("Lower-bound I(0)", "Upper-bound I(1)")
        tab <- cbind(tab, "Lower-bound I(0)" = parameters[1], "Upper-bound I(1)" = parameters[2],
                                  alpha = alpha)
        rval$parameters <- parameters
        if ((alpha %in% c(0.1, 0.05, 0.025, 0.01)) & exact == FALSE) {
            PSS2001parameters <- c(bounds_pss2001$I0, bounds_pss2001$I1)
            names(PSS2001parameters) <- c("Lower-bound I(0)", "Upper-bound I(1)")
            rval$PSS2001parameters <- PSS2001parameters
        }
    }
    if (pvalue == TRUE) {
        tab <- cbind(tab, p.value = p_value)
        rval$p.value <- p_value
    }
    rval$tab <- tab
    class(rval) <- "htest"
    return(rval)
}

#' Bounds t-test for no cointegration
#'
#' \code{bounds_t_test} performs the t-bounds test for no cointegration
#' \cite{Pesaran et al. (2001)}. It is a t-test on the parameters of a UECM
#' (Unrestricted Error Correction Model).
#'
#' @param case An integer (1, 3 or 5) or a character string specifying whether
#'   the 'intercept' and/or the 'trend' have to participate in the
#'   short-run relationship (see section 'Cases' below). Note that the t-bounds
#'   test can't be applied for cases 2 and 4.
#' @inheritParams bounds_f_test
#' @inherit bounds_f_test return
#'
#' @section Hypothesis testing: \deqn{\Delta y_{t} = c_{0} + c_{1}t +
#'   \pi_{y}y_{t-1} + \sum_{j=1}^{k}\pi_{j}x_{j,t-1} +
#'   \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} +
#'   \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} +
#'   \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \epsilon_{t}}
#'   \deqn{\mathbf{H_{0}:} \pi_{y} = 0}
#'   \deqn{\mathbf{H_{1}:} \pi_{y} \neq 0}
#'
#' @inheritSection bounds_f_test alpha, bounds and p-value
#' @inheritSection bounds_f_test Cases
#' @inheritSection bounds_f_test References
#' @seealso \code{\link{bounds_f_test}} \code{\link{ardl}} \code{\link{uecm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords htest ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## How to use cases under different models (regarding deterministic terms)
#'
#' ## Construct an ARDL(3,1,3,2) model with different deterministic terms -
#'
#' # Without constant
#' ardl_3132_n <- ardl(LRM ~ LRY + IBO + IDE -1, data = denmark, order = c(3,1,3,2))
#'
#' # With constant
#' ardl_3132_c <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#'
#' # With constant and trend
#' ardl_3132_ct <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM), data = denmark, order = c(3,1,3,2))
#'
#' ## t-bounds test for no level relationship (no cointegration) ----------
#'
#' # For the model without a constant
#' bounds_t_test(ardl_3132_n, case = 1)
#' # or
#' bounds_t_test(ardl_3132_n, case = "n")
#'
#' # For the model with a constant
#' # Including the constant term in the short-run relationship (unrestricted constant)
#' bounds_t_test(ardl_3132_c, case = "uc")
#' # or
#' bounds_t_test(ardl_3132_c, case = 3)
#'
#' # For the model with constant and trend
#' # Including the constant term and the trend in the short-run relationship
#' # (unrestricted constant and unrestricted trend)
#' bounds_t_test(ardl_3132_ct, case = "ucut")
#' # or
#' bounds_t_test(ardl_3132_ct, case = 5)
#'
#' ## Note that you can't use bounds t-test for cases 2 and 4, or use a wrong model
#'
#' # For example, the following tests will produce an error:
#' \dontrun{
#' bounds_t_test(ardl_3132_n, case = 2)
#' bounds_t_test(ardl_3132_c, case = 4)
#' bounds_t_test(ardl_3132_ct, case = 3)
#' }
#'
#' ## Asymptotic p-value and critical value bounds (assuming T = 1000) ----
#'
#' # Include critical value bounds for a certain level of significance
#'
#' # t-statistic is larger than the I(1) bound (for a=0.05) as expected (p-value < 0.05)
#' btt <- bounds_t_test(ardl_3132_c, case = 3, alpha = 0.05)
#' btt
#' btt$tab
#'
#' # Traditional but less precise critical value bounds, as presented in Pesaran et al. (2001)
#' btt$PSS2001parameters
#'
#' # t-statistic doesn't exceed the I(1) bound (for a=0.005) as p-value is greater than 0.005
#' bounds_t_test(ardl_3132_c, case = 3, alpha = 0.005)
#'
#' ## Exact sample size p-value and critical value bounds -----------------
#'
#' # Setting a seed is suggested to allow the replication of results
#' # 'R' can be increased for more accurate resutls
#'
#' # t-statistic is smaller than the I(1) bound (for a=0.01) as expected (p-value > 0.01)
#' # Note that the exact sample p-value (0.009874) is very different than the asymptotic (0.005538)
#' # It can take more than 90 seconds
#' \dontrun{
#' set.seed(2020)
#' bounds_t_test(ardl_3132_c, case = 3, alpha = 0.01, exact = TRUE)
#' }

bounds_t_test <- function(object, case, alpha = NULL, pvalue = TRUE,
                               exact = FALSE, R = 40000, vcov_matrix = NULL) {
    # no visible binding for global variable NOTE solution
    k <- I0 <- tI0 <- tI1 <- NULL; rm(k, I0, tI0, tI1)

    if (isTRUE(all.equal(c("dynlm", "lm", "ardl"), class(object)))) {
        object <- uecm(object)
        vcov_matrix <- stats::vcov(object)
    }

    if (is.null(vcov_matrix)) {
        vcov_matrix <- stats::vcov(object)
    }
    alpha_long_seq <- c(seq(0, 0.1, by = 0.001), seq(0.11, 1, by = 0.01))
    alpha_short_seq <- c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.15, 0.2)
    if (is.null(alpha) & (pvalue == FALSE)) {
        stop("Specify an 'alpha' level, set 'pvalue' = TRUE or both", call. = FALSE)
    }
    if ((!is.null(alpha) & (length(alpha) != 1)) | (!is.null(alpha) & !any(dplyr::near(alpha, alpha_long_seq)))) {
        stop("alpha must be either 'NULL' or one of the numbers produced by the following code:\n",
            "c(seq(0.001, 0.1, by = 0.001), seq(0.11, 0.99, by = 0.01))", call. = FALSE)
    }
    case <- parse_case(parsed_formula = object$parsed_formula, case = case)
    if (!(case %in% c(1, 3, 5))) {
        stop("The t-bounds test applies only when 'case' is either 1, 3 or 5", call. = FALSE)
    }
    kx <- object$parsed_formula$kx
    T <- length(object$residuals)
    dname <- deparse(object$full_formula)
    if (exact == FALSE) {
        nullvalue <- c(kx, 1000)
    } else {
        nullvalue <- c(kx, T)
    }
    names(nullvalue) <- c("k", "T")

    indep_vars <- object$parsed_formula$x_part$var
    restricted_coef <- sapply(1:length(indep_vars), function(i) {
        ifelse(object$order[i + 1] == 0, indep_vars[i], paste0("L(", indep_vars[i], ", 1)"))
    })
    restricted_names <- c(paste0("L(", object$parsed_formula$y_part$var, ", 1)"), restricted_coef)
    restricted_coef <- which(names(stats::coef(object)) %in% restricted_names)

    statistic <- lmtest::coeftest(object, vcov = vcov_matrix)[restricted_coef[1], "t value"]
    names(statistic) <- "t"

    if ((exact == FALSE) & (kx <= 10) & any(c(is.null(alpha), alpha %in% alpha_short_seq))) {
        caselatin <- ifelse(case == 1, "i",
                     ifelse(case == 3, "iii", "v"))
        if (!is.null(alpha)) {
            if (alpha %in% c(0.1, 0.05, 0.025, 0.01)) {
                bounds_pss2001 <- eval(parse(text = paste0("crit_val_bounds_pss2001$t$", caselatin))) %>%
                    dplyr::filter(alpha %in% !!alpha, k == kx) %>%
                    dplyr::select(I0, I1)
            }
            if (alpha %in% alpha_short_seq) {
                bounds <- eval(parse(text = paste0("crit_val_bounds$I0$", caselatin))) %>%
                    dplyr::filter(alpha == !!alpha, k == kx) %>%
                    dplyr::select(tI0) %>%
                    dplyr::rename(I0 = tI0)
                bounds <- cbind(bounds,
                    eval(parse(text = paste0("crit_val_bounds$I1$", caselatin))) %>%
                        dplyr::filter(alpha == !!alpha, k == kx) %>%
                        dplyr::select(tI1) %>%
                        dplyr::rename(I1 = tI1))
            }
        }
        if (pvalue == TRUE) {
            critvalbounds <- eval(parse(text = paste0("crit_val_bounds$I1$", caselatin)))
            if (statistic %in% critvalbounds$tI1) {
                p_value <- critvalbounds %>%
                    dplyr::filter(k == kx) %>%
                    dplyr::filter(tI1 == statistic) %>%
                    dplyr::select(alpha) %>%
                    unlist()
            } else {
                if (max(dplyr::filter(critvalbounds, k == kx)$tI1) < statistic) {
                    p_value <- 0.999999
                } else if (min(dplyr::filter(critvalbounds, k == kx)$tI1) > statistic) {
                    p_value <- 0.000001
                } else {
                    p_value <- critvalbounds %>%
                        dplyr::filter(k == kx) %>%
                        dplyr::filter(tI1 %in% c(max(tI1[which(tI1 < statistic)]),
                                                 min(tI1[which(tI1 > statistic)]))) %>%
                        dplyr::mutate(p_value = max(alpha) - (max(alpha)-min(alpha)) * ((max(tI1) - statistic) /
                            (max(tI1)-min(tI1)))) %>%
                        dplyr::select(p_value) %>% unlist() %>% .[1]
                }
            }
        }
    } else {
        T_asy_or_exact <- ifelse(exact == FALSE, 1000, T)
        tb <- t_bounds_sim(case = case, k = kx, alpha = alpha_long_seq,
                              T = T_asy_or_exact, R = R)
        if (!is.null(alpha)) {
            bounds <- tb[which(dplyr::near(alpha, alpha_long_seq)),]
        }
        if (pvalue == TRUE) {
            len <- length(alpha_long_seq)
            I1 <- tb$I1[1:len]
            if (statistic %in% I1) {
                p_value <- alpha_long_seq[statistic == I1]
            } else { # linear interpolation
                if (max(I1) < statistic) {
                    p_value <- 0.999999
                } else if (min(I1) > statistic) {
                    p_value <- 0.000001
                } else {
                    interp_cond <- I1 %in% c(max(I1[which(I1 < statistic)]),
                                              min(I1[which(I1 > statistic)]))
                    interp_I1 <- I1[interp_cond]
                    interp_alpha <- alpha_long_seq[interp_cond]
                    p_value <- max(interp_alpha) - (max(interp_alpha)-min(interp_alpha)) *
                               ((max(interp_I1) - statistic) / (max(interp_I1)-min(interp_I1)))
                }
            }
        }
    }
    method <- "Bounds t-test for no cointegration"
    alternative <- "Possible cointegration"
    rval <- list(method = method, alternative = alternative, statistic = statistic,
                 null.value = nullvalue, data.name = dname)
    tab <- data.frame(statistic = statistic)
    if (!is.null(alpha)) {
        parameters <- c(bounds$I0, bounds$I1)
        names(parameters) <- c("Lower-bound I(0)", "Upper-bound I(1)")
        tab <- cbind(tab, "Lower-bound I(0)" = parameters[1], "Upper-bound I(1)" = parameters[2],
                                  alpha = alpha)
        rval$parameters <- parameters
        if ((alpha %in% c(0.1, 0.05, 0.025, 0.01)) & exact == FALSE) {
            PSS2001parameters <- c(bounds_pss2001$I0, bounds_pss2001$I1)
            names(PSS2001parameters) <- c("Lower-bound I(0)", "Upper-bound I(1)")
            rval$PSS2001parameters <- PSS2001parameters
        }
    }
    if (pvalue == TRUE) {
        tab <- cbind(tab, p.value = p_value)
        rval$p.value <- p_value
    }
    rval$tab <- tab
    class(rval) <- "htest"
    return(rval)
}
