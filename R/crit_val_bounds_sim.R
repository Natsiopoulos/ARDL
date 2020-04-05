#' F-test of regression's overall significance
#'
#' \code{f_test_custom} performs an overall significance F-test on a regression.
#' It is used along with \code{\link[stats]{.lm.fit}} to get the F-statistic as
#' this is about 10 times faster than extracting it from a regression using
#' \code{\link[stats]{lm}}.
#'
#' @param dep_var A numeric vector or a matrix with one column representing the
#'   dependent variable.
#' @param indep_vars A matrix representing the independent variables.
#' @param model_res A numeric vector representing the regression's residuals.
#' @param const A logical indicating whether the constant term should be
#'   restricted too.
#'
#' @return \code{f_test_custom} returns a list containing the F-statistic and
#'   the numerator's degrees of freedom.
#' @seealso \code{\link{vcov_custom}} \code{\link{f_bounds_sim}}
#'   \code{\link{t_bounds_sim}}independent
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

f_test_custom <- function(dep_var, indep_vars, model_res, const = TRUE){
    df2 <- nrow(indep_vars) - ncol(indep_vars)
    dep_var_mean <- mean(dep_var)
    mss <- sum(model_res^2)
    if( const == TRUE ) {
        df1 <- ncol(indep_vars) - 1 #number of restricted coefficients
        mss0 <- sum((dep_var - dep_var_mean)^2)
    } else {
        df1 <- ncol(indep_vars) #number of restricted coefficients
        mss0 <- sum((dep_var)^2)
    }
    return(list(f = ((mss0 - mss)/df1) / (mss/df2), df1 = df1))
}

#' Variance-Covariance matrix of a regression
#'
#' \code{vcov_custom} creates the Variance-Covariance matrix of a regression. It
#' is used instead of the \code{\link[stats]{vcov}} because the latter doesn't
#' work with \code{\link[stats]{.lm.fit}}.
#'
#' @param indep_vars A matrix representing the independent variables.
#' @param model_res A numeric vector representing the regression's residuals.
#'
#' @return \code{vcov_custom} returns a Variance-Covariance matrix.
#' @seealso \code{\link{f_test_custom}} \code{\link{f_bounds_sim}}
#'   \code{\link{t_bounds_sim}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

vcov_custom <- function(indep_vars, model_res){
    xx <- t(indep_vars) %*% indep_vars
    return(solve(xx) * sum(model_res ^ 2) / (nrow(indep_vars) - ncol(indep_vars)))
}

#' Critical value bounds stochastic simulation for Wald bounds-test for no
#' cointegration
#'
#' \code{f_bounds_sim} simulates the critical value bounds for the  Wald
#' bounds-test for no cointegration \cite{Pesaran et al. (2001)} expressed both
#' as F-statistics and as Chisq-statistics.
#'
#' @param case An integer from 1-5 specifying whether the 'intercept' and/or the
#'   trend' have to participate in the long-run/cointegrating
#'   relationship/equation (see section 'Cases' in \code{\link{bounds_f_test}}).
#' @param k The number of independent variables.
#' @param alpha A numeric vector between 0 and 1 indicating the significance
#'   level of the critical value bounds. Multiple values can be used.
#' @param T An integer indicating the number of observations.
#' @param R An integer indicating how many iterations will be used. Default is
#'   40000.
#'
#' @return \code{f_bounds_sim} returns a list containing two data frames. One
#'   with the critical value bounds for the F-statistic and one with the
#'   critical value bounds for the Chisq-statistic.
#' @seealso \code{\link{t_bounds_sim}} \code{\link{bounds_f_test}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal ts
#'

f_bounds_sim <- function(case, k, alpha, T, R = 40000) {
	chi2_crit_val0 <- c()
	chi2_crit_val1 <- c()
    f_crit_val0 <- c()
    f_crit_val1 <- c()
	for(i in 1:R) {
        # set the independent covariates
        y1 <- cumsum(stats::rnorm(T))
        dep_var <- as.matrix(diff(y1, 1))
        y1lag1 <- as.matrix(y1[1:(T-1)])

        if (k != 0) {
            x <- matrix(stats::rnorm(T * k), T, k)
            x1 <- apply(x, 2, cumsum)
            xlag1 <- x[1:(T - 1), ]
            x1lag1 <- x1[1:(T - 1), ]
        }

        # set the deterministic linear trend
    	if (case %in% c(1, 2, 3)) {
            # dataset already set
        } else if (case %in% c(4, 5)) {
    	   	if (k == 0) {
    			xlag1 <- c(t = 1:(T - 1))
    			x1lag1 <- c(t = 1:(T - 1))
    		} else {
		 		xlag1 <- cbind(t = c(1:(T - 1)), xlag1)
    			x1lag1 <- cbind(t = c(1:(T - 1)), x1lag1)
    		}
    	}
        xlag1 <- as.matrix(xlag1)
        x1lag1 <- as.matrix(x1lag1)

        if (case == 1) {
    		if (k == 0) {
                indep_vars <- y1lag1
                model0 <- stats::.lm.fit(y = dep_var, x = indep_vars) # I(0) and I(1) are identical
                f_test <- f_test_custom(dep_var, indep_vars, model0$residuals, const = FALSE)
    			f_crit_val0[i] <- f_crit_val1[i] <- f_test$f # F and Chisq are identical for k=0 & no c,t
                chi2_crit_val0[i] <- chi2_crit_val1[i] <- f_test$f / f_test$df1
    		} else {
                indep_vars <- as.matrix(cbind(xlag1, y1lag1))
                model0 <- stats::.lm.fit(y = dep_var, x = indep_vars)
                f_test <- f_test_custom(dep_var, indep_vars, model0$residuals, const = FALSE)
				f_crit_val0[i] <- f_test$f
                chi2_crit_val0[i] <-f_test$f * f_test$df1

                indep_vars <- as.matrix(cbind(x1lag1, y1lag1))
                model1 <- stats::.lm.fit(y = dep_var, x = indep_vars)
                f_test <- f_test_custom(dep_var, indep_vars, model1$residuals, const = FALSE)
				f_crit_val1[i] <- f_test$f
				chi2_crit_val1[i] <- f_test$f * f_test$df1
    		}
    	} else if (case %in% c(2, 5)) {
			if (k == 0) {
    			if (case == 2) {
                    indep_vars <- as.matrix(cbind(1, y1lag1))
                    model0 <- stats::.lm.fit(y = dep_var, x = indep_vars) # I(0) and I(1) are identical
	   				w0 <- w1 <- aod::wald.test(b = stats::coef(model0), Sigma = vcov_custom(indep_vars, model0$residuals), Terms = 1:(k + 2))$result
	   			} else if (case == 5) {
                    indep_vars <- as.matrix(cbind(1, xlag1, y1lag1))
					model0 <- stats::.lm.fit(y = dep_var, x = indep_vars) # I(0) and I(1) are identical
	   				w0 <- w1 <- aod::wald.test(b = stats::coef(model0), Sigma = vcov_custom(indep_vars, model0$residuals), Terms = 3:(k + 3))$result
	   			}
	   		} else {
                indep_vars0 <- as.matrix(cbind(1, xlag1, y1lag1))
    			model0 <- stats::.lm.fit(y = dep_var, x = indep_vars0)
                indep_vars1 <- as.matrix(cbind(1, x1lag1, y1lag1))
    			model1 <- stats::.lm.fit(y = dep_var, x = indep_vars1)
    			if (case == 2) {
	    			w0 <- aod::wald.test(b = stats::coef(model0), Sigma = vcov_custom(indep_vars0, model0$residuals), Terms = 1:(k + 2))$result
    				w1 <- aod::wald.test(b = stats::coef(model1), Sigma = vcov_custom(indep_vars1, model1$residuals), Terms = 1:(k + 2))$result
    			} else if (case == 5) {
	    			w0 <- aod::wald.test(b = stats::coef(model0), Sigma = vcov_custom(indep_vars0, model0$residuals), Terms = 3:(k + 3))$result
    				w1 <- aod::wald.test(b = stats::coef(model1), Sigma = vcov_custom(indep_vars1, model1$residuals), Terms = 3:(k + 3))$result
    			}
    		}
            chi2_crit_val0[i] <- w0[[1]][1]
            chi2_crit_val1[i] <- w1[[1]][1]
            f_crit_val0[i] <- w0[[1]][1] / w0[[1]][2]
            f_crit_val1[i] <- w1[[1]][1] / w1[[1]][2]
		} else if (case %in% c(3, 4)) {
			if (k == 0) {
				if (case == 3) {
                    indep_vars <- as.matrix(cbind(1, y1lag1))
                    model0 <- stats::.lm.fit(y = dep_var, x = indep_vars) # I(0) and I(1) are identical
                } else if (case == 4) {
                    indep_vars <- as.matrix(cbind(1, xlag1, y1lag1))
                    model0 <- stats::.lm.fit(y = dep_var, x = indep_vars) # I(0) and I(1) are identical
                }
    			F0 <- F1 <- f_test_custom(dep_var, indep_vars, model0$residuals, const = TRUE)
			} else {
                indep_vars0 <- as.matrix(cbind(1, xlag1, y1lag1))
    			model0 <- stats::.lm.fit(y = dep_var, x = indep_vars0)
                indep_vars1 <- as.matrix(cbind(1, x1lag1, y1lag1))
    			model1 <- stats::.lm.fit(y = dep_var, x = indep_vars1)
    			F0 <- f_test_custom(dep_var, indep_vars0, model0$residuals, const = TRUE)
    			F1 <- f_test_custom(dep_var, indep_vars1, model1$residuals, const = TRUE)
			}
            chi2_crit_val0[i] <- F0$f * F0$df1
            chi2_crit_val1[i] <- F1$f * F1$df1
            f_crit_val0[i] <- F0$f
            f_crit_val1[i] <- F1$f
		}
	}

	chi2_LB <- stats::quantile(chi2_crit_val0, 1 - alpha)
	chi2_UB <- stats::quantile(chi2_crit_val1, 1 - alpha)
	chi2_bounds <- cbind(chi2_LB, chi2_UB)

    f_LB <- stats::quantile(f_crit_val0, 1 - alpha)
    f_UB <- stats::quantile(f_crit_val1, 1 - alpha)
    f_bounds <- cbind(f_LB, f_UB)

	chi2_mean0 <- mean(chi2_crit_val0)
	chi2_mean1 <- mean(chi2_crit_val1)
	chi2_bounds <- rbind(chi2_bounds, c(chi2_mean0, chi2_mean1))
	chi2_var0 <- stats::var(chi2_crit_val0)
	chi2_var1 <- stats::var(chi2_crit_val1)
	chi2_bounds <- rbind(chi2_bounds, c(chi2_var0, chi2_var1))

    f_mean0 <- mean(f_crit_val0)
    f_mean1 <- mean(f_crit_val1)
    f_bounds <- rbind(f_bounds, c(f_mean0, f_mean1))
    f_var0 <- stats::var(f_crit_val0)
    f_var1 <- stats::var(f_crit_val1)
    f_bounds <- rbind(f_bounds, c(f_var0, f_var1))

	chi2_bounds <- as.data.frame(chi2_bounds)
    f_bounds <- as.data.frame(f_bounds)

	colnames(chi2_bounds) <- c('I0', 'I1')
    colnames(f_bounds) <- c('I0', 'I1')
    rownames(chi2_bounds) <- c(paste0("a", alpha), 'Mean', 'Variance')
    rownames(f_bounds) <- c(paste0("a", alpha), 'Mean', 'Variance')

	return(list(f_bounds = f_bounds, chisq_bounds = chi2_bounds))
}

#' Critical value bounds stochastic simulation for t-bounds test for no
#' cointegration
#'
#' \code{t_bounds_sim} simulates the critical value bounds for the t-bounds test
#' for no cointegration \cite{Pesaran et al. (2001)}.
#'
#' @param case An integer (1, 3 or 5) specifying whether the 'intercept' and/or
#'   the trend' have to participate in the short-run relationship (see section
#'   'Cases' in \code{\link{bounds_t_test}}).
#' @param k The number of independent variables.
#' @param alpha A numeric vector between 0 and 1 indicating the significance
#'   level of the critical value bounds. Multiple values can be used.
#' @param T An integer indicating the number of observations.
#' @param R An integer indicating how many iterations will be used. Default is
#'   40000.
#'
#' @return \code{t_bounds_sim} returns a data frame with the critical value
#'   bounds for the t-statistic.
#' @seealso \code{\link{f_bounds_sim}} \code{\link{bounds_t_test}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal ts
#'

t_bounds_sim <- function(case, k, alpha, T, R = 40000) {
    t_crit_val0 <- c()
    t_crit_val1 <- c()

    if (case %in% c(2,4)) {
        stop("The t-statistic bounds test is not applicable under case II and case IV", call. = FALSE)
    } else if (case %in% c(1)) {
        restriction_place <- k+1
    } else if (case %in% c(3)) {
        restriction_place <- k+2
    } else if (case %in% c(5)) {
        restriction_place <- k+3
    }
    for(i in 1:R) {
        # set the independent covariates
        y1 <- cumsum(stats::rnorm(T))
        dy1 <- diff(y1, 1)
        y1lag1 <- y1[1:(T-1)]

        if (k != 0) {
            x <- matrix(stats::rnorm(T * k), T, k)
            x1 <- apply(x, 2, cumsum)
            xlag1 <- x[1:(T - 1), ]
            x1lag1 <- x1[1:(T - 1), ]
        }

        # set the deterministic linear trend
        if (case %in% c(1,3)) {
            # dataset already set
        } else if (case %in% c(5)) {
            if (k == 0) {
                xlag1 <- c(t = 1:(T - 1))
                x1lag1 <- c(t = 1:(T - 1))
            } else {
                xlag1 <- cbind(t = c(1:(T - 1)), xlag1)
                x1lag1 <- cbind(t = c(1:(T - 1)), x1lag1)
            }
        }

        if (case == 1) {
            if (k == 0) {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ y1lag1 -1))$coefficients[restriction_place, 3] # I(0) and I(1) are identical
                t_crit_val1[i] <- t_crit_val0[i]
            } else {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ xlag1 + y1lag1 -1))$coefficients[restriction_place, 3]
                t_crit_val1[i] <- summary(stats::lm(dy1 ~ x1lag1 + y1lag1 -1))$coefficients[restriction_place, 3]
            }
        } else if (case %in% c(5)) {
            if (k == 0) {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ xlag1 + y1lag1))$coefficients[restriction_place, 3] # I(0) and I(1) are identical
                t_crit_val1[i] <- t_crit_val0[i]
            } else {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ xlag1 + y1lag1))$coefficients[restriction_place, 3]
                t_crit_val1[i] <- summary(stats::lm(dy1 ~ x1lag1 + y1lag1))$coefficients[restriction_place, 3]
            }
        } else if (case %in% c(3)) {
            if (k == 0) {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ y1lag1))$coefficients[restriction_place, 3] # I(0) and I(1) are identical
                t_crit_val1[i] <- t_crit_val0[i]
            } else {
                t_crit_val0[i] <- summary(stats::lm(dy1 ~ xlag1 + y1lag1))$coefficients[restriction_place, 3]
                t_crit_val1[i] <- summary(stats::lm(dy1 ~ x1lag1 + y1lag1))$coefficients[restriction_place, 3]
            }
        }
    }

    t_LB <- stats::quantile(t_crit_val0, alpha)
    t_UB <- stats::quantile(t_crit_val1, alpha)
    t_bounds <- cbind(t_LB, t_UB)

    t_mean0 <- mean(t_crit_val0)
    t_mean1 <- mean(t_crit_val1)
    t_bounds <- rbind(t_bounds, c(t_mean0, t_mean1))
    t_var0 <- stats::var(t_crit_val0)
    t_var1 <- stats::var(t_crit_val1)
    t_bounds <- rbind(t_bounds, c(t_var0, t_var1))

    t_bounds <- as.data.frame(t_bounds)

    colnames(t_bounds) <- c('I0', 'I1')
    rownames(t_bounds) <- c(paste0("a", alpha), 'Mean', 'Variance')

    return(t_bounds)
}
