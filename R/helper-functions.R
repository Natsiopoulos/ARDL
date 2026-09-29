AIC_pss <- function(model){
    # maximized log-likelihood value of the model
    LLp <- stats::logLik(model)
    # number of freely estimated coefficients
    sp <- length(model$coefficients)
    c(LLp - sp)
}
# First estimation date so every candidate in the search can be fit on the
# same sample. order_caps is the highest lag allowed for each variable
# (max_order, or fixed_order where that lag is held fixed). User start/end
# tighten the window; they never move the start earlier than this burn-in.
balanced_estimation_start <- function(data, order_caps, start = NULL, end = NULL) {
    M <- max(order_caps)
    n <- NROW(data)
    if (M + 1L > n) {
        stop("Not enough observations for a balanced sample. The longest lag in the search is ",
             M, " but the data have ", n, " rows.", call. = FALSE)
    }
    idx <- zoo::index(data)
    i_bal <- M + 1L
    if (is.null(start)) {
        i_user <- 1L
    } else {
        w <- stats::window(data, start = start)
        if (NROW(w) == 0) {
            stop("'start' is after the end of the sample.", call. = FALSE)
        }
        i_user <- match(zoo::index(w)[1], idx)
        if (is.na(i_user)) {
            stop("Could not match 'start' to the data index.", call. = FALSE)
        }
    }
    i_est <- max(i_user, i_bal)
    if (!is.null(end)) {
        w_end <- stats::window(data, end = end)
        if (NROW(w_end) == 0) {
            stop("'end' is before the start of the sample.", call. = FALSE)
        }
        i_end <- match(zoo::index(w_end)[NROW(w_end)], idx)
        if (is.na(i_end) || i_est > i_end) {
            stop("The balanced sample starts after 'end'.", call. = FALSE)
        }
    }
    idx[i_est]
}

BIC_pss <- function(model){
    # maximized log-likelihood value of the model
    LLp <- stats::logLik(model)
    # number of freely estimated coefficients
    sp <- length(model$coefficients)
    # number of observations
    TT <- log(stats::nobs(model))
    c(LLp - (sp/2)*TT)
}
