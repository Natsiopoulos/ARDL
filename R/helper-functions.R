AIC_pss <- function(model){
    # maximized log-likelihood value of the model
    LLp <- stats::logLik(model)
    # number of freely estimated coefficients
    sp <- length(model$coefficients)
    LLp - sp
}
