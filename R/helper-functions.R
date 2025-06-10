AIC_pss <- function(model){
    # maximized log-likelihood value of the model
    LLp <- stats::logLik(model)
    # number of freely estimated coefficients
    sp <- length(model$coefficients)
    c(LLp - sp)
}
BIC_pss <- function(model){
    # maximized log-likelihood value of the model
    LLp <- stats::logLik(model)
    # number of freely estimated coefficients
    sp <- length(model$coefficients)
    # number of observations
    TT <- ln(nobs(model))
    c(LLp - (sp/2)*TT)
}
