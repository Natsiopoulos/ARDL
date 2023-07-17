#' Convert dynlm model (ardl, uecm, recm) to lm model
#'
#' Takes a \code{\link[dynlm]{dynlm}} model of \code{\link[base]{class}} 'ardl',
#' 'uecm' or 'recm' and converts it into an \code{\link[stats]{lm}} model. This
#' can help using the model as a regular \code{\link[stats]{lm}} model with
#' functions that are not compatible with \code{\link[dynlm]{dynlm}} models such
#' as the \code{\link[stats]{predict}} function to forecast.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl', 'uecm' or 'recm'.
#' @param ... Currently unused argument.
#'
#' @return \code{to_lm} returns an object of \code{\link[base]{class}}
#'   \code{"lm"}.
#'
#' @seealso \code{\link{ardl}}, \code{\link{uecm}}, \code{\link{recm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords models ts
#' @export
#' @examples
#' ## Convert ARDL into lm ------------------------------------------------
#'
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' ardl_3132_lm <- to_lm(ardl_3132)
#' summary(ardl_3132)$coefficients
#' summary(ardl_3132_lm)$coefficients
#'
#' ## Convert UECM into lm ------------------------------------------------
#'
#' uecm_3132 <- uecm(ardl_3132)
#' uecm_3132_lm <- to_lm(uecm_3132)
#' summary(uecm_3132)$coefficients
#' summary(uecm_3132_lm)$coefficients
#'
#' ## Convert RECM into lm ------------------------------------------------
#'
#' recm_3132 <- recm(ardl_3132, case = 2)
#' recm_3132_lm <- to_lm(recm_3132)
#' summary(recm_3132)$coefficients
#' summary(recm_3132_lm)$coefficients
#'
#' ## Use the lm model to forecast ----------------------------------------
#'
#' # Forecast using the in-sample data
#' insample_data <- ardl_3132$model
#' head(insample_data)
#' predicted_values <- predict(ardl_3132_lm, newdata = insample_data)
#'
#' # The predicted values are expected to be the same as the fitted values
#' ardl_3132$fitted.values
#' predicted_values
#'
#' # Convert to ts class for the plot
#' predicted_values <- ts(predicted_values, start = c(1974,4), frequency=4)
#' plot(denmark$LRM, lwd=4) #The input dependent variable
#' lines(ardl_3132$fitted.values, lwd=4, col="blue") #The fitted values
#' lines(predicted_values, lty=2, lwd=2, col="red") #The predicted values

to_lm <- function(object, ...) {
    objmodel <- object$model
    dep_var <- names(object$model)[1]
    diff_var <- grepl("d(", dep_var, fixed=TRUE)
    lag_var <- grepl("L(", dep_var, fixed=TRUE)
    if (lag_var | diff_var) {
        if (lag_var) {
            y <- gsub(" ", "", dep_var) %>%
                sub("(", ".", ., fixed=TRUE) %>%
                sub(")", "", ., fixed=TRUE) %>%
                sub(",", ".", ., fixed=TRUE)
        } else if (diff_var) {
            y <- gsub(" ", "", dep_var) %>%
                sub("(", ".", ., fixed=TRUE) %>%
                sub(")", "", ., fixed=TRUE)
        }
        names(objmodel)[1] <- y
        if (attr(object$terms,"intercept") == 0) {
            formula <- formula(paste0(y, " ~ . -1"))
        } else {
            formula <- formula(paste0(y, " ~ ."))
        }
    } else {
        if (attr(object$terms,"intercept") == 0) {
            formula <- formula(paste0(dep_var, " ~ . -1"))
        } else {
            formula <- formula(paste0(dep_var, " ~ ."))
        }
    }
    return(stats::lm(formula, data = objmodel))
}
