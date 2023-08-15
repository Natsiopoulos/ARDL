#' Convert dynlm model (ardl, uecm, recm) to lm model
#'
#' Takes a \code{\link[dynlm]{dynlm}} model of \code{\link[base]{class}} 'ardl',
#' 'uecm' or 'recm' and converts it into an \code{\link[stats]{lm}} model. This
#' can help using the model as a regular \code{\link[stats]{lm}} model with
#' functions that are not compatible with \code{\link[dynlm]{dynlm}} models such
#' as the \code{\link[stats]{predict}} function to forecast.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl', 'uecm' or 'recm'.
#' @param fix_names A logical, indicating whether the variable names should be
#' rewritten without special functions and character in the names such as "d()"
#' or "L()". When \code{fix_names = TRUE}, the characters "(", and "," are
#' replaces with ".", and ")" and spaces are deleted. The name of the dependent
#' variable is always transformed, regardless of the value of this parameter.
#' Default is FALSE.
#' @param data_class If "ts", it converts the data class to
#' \code{\link[stats]{ts}} (see examples for its usage). The default is
#' \code{\link[base]{NULL}}, which uses the same data provided in the original
#' object.
#' @param ... Currently unused argument.
#'
#' @return \code{to_lm} returns an object of \code{\link[base]{class}}
#' \code{"lm"}.
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
#'
#' ## Convert to lm for post-estimation testing ---------------------------
#'
#' # Ramsey's RESET test for functional form
#' library(lmtest) # for resettest()
#' library(strucchange) # for efp(), and sctest()
#'
#' \dontrun{
#'     # This produces an error.
#'     # resettest() cannot use data of class 'zoo' such as the 'denmark' data
#'     # used to build the original model
#'     resettest(uecm_3132, type = c("regressor"))
#' }
#'
#' uecm_3132_lm <- to_lm(uecm_3132, data_class = "ts")
#' resettest(uecm_3132_lm, power = 2)
#'
#' # CUSUM test for structural change detection
#' \dontrun{
#'     # This produces an error.
#'     # efp() does not understand special functions such as "d()" and "L()"
#'     efp(uecm_3132$full_formula, data = uecm_3132$model)
#' }
#'
#' uecm_3132_lm_names <- to_lm(uecm_3132, fix_names = TRUE)
#' fluctuation <- efp(uecm_3132_lm_names$full_formula,
#'                    data = uecm_3132_lm_names$model)
#' sctest(fluctuation)
#' plot(fluctuation)
#'

to_lm <- function(object, fix_names = FALSE, data_class = NULL, ...) {
    objmodel <- object$model
    if (!is.null(data_class)) {
        if (data_class == "ts") {
            objmodel <- stats::ts(objmodel, start = stats::start(objmodel[,1]), frequency = stats::frequency(objmodel[,1]))
        }
    }
    dep_var <- colnames(objmodel)[1]
    fix_names_fun <- function(text) {
        text <- gsub(" ", "", text) %>%
            gsub("(", ".", ., fixed = TRUE) %>%
            gsub(")", "", ., fixed = TRUE) %>%
            gsub(",", ".", ., fixed = TRUE)
        return(text)
    }
    y <- fix_names_fun(dep_var)

    if (fix_names) {
        colnames(objmodel) <- sapply(colnames(objmodel), fix_names_fun)
        if (attr(object$terms,"intercept") == 0) {
            formula <- formula(paste0(y, " ~ . -1"))
        } else {
            formula <- formula(paste0(y, " ~ ."))
        }

        full_formula <- as.character(object$full_formula)
        lm_model <- stats::lm(formula, data = objmodel)
        lm_model$full_formula <- stats::formula(paste0(y, "~", fix_names_fun(full_formula[3])))

        return(lm_model)
    } else {
        colnames(objmodel)[1] <- y
        if (attr(object$terms,"intercept") == 0) {
            formula <- formula(paste0(y, " ~ . -1"))
        } else {
            formula <- formula(paste0(y, " ~ ."))
        }

        return(stats::lm(formula, data = objmodel))
    }
}
