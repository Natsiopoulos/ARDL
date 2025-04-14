#' ARDL prediction
#'
#' Makes predictions based on fitted ARDL models, extending the 
#' \code{\link[stats]{predict}} generic function for the 
#' \code{\link[ARDL-package]}. It is designed to take advantage of lags of the 
#' dependent variable from the fitted model and new values of the independent
#' variable(s) to calculate predictions.
#' 
#' @param object A fitted 'ardl' model of \code{\link[base]{class}} 'ardl',
#' created using \code{ardl}.
#' @param newdata A time series object (e.g., "ts", "zoo" or "zooreg") or a 
#' data frame containing the new values of \eqn{x} required to make 
#' predictions.
#' @param se.fit A switch indicating if standard errors are required.
#' 
#' @return \code{predict} returns predictions of \eqn{y}.
#' 
#' @section Calculation:
#' Details of the calculation, Kleanthis' matrices detail
#'
#' @seealso \code{\link{ardl}}
#' @keywords models ts
#' @examples
#' ## Prediction example -----
#'
#' # Indirectly from an ARDL
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' recm_3132 <- recm(ardl_3132, case = 2)
#' @importFrom stats predict  
#' @method predict ardl
#' @export
predict.ardl <- function(object, newdata, se.fit = FALSE, ...) {
    # Handling simple data.frames
    if (!any(c("ts", "zoo", "zooreg") %in% class(newdata))) {
        newdata <- stats::ts(newdata, start = 1, end = nrow(newdata), 
                             frequency = 1)
    }
    # Add check for empty cells in newdata
    kx <- object$parsed_formula$kx
    kw <- object$parsed_formula$kw
    kfixed <- object$parsed_formula$kfixed
    coeffs <- matrix(object$coefficients)
    #Because when no intercept, object$parsed_formula$w_part$var was returning "-1" instead of NULL
    if (object$parsed_formula$kw == 0) {
        w_part <- NULL
    } else {
        w_part <- object$parsed_formula$w_part$var
    }
    model_matrix <- object$model[,c(object$parsed_formula$y_part$var,
                                    w_part,
                                    object$parsed_formula$x_part$var,
                                    object$parsed_formula$fixed_part$var)]
    model_matrix <- t(matrix(unlist(model_matrix), nrow = ncol(model_matrix), 
                             byrow = TRUE, dimnames = dimnames(t(model_matrix))))
    n_ahead <- nrow(newdata)
    if (kw == 2) {
        olddata_w <- zoo::coredata(object$model[,2])
        newdata_w <- cumsum(c(olddata_w[length(olddata_w)], 
                              rep(diff(olddata_w, 1)[1], n_ahead)))[-1]
        olddata_w <- object$model[,2]
        newdata_w <- ts(newdata_w, start = end(olddata_w) + 1/frequency(olddata_w), 
                        frequency = frequency(olddata_w))
    }
    predictor <- c()
    model_matrix_y <- model_matrix[,1]
    kw_correction <- if(object$parsed_formula$kw == 2){c(-1,-2)} else {-1}
    model_matrix_x <- model_matrix[,kw_correction]
    model_matrix_x <- model_matrix_x[,1:kx]
    for (j in 1:n_ahead) {
        Y <- c()
        design_matrix <- c()
        for (i in 0:c(object$order[1]-1)) {
            lags_y <- dplyr::lag(model_matrix_y, n = i)
            Y <- c(Y, lags_y[length(lags_y)])
        }
        for (k in 1:kx) {
            Xk <- c()
            for (i in 0:object$order[k+1]) { 
                model_matrix_xk <- c(model_matrix_x[,k], newdata[1:j,k])
                lags_x <- dplyr::lag(model_matrix_xk, n = i)
                Xk <- c(Xk, lags_x[length(lags_x)])
            }
            design_matrix <- c(design_matrix, Xk)
        }
        if (kfixed != 0) {
            design_matrix <- c(design_matrix, 
                               unlist(newdata[j,(kx+1):ncol(newdata)]))#Add newdata for dummies
        }
        design_matrix <- c(Y, design_matrix)
        if (kw == 2) {
            design_matrix <- c(1, newdata_w[j], design_matrix)#Add unit vector
        } else if (kw == 1) {
            design_matrix <- c(1, design_matrix)#Add unit vector and trend
        }
        predictor <- c(predictor, t(coeffs) %*% matrix(design_matrix))
        model_matrix_y <- c(model_matrix_y, predictor[length(predictor)])
    }
    # Return as a named num like predict.lm
    return(setNames(predictor, seq(1, n_ahead)))
}
# 
# predict.ardl(object, newdata)