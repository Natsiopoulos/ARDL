#' ARDL prediction
#'
#' Makes predictions based on fitted ARDL models, extending the 
#' generic function \code{\link[stats]{predict}} for use with 
#' \code{\link{ardl}}. It is designed to take advantage of lags of the 
#' dependent variable from the fitted model and new values of the independent 
#' variable(s) to calculate predictions.
#' 
#' @param object A fitted model of \code{\link[base]{class}} \code{ardl}.
#' @param newdata A time series object (\code{\link[stats]{ts}},
#' \code{\link[zoo]{zoo}} or \code{\link[zoo]{zooreg}}) or a 
#' \code{\link[base]{data.frame}} containing the new values of \eqn{x} required
#' to make predictions.
#' 
#' @details
#' \code{predict.ardl} works recursively to calculate predictions based on \eqn{y_{t-p}} values
#' contained within a fitted model, and new values of \eqn{x} supplied through the
#' \code{newdata} argument. It supports the 5 different deterministic cases,
#' involving different restrictions on the intercept and/or trend
#' (see \code{\link{recm}}), as well as fixed regressors (e.g. dummies). It is 
#' important to note that for a given prediction \eqn{\hat{y_t}}, the 
#' independent variables provided through \code{newdata}, are expected to follow 
#' consecutively, i.e. \eqn{x_t}, \eqn{x_{t+1}}, \eqn{x_{t+2}}, with no gaps.
#' If a time series object is passed to the function, and the underlying 
#' \code{ardl} model contains a trend (Case 4 & 5 under PSS), the timestamps 
#' defined in \code{newdata} will be ignored. The trend will be constructed from
#' the existing \code{ardl} model.
#' 
#' @section Calculation
#'  
#'  
#' @return \code{predict.ardl} returns values for \eqn{\hat{y_t}} equal in length
#' to the number of rows in \code{newdata}, as a \code{\link[base]{vector}}.
#' 
#' @seealso \code{\link{ardl}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @author Daniel Finnan, \email{dan@@custom-made.org.uk}
#' @keywords models ts
#' @examples
#' data(denmark)
#' ## Estimate an ARDL(2,1,2,1,1) model, using a subset of the denmark data
#' ardl_21211 <- ardl(LRM ~ LRY + LPY + IBO + IDE, data = denmark,
#'                   order = c(2,1,2,1,1),
#'                   start = "1974 Q1", end = "1986 Q2")
#' ## Make predictions based on the remainder of the denmark data not used in 
#' ## training the model
#' predict_data <- window(denmark, start = "1986 Q3", end = "1987 Q3")
#' ## Drop the dependent variable, since we're predicting it
#' predict_data <- predict_data[, c("LRY", "LPY", "IBO", "IDE")]
#' predictions <- predict.ardl(ardl_21211, predict_data)
#'
#' ## Estimate a ARDL(4,4,4,4) model with a linear trend and dummies
#' ## Create dummies
#' d_74Q1_75Q3_ <- ifelse(time(denmark) >= "1974 Q1" & time(denmark) <= "1975 Q3", 1, 0)
#' # Add them to the data
#' denmark <- cbind(denmark, d_74Q1_75Q3)
#' ## Estimate the model
#' ardl_4444 <- ardl(LRM ~ LRY + LPY + IBO + trend(LRM) | d_74Q1_75Q3,
#'                  data = denmark, 
#'                  order = c(4,4,4,4),
#'                  start = "1974 Q1", end = "1986 Q2")
#' ## Take the remaining data not used in training the model
#' predict_data <- window(denmark, start = "1986 Q3", end = "1987 Q3")
#' ## Drop the dependent variable
#' predict_data <- predict_data[, c("LRY", "LPY", "IBO", "d_74Q1_75Q3")]
#' ## Compute the predictions
#' predictions <- predict.ardl(ardl_4444, predict_data)
#'
#' @importFrom stats predict  
#' @method predict ardl
#' @export
predict.ardl <- function(object, newdata, ...) {
    if (missing(newdata)) {
        stop("Need to provide some 'newdata' to predict.", call. = FALSE)
    }
    if (!identical(class(object), c("dynlm","lm", "ardl"))) {
        stop("'object' needs to be a fitted model using 'ardl' method.", call. = FALSE)
    }
    if (any(is.na(newdata))) {
        stop("'newdata' contains some NA values.", call. = FALSE)
    }
    # Check all necessary columns are in new data
    orig_cols <- c(object$parsed_formula$x_part$var, object$parsed_formula$fixed_part$var)
    newdata_cols <- colnames(newdata)
    if (identical(orig_cols, newdata_cols) == FALSE) {
        stop("'newdata' columns don't match original data.", call. = FALSE)
    }
    # Handling simple data.frames
    if (!any(c("ts", "zoo", "zooreg") %in% class(newdata))) {
        newdata <- stats::ts(newdata, start = 1, end = nrow(newdata), 
                             frequency = 1)
    }
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