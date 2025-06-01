#' Predict method for ARDL model fits
#'
#' Makes predictions based on fitted ARDL models, extending the 
#' generic function \code{\link[stats]{predict}} for use with 
#' \code{\link{ardl}}. It is designed to take advantage of lags of the dependent
#' variable (AR components) from the fitted model and new values of the 
#' independent variable(s) to calculate predictions.
#' 
#' @param object A fitted model of \code{\link[base]{class}} \code{\link{ardl}}.
#' @param newdata A time series object (\code{\link[stats]{ts}},
#' \code{\link[zoo]{zoo}}, \code{\link[zoo]{zooreg}}) or a 
#' \code{\link[base]{data.frame}} containing the new values of the independent 
#' variables \eqn{x} required to make predictions.
#' @param ... further arguments passed to or from other methods.
#' 
#' @details
#' \code{predict.ardl} works recursively to calculate predictions based on
#' \eqn{y_{t-1}, ..., y_{t-p}} values contained within a fitted model, and new 
#' values of \eqn{x_{T+1}, ..., x_{T+h}}, where \eqn{T} denotes the last observation 
#' of \eqn{t} from the original data and \eqn{h} is the number of new 
#' observations that will be used to calculate predictions. These are supplied 
#' through the \code{newdata} argument. It is important to note that for a given 
#' prediction \eqn{\hat{y}_{T+1}}, the independent variables provided through 
#' \code{newdata}, are expected to follow consecutively from the last observation
#' \eqn{x_{T}} used to create the model, i.e. \eqn{x_{T+1}, ..., x_{T+h}},
#' with no gaps. If a time series object is passed to the function, the frequency
#' and whether the observations follow consecutively will be checked. 
#' 
#' @section Calculation example:
#' 
#' We want to predict \eqn{\hat{y}_{T+1}} given an \eqn{ARDL(2,2)} model:
#' 
#' \deqn{
#'      \hat{y}_{T+1} = Z \boldsymbol{\beta}
#' }
#' where \eqn{Z} is our design matrix and \eqn{\beta} is a vector of our model 
#' coefficients. We need 2 lags for our dependent and independent variables:
#' \eqn{y_T}, \eqn{y_{T-1}} and \eqn{X_T}, \eqn{X_{T-1}}.
#' Plus, \eqn{X_{T+1}} (from \code{newdata}), which in our design matrix is 
#' equivalent to time \eqn{t} in our original model:
#' \deqn{
#' Z =
#'     \left[
#'         \begin{array}{c|cccccc}
#'         & \mathbf{c} & \mathbf{y_{t-1}} & \mathbf{y_{t-2}} & \mathbf{x_t} &
#'         \mathbf{x_{t-1}} & \mathbf{x_{t-2}} \\
#'         \hline
#'         T\!+\!1 & 1 & y_{T} & y_{T-1} & x_{T+1} & x_{T} & x_{T-1}
#'         \end{array}
#'         \right]
#' }
#' We proceed to the next iteration, predicting \eqn{\hat{y}_{T+2}}, using the
#' previously calculated value of \eqn{\hat{y}_{T+1}}:
#' \deqn{
#' Z =
#' \left[
#'     \begin{array}{c|cccccc}
#'     & \mathbf{c} & \mathbf{y_{t-1}} & \mathbf{y_{t-2}} & \mathbf{x_t} & \mathbf{x_{t-1}} & \mathbf{x_{t-2}} \\
#'     \hline
#'     T\!+\!2 & 1 & y_{T+1} & y_{T} & x_{T+2} & x_{T+1} & x_{T}
#'     \end{array}
#'     \right]
#' }
#' 
#' Iteration continues until we've calculated all the necessary values of
#' \eqn{\hat{y}_{T+h}} for \code{newdata}.
#'  
#' @return \code{predict.ardl} returns values for \eqn{\hat{y}_{T+1}, ..., 
#' \hat{y}_{T+h}} equal in length to the number of rows in \code{newdata}, 
#' as a \code{\link[base]{vector}}.
#' 
#' @seealso \code{\link{ardl}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @author Daniel Finnan, \email{dan@@custom-made.org.uk}
#' @keywords models ts
#' @examples
#' data(denmark)
#' ## Estimate an ARDL(2,1,2,1,1) model, using a subset of the denmark data
#' ardl_21211 <- ardl(LRM ~ LRY + LPY + IBO + IDE, data = denmark,
#'                    order = c(2,1,2,1,1),
#'                    start = "1974 Q1", end = "1986 Q2")
#' ## Make predictions based on the remainder of the denmark data not used in 
#' ## training the model
#' predict_data <- window(denmark, start = "1986 Q3", end = "1987 Q3")
#' ## Drop the dependent variable, since we're predicting it
#' predict_data <- predict_data[, c("LRY", "LPY", "IBO", "IDE")]
#' predictions <- predict.ardl(ardl_21211, predict_data)
#' 
#' ## Estimate a ARDL(4,4,4,4) model with a linear trend and dummies
#' ## Create dummies
#' d_74Q1_75Q3 <- ifelse(time(denmark) >= "1974 Q1" & time(denmark) <= "1975 Q3", 1, 0)
#' # Add them to the data
#' denmark_dums <- cbind(denmark, d_74Q1_75Q3)
#' ## Estimate the model
#' ardl_4444 <- ardl(LRM ~ LRY + LPY + IBO + trend(LRM) | d_74Q1_75Q3,
#'                   data = denmark_dums, 
#'                   order = c(4,4,4,4),
#'                   start = "1974 Q1", end = "1986 Q2")
#' ## Take the remaining data not used in training the model
#' predict_data <- window(denmark_dums, start = "1986 Q3", end = "1987 Q3")
#' ## Drop the dependent variable
#' predict_data <- predict_data[, c("LRY", "LPY", "IBO", "d_74Q1_75Q3")]
#' ## Compute the predictions
#' predictions <- predict.ardl(ardl_4444, predict_data)
#'
#' @method predict ardl
#' @export
predict.ardl <- function(object, newdata, ...) {
    newdata_class <- class(newdata)
    if (missing(newdata)) {
        stop("Need to provide some 'newdata' to predict.", call. = FALSE)
    }
    if (!any(c("ts", "zoo", "zooreg", "data.frame") %in% newdata_class)) {
        stop("'newdata' must be a 'ts', 'zoo', 'zooreg' or 'data.frame' object.", call. = FALSE)
    }
    if (!identical(class(object), c("dynlm","lm", "ardl"))) {
        stop("'object' needs to be a fitted model using 'ardl' method.", call. = FALSE)
    }
    if (any(is.na(newdata))) {
        stop("'newdata' contains some NA values.", call. = FALSE)
    }
    if ((any(c("ts", "zoo", "zooreg") %in% newdata_class) && (frequency(object$data) != frequency(newdata)))) {
        stop("'newdata' needs to have the same frequency as the original data.", call. = FALSE)
    }
    # Consecutive series zoo/zooreg
    # Note: we need to subset data in ardl model, since it stores complete dataset, despite start/end parameter
    if (any(c("zoo", "zooreg") %in% newdata_class) && !((end(object$data[index(object),]) + deltat(object$data[index(object),])) == start(newdata))) {
        stop("Observations in 'newdata' need to follow original data consecutively.", call. = FALSE)
    }
    # Consecutive series ts. Note: ts can't use delta date function, for simplisicity we coerce to zoo
    if ( is.ts(newdata) && !((end(as.zoo(object$data[index(object),])) + deltat(as.zoo(object$data[index(object),]))) == start(as.zoo(newdata))) ) {
        stop("Observations in 'newdata' need to follow original data consecutively.", call. = FALSE)
    }
    # Check all necessary columns are in new data
    orig_cols <- c(object$parsed_formula$x_part$var, object$parsed_formula$fixed_part$var)
    newdata_cols <- colnames(newdata)
    if (identical(orig_cols, newdata_cols) == FALSE) {
        stop("'newdata' columns don't match original data.", call. = FALSE)
    }
    # Handling simple data.frames
    if (!any(c("ts", "zoo", "zooreg") %in% newdata_class)) {
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
    # Return data.frame if newdata isn't a time series object, otherwise return
    # time series object with timestamps
    if (!any(c("ts", "zoo", "zooreg") %in% newdata_class)) {
        return(data.frame(predictions = predictor))
    } else {
        return(cbind(newdata, predictor) %>% .[,dim(.)[2]])
    }
}