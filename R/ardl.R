#' ARDL model regression
#'
#' A simple way to construct complex ARDL specifications providing just the
#' model order additional to the model formula. It uses
#' \code{\link[dynlm]{dynlm}} under the hood.
#'
#' The \code{formula} should contain only variables that exist in the data
#' provided through \code{data} plus some additional functions supported by
#' \code{\link[dynlm]{dynlm}} (i.e., \code{trend()}).
#'
#' You can also specify fixed variables that are not supposed to be lagged (e.g.
#' dummies etc.) simply by placing them after \code{|}. For example, \code{y ~
#' x1 + x2 | z1 + z2} where \code{z1} and \code{z2} are the fixed variables and
#' should not be considered in \code{order}. Note that the \code{|} notion
#' should not be confused with the same notion in \code{dynlm} where it
#' introduces instrumental variables.
#'
#' @param formula A "formula" describing the linear model. Details for model
#'   specification are given under 'Details'.
#' @param data A time series object (e.g., "ts", "zoo" or "zooreg") or a data
#'   frame containing the variables in the model. In the case of a data frame,
#'   it is coerced into a \code{\link[stats]{ts}} object with \code{start = 1},
#'   \code{end = nrow(data)} and \code{frequency = 1}. If not found in data, the
#'   variables are NOT taken from any environment.
#' @param order A specification of the order of the ARDL model. A numeric vector
#'   of the same length as the total number of variables (excluding the fixed
#'   ones, see 'Details'). It should only contain positive integers or 0. An
#'   integer could be provided if all variables are of the same order.
#' @param start Start of the time period which should be used for fitting the
#'   model.
#' @param end End of the time period which should be used for fitting the model.
#' @param ... Additional arguments to be passed to the low level regression
#'   fitting functions.
#'
#' @return \code{ardl} returns an object of \code{\link[base]{class}}
#'   \code{c("dynlm", "lm", "ardl")}. In addition, attributes 'order', 'data',
#'   'parsed_formula' and 'full_formula' are provided.
#'
#' @section Mathematical Formula:
#' The general form of an \eqn{ARDL(p,q_{1},\dots,q_{k})}{ARDL(p,q1,...,qk)} is:
#' \deqn{y_{t} = c_{0} + c_{1}t + \sum_{i=1}^{p}b_{y,i}y_{t-i} +
#' \sum_{j=1}^{k}\sum_{l=0}^{q_{j}}b_{j,l}x_{j,t-l} + \epsilon_{t}}
#'
#' @seealso \code{\link{uecm}}, \code{\link{recm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords models ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## Estimate an ARDL(3,1,3,2) model -------------------------------------
#'
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' summary(ardl_3132)
#'
#' ## Add dummies or other variables that should stay fixed ---------------
#'
#' d_74Q1_75Q3 <- ifelse(time(denmark) >= 1974 & time(denmark) <= 1975.5, 1, 0)
#'
#' # the date can also be setted as below
#' d_74Q1_75Q3_ <- ifelse(time(denmark) >= "1974 Q1" & time(denmark) <= "1975 Q3", 1, 0)
#' identical(d_74Q1_75Q3, d_74Q1_75Q3_)
#' den <- cbind(denmark, d_74Q1_75Q3)
#' ardl_3132_d <- ardl(LRM ~ LRY + IBO + IDE | d_74Q1_75Q3,
#'                     data = den, order = c(3,1,3,2))
#' summary(ardl_3132_d)
#' compare <- data.frame(AIC = c(AIC(ardl_3132), AIC(ardl_3132_d)),
#'                       BIC = c(BIC(ardl_3132), BIC(ardl_3132_d)))
#' rownames(compare) <- c("no ummy", "with dummy")
#' compare
#'
#' ## Estimate an ARDL(3,1,3,2) model with a linear trend -----------------
#'
#' ardl_3132_tr <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM),
#'                      data = denmark, order = c(3,1,3,2))
#'
#' # Alternative time trend specifications:
#' # time(LRM)                 1974 + (0, 1, ..., 55)/4 time(data)
#' # trend(LRM)                (1, 2, ..., 55)/4        (1:n)/freq
#' # trend(LRM, scale = FALSE) (1, 2, ..., 55)          1:n
#'
#' ## Subsample ARDL regression (start after 1975 Q4) ---------------------
#'
#' ardl_3132_sub <- ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                       order = c(3,1,3,2), start = "1975 Q4")
#'
#' # the date can also be setted as below
#' ardl_3132_sub2 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                        order = c(3,1,3,2), start = c(1975,4))
#' identical(ardl_3132_sub, ardl_3132_sub2)
#' summary(ardl_3132_sub)
#'
#' ## Ease of use ---------------------------------------------------------
#'
#' # The model specification of the ardl_3132 model can be created as easy as order=c(3,1,3,2)
#' # or else, it could be done using the dynlm package as:
#' library(dynlm)
#' m <- dynlm(LRM ~ L(LRM, 1) + L(LRM, 2) + L(LRM, 3) + LRY + L(LRY, 1) + IBO + L(IBO, 1) +
#'            L(IBO, 2) + L(IBO, 3) + IDE + L(IDE, 1) + L(IDE, 2), data = denmark)
#' identical(m$coefficients, ardl_3132$coefficients)
#'
#' # The full formula can be extracted from the ARDL model, and this is equal to
#' ardl_3132$full_formula
#' m2 <- dynlm(ardl_3132$full_formula, data = ardl_3132$data)
#' identical(m$coefficients, m2$coefficients)

ardl <- function(formula, data, order, start = NULL, end = NULL, ...) {
    if (!any(c("ts", "zoo", "zooreg") %in% class(data))) {
        data <- stats::ts(data, start = 1, end = nrow(data), frequency = 1)
    }

    parsed_formula <- parse_formula(formula = formula, colnames_data = colnames(data))
    order <- parse_order(orders = order, order_name = "order", kz = parsed_formula$kz)
    ardl_formula <- build_ardl_formula(parsed_formula = parsed_formula, order = order)

    full_formula <- formula(ardl_formula$full)
    ardl_model <- dynlm::dynlm(full_formula, data = data, start = start, end = end, ...)
    # for model compatibility in the global env
    attr(ardl_model$terms, ".Environment") <- .GlobalEnv
    attr(attr(ardl_model$model, "terms"), ".Environment") <- .GlobalEnv
    attr(full_formula, ".Environment") <- .GlobalEnv

    ardl_model$order <- order
    ardl_model$data <- data
    ardl_model$parsed_formula <- parsed_formula
    ardl_model$full_formula <- full_formula

    attr(ardl_model, "class") <- c(class(ardl_model), "ardl")

    return(ardl_model)
}
