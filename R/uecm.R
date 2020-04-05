#' Unrestricted ECM regression
#'
#' \code{uecm} is a generic function used to construct Unrestricted Error
#' Correction Models (UECM). The function invokes two different
#' \code{\link[utils]{methods}}. The default method works exactly like
#' \code{\link{ardl}}. The other method requires an object of
#' \code{\link[base]{class}} 'ardl'. Both methods create the conditional UECM,
#' which is the UECM of the underlying ARDL.
#'
#' @inherit ardl details
#'
#' @return \code{uecm} returns an object of \code{\link[base]{class}}
#'   \code{c("dynlm", "lm", "uecm")}. In addition, attributes 'order', 'data',
#'   'parsed_formula' and 'full_formula' are provided.
#'
#' @section Mathematical Formula: The formula of an Unrestricted ECM conditional
#'   to an \eqn{ARDL(p,q_{1},\dots,q_{k})}{ARDL(p,q1,...,qk)} is: \deqn{\Delta
#'   y_{t} = c_{0} + c_{1}t + \pi_{y}y_{t-1} + \sum_{j=1}^{k}\pi_{j}x_{j,t-1} +
#'   \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} +
#'   \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} +
#'   \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \epsilon_{t}}
#'
#' @seealso \code{\link{ardl}} \code{\link{recm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords models ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## Estimate the UECM, conditional to it's underlying ARDL(3,1,3,2) -----
#'
#' # Indirectly
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' uecm_3132 <- uecm(ardl_3132)
#'
#' # Directly
#' uecm_3132_ <- uecm(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' identical(uecm_3132, uecm_3132_)
#' summary(uecm_3132)

uecm <- function(...) {
    UseMethod("uecm")
}

#' @rdname uecm
#'
#' @param object An object of \code{\link[base]{class}} 'ardl'.
#'
#' @export
#'

uecm.ardl <- function(object, ...) {

    parsed_formula <- object$parsed_formula
    order <- object$order
    data <- object$data

    start <- start(object)
    end <- end(object)

    uecm_formula <- build_uecm_formula(parsed_formula = parsed_formula, order = order)
    full_formula <- stats::formula(uecm_formula$full)

    uecm_model <- dynlm::dynlm(full_formula, data = data, start = start, end = end)
    # for model compatibility in the global env
    attr(uecm_model$terms, ".Environment") <- .GlobalEnv
    attr(attr(uecm_model$model, "terms"), ".Environment") <- .GlobalEnv
    attr(full_formula, ".Environment") <- .GlobalEnv

    uecm_model$order <- order
    uecm_model$data <- data
    uecm_model$parsed_formula <- parsed_formula
    uecm_model$full_formula <- full_formula

    attr(uecm_model, "class") <- c(class(uecm_model), "uecm")

    return(uecm_model)
}

#' @rdname uecm
#'
#' @param order A specification of the order of the underlying ARDL model (e.g.,
#'   for the UECM of an ARDL(1,0,2) model it should be \code{order = c(1,0,2)}).
#'   A numeric vector of the same length as the total number of variables
#'   (excluding the fixed ones, see 'Details'). It should only contain positive
#'   integers or 0. An integer could be provided if all variables are of the
#'   same order.
#' @inheritParams ardl
#'
#' @export
#'

uecm.default <- function(formula, data, order, start = NULL, end = NULL, ...) {

    if (!any(c("ts", "zoo", "zooreg") %in% class(data))) {
        data <- stats::ts(data, start = 1, end = nrow(data), frequency = 1)
    }
    parsed_formula <- parse_formula(formula = formula, colnames_data = colnames(data))
    order <- parse_order(orders = order, order_name = "order", kz = parsed_formula$kz)

    uecm_formula <- build_uecm_formula(parsed_formula = parsed_formula, order = order)
    full_formula <- stats::formula(uecm_formula$full)

    uecm_model <- dynlm::dynlm(full_formula, data = data, start = start, end = end, ...)
    # for model compatibility in the global env
    attr(uecm_model$terms, ".Environment") <- .GlobalEnv
    attr(attr(uecm_model$model, "terms"), ".Environment") <- .GlobalEnv
    attr(full_formula, ".Environment") <- .GlobalEnv

    uecm_model$order <- order
    uecm_model$data <- data
    uecm_model$parsed_formula <- parsed_formula
    uecm_model$full_formula <- full_formula

    attr(uecm_model, "class") <- c(class(uecm_model), "uecm")

    return(uecm_model)
}
