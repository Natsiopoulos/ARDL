#' Cointegrating equation (long-run level relationship)
#'
#' Creates the cointegrating equation (long-run level relationship) providing an
#' 'ardl', 'uecm' or 'recm' model.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl', 'uecm' or
#'   'recm'.
#' @param case An integer from 1-5 or a character string specifying whether the
#'   'intercept' and/or the 'trend' have to participate in the long-run level
#'   relationship (cointegrating equation) (see section 'Cases' below). If the
#'   input object is of class 'recm', \code{case} is not needed as the model is
#'   already under a certain case.
#' @param ... Currently unused argument.
#'
#' @return \code{coint_eq} returns an numeric vector containing the
#'   cointegrating equation.
#'
#' @inheritSection recm Cases
#' @inheritSection recm References
#' @seealso \code{\link{ardl}} \code{\link{uecm}} \code{\link{recm}}
#'   \code{\link{bounds_f_test}} \code{\link{bounds_t_test}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords ts
#' @export
#' @examples
#' data(denmark)
#' library(zoo) # for cbind.zoo()
#'
#' ## Estimate the Cointegrating Equation of an ARDL(3,1,3,2) model -------
#'
#' # From an ARDL model (under case 2, restricted constant)
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' ce2_ardl <- coint_eq(ardl_3132, case = 2)
#'
#' # From an UECM (under case 2, restricted constant)
#' uecm_3132 <- uecm(ardl_3132)
#' ce2_uecm <- coint_eq(uecm_3132, case = 2)
#'
#' # From a RECM (under case 2, restricted constant)
#' # Notice that if a RECM has already been estimated under a certain case,
#' # the 'coint_eq()' can't be under different case, so no 'case' argument needed.
#' recm_3132 <- recm(uecm_3132, case = 2)
#' # The RECM is already under case 2, so the 'case' argument is no needed
#' ce2_recm <- coint_eq(recm_3132)
#'
#' identical(ce2_ardl, ce2_uecm, ce2_recm)
#'
#' ## Check for a degenerate level relationship ---------------------------
#'
#' # The bounds F-test under both cases reject the Null Hypothesis of no level relationship.
#' bounds_f_test(ardl_3132, case = 2)
#' bounds_f_test(ardl_3132, case = 3)
#'
#' # The bounds t-test also rejects the NUll Hypothesis of no level relationship.
#' bounds_t_test(ardl_3132, case = 3)
#'
#' # But when the constant enters the long-run equation (case 3)
#' # this becomes a degenerate relationship.
#' ce3_ardl <- coint_eq(ardl_3132, case = 3)
#' den <- cbind.zoo(LRM = denmark[,"LRM"], ce2_ardl, ce3_ardl)
#'
#' if (requireNamespace("xts", quietly = TRUE)) {
#'
#' library(xts)
#' den <- xts(den)
#' plot(den, legend.loc = "right")
#' plot(den[,-3], legend.loc = "right")
#'
#' } else {
#'
#' plot(den, col = c(1,2,3), screens = 1)
#' legend("right", lty = 1, legend = colnames(den), col = c(1:3))
#' plot(den[,-3], col = c(1,2), screens = 1)
#' legend("top", lty = 1, legend = colnames(den[,-3]), col = c(1:2))
#'
#' }

coint_eq <- function(object, case) {
    UseMethod("coint_eq")
}

#' @rdname coint_eq
#' @export
#'

coint_eq.recm <- function(object, ...) {
    object$data[,object$parsed_formula$y_part$var]  - stats::lag(object$data[,"ect"], 1)
}

#' @rdname coint_eq
#' @export
#'

coint_eq.default <- function(object, case) {
    if ("recm_indicator" %in% class(object)) {
        recm_indicator <- TRUE
        class(object) <- class(object)[-4]
    } else {
        recm_indicator <- FALSE
    }
    if (isTRUE(all.equal(c("dynlm", "lm", "ardl"), class(object)))) {
        object <- uecm(object)
    } else if (!(isTRUE(all.equal(c("dynlm", "lm", "uecm"), class(object))))) {
        stop(paste0("no applicable for an object of class \"", paste0(class(object), collapse = '" "'), "\""),  call. = FALSE)
    }
    parsed_formula <- object$parsed_formula
    case <- parse_case(parsed_formula = parsed_formula, case = case)
    order <- object$order
    data <- object$data
    lr_multipliers <- multipliers(object = object, type = "lr")
    lr_mult_names <- as.character(lr_multipliers$term)
    lr_multipliers <- lr_multipliers$estimate

    if (case == 1) {
        lr_multipliers <- c(0, 0, lr_multipliers)
        trend_var <- rep(0, nrow(data))
    } else if (case == 2) {
        lr_multipliers <- c(lr_multipliers[1], 0, lr_multipliers[2:length(lr_multipliers)])
        trend_var <- rep(0, nrow(data))
    } else if (case == 3) {
        lr_multipliers <- c(0, 0, lr_multipliers[-1])
        trend_var <- rep(0, nrow(data))
    } else if (case == 4) {
        lr_multipliers <- c(0, lr_multipliers[-1])
        is_trend_in_lr <- "trend" %in% stringr::str_sub(stringr::str_replace_all(lr_mult_names[2], " ", ""), 1, 5)
        is_time_in_lr <- "time(" %in% stringr::str_sub(stringr::str_replace_all(lr_mult_names[2], " ", ""), 1, 5)
        if (is_trend_in_lr == TRUE) {
            trimed_trend <- stringr::str_replace_all(lr_mult_names[2], " ", "")
            short_match <- stringr::str_sub(trimed_trend, nchar(trimed_trend) - 7, nchar(trimed_trend))
            long_match <- stringr::str_sub(trimed_trend, nchar(trimed_trend) - 11, nchar(trimed_trend))
            if (any(c("scale=FALSE)", "scale=F)") %in% c(short_match, long_match))) {
                trend_var <- 1:nrow(data)
            } else {
                trend_var <- (1:nrow(data)) / stats::frequency(data)
            }
        } else if (is_time_in_lr == TRUE) {
            trend_var <- stats::time(data) %>% as.numeric()
        }
    } else if (case == 5) {
        lr_multipliers <- c(0, 0, lr_multipliers[-(1:2)])
        trend_var <- rep(0, nrow(data))
    }
    unit_vector <- rep(1, nrow(data))

    # create the design matrix to compute the coint_eq (cointegrating equation in levels)
    design_matrix <- data.frame(unit_vector, trend_var,
        dplyr::as_tibble(data) %>% dplyr::select(parsed_formula$x_part$var))
    # create the coint_eq components (multipliers * variable)
    design_matrix <- lapply(1:ncol(design_matrix), function(i) design_matrix[ ,i] * lr_multipliers[i]) %>%
        as.data.frame()
    # add the dependent variable
    design_matrix <- data.frame(y = data[, parsed_formula$y_part$var], design_matrix)
    # compute the coint_eq
    coint_eq <- design_matrix %>%
        dplyr::mutate(coint_eq = rowSums(.[2:ncol(design_matrix)])) %>%
        dplyr::select(coint_eq) %>%
        .[[1]]
    # to inherit the proper time-series class and properties
    res <- cbind(data, coint_eq)
    res <- res[, "coint_eq"]
    if (recm_indicator) {
        coint_eq <- stats::ts(coint_eq, start = stats::start(data), frequency = stats::frequency(data))
        res <- list(coint_eq = coint_eq, design_matrix = design_matrix, data= data,
                    parsed_formula = parsed_formula, case = case, order = order)
    }
    return(res)
}
