#' Formula parser
#'
#' It parses the formula and separates the dependent, independent and fixed
#' variables and also the constant and linear trends (if present).
#'
#' The notation we follow (e.g., using y, x, z, w etc.) is according to
#' \cite{Pesaran et al. (2001)}.
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
#' @param colnames_data A character vector containing the colnames of the data
#'   used in the formula (usually via \code{colnames(data)}).
#'
#' @return A list containing other lists with the names of the dependent,
#'   independent and fixed variables, the constant and linear trends and the
#'   number of variables in each category.
#'
#' @section References: Pesaran, M. H., Shin, Y., & Smith, R. J. (2001). Bounds
#'   testing approaches to the analysis of level relationships. \emph{Journal of
#'   Applied Econometrics}, 16(3), 289-326
#'
#' @seealso \code{\link{parse_order}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

parse_formula <- function(formula, colnames_data) {

    # turn formula (from class = formula) into character
    formula <- Reduce(paste, deparse(formula))

    # split at fixed_part at |
    formula <- stringr::str_split(formula, pattern = "[|]")[[1]]
    w_part <- list(var = "")
    if (length(formula) == 1) {
        fixed_part <- list(var = "")
    } else if (length(formula) > 2) {
        stop("Formula can not contain more than one '|' symbols.", call. = FALSE)
    } else {
        # split fixed_part at + and trim white spaces
        fixed_part <- stringr::str_split(formula[2], pattern = "[+]")[[1]] %>%
            stringr::str_trim(side = "both") %>%
            list(var = .)
        if (fixed_part$var[1] == "") stop("The '|' symbol must be followed by at least one variable name.", call. = FALSE)
        # check if linear/non-constant intercept exists in fixed_part
        temp <- stringr::str_sub(stringr::str_replace_all(fixed_part$var, " ", ""), 1, 6) %in% "trend(" |
            stringr::str_sub(stringr::str_replace_all(fixed_part$var, " ", ""), 1, 5) %in% "time("
        if (sum(temp) != 0) {
            w_part$var <- fixed_part$var[temp]
            if (sum(temp) == length(fixed_part$var)) {
                fixed_part$var <- ""
            } else {
                fixed_part$var <- fixed_part$var[!(temp)]
            }
        }
        rm(temp)
    }

    formula <- stringr::str_split(formula[1], pattern = "[~]")[[1]] # split at ~
    if (length(formula) == 1) {
        stop("The formula must contain the '~' symbol.", call. = FALSE)
    } else if (length(formula) > 2){
        stop("The formula must contain exactly one '~' symbol.", call. = FALSE)
    }

    # split y_part at + (if any) and trim white spaces
    y_part <- stringr::str_split(formula[1], pattern = "[+]")[[1]] %>%
        stringr::str_trim(side = "both") %>%
        list(var = .)
    if ((length(y_part$var) > 1) | y_part$var[1] == "") {
        stop("The formula must contain exactly one dependent variable.", call. = FALSE)
    }

    # identify if there is an interpect exclusion
    formula = stringr::str_split(formula[2], pattern = "[-]")[[1]] %>%
        stringr::str_trim(side = "both")
    if (length(formula) == 2) {
        if (stringr::str_sub(formula[2], start = 1, end = 1) == "1") {
            if (w_part$var[1] != "") { # constant intercept exclusion
                w_part$var <- c("- 1", w_part$var)
            } else {
                w_part$var <- "- 1"
            }
        } else {
            stop("The formula can only accept the term '-1' for the exclusion of constant intercept.", call. = FALSE)
        }
    } else if (length(formula) > 2) {
        stop("The formula can only accept one '-1' constant intercept exclusion term.", call. = FALSE)
    }

    # split x_part at + and trim white spaces
    if (length(formula) != 1) {
        formula = paste0(formula[1], stringr::str_sub(formula[2], start = 2, end = stringr::str_count(formula[2])))
    }
    x_part <- stringr::str_split(formula, pattern = "[+]")[[1]] %>%
        stringr::str_trim(side = "both") %>%
        list(var = .)
    if (x_part$var[1] == "") {
        warnings("The formula contains only an AR (Autoregressive) part.")
    }
    # check if linear/non-constant intercept exists in x_part
    temp <- stringr::str_sub(stringr::str_replace_all(x_part$var, " ", ""), 1, 6) %in% "trend(" |
        stringr::str_sub(stringr::str_replace_all(x_part$var, " ", ""), 1, 5) %in% "time("
    if (sum(temp) != 0) {
        if (w_part$var[1] != "") {
            w_part$var <- c(w_part$var, x_part$var[temp])
        } else {
            w_part$var <- x_part$var[temp]
        }
        if (sum(temp) == length(x_part$var)) {
            x_part$var <- ""
        } else {
            x_part$var <- x_part$var[!(temp)]
        }
    }
    rm(temp)

    # create the union set of variables
    if (y_part$var[1] == "") y_part$var <- c()
    if (x_part$var[1] == "") x_part$var <- c()
    if (fixed_part$var[1] == "") fixed_part$var <- c()
    if (w_part$var[1] == "") w_part$var <- c()
    z_part <- list(var = c(y_part$var, x_part$var))
    kz <- length(z_part$var)
    kx <- length(x_part$var)
    temp <- stringr::str_sub(stringr::str_replace_all(w_part$var, " ", ""), 1, 5)
    kw <- if ((length(w_part$var) == 0) | (any(c("trend", "time(") %in% temp) & ("-1" %in% temp))) {
        1
    } else if (any(c("trend", "time(") %in% temp) & !("-1" %in% temp)) {
        2
    } else if (!any(c("trend", "time(") %in% temp) & ("-1" %in% temp)) {
        0
    }
    rm(temp)
    kfixed <- length(fixed_part$var)

    # check for correct function inputs
    # check if variables exist
    if (sum(!(c(z_part$var, fixed_part$var) %in% colnames_data)) >= 1) {
        stop(c("Variable(s) ",
                   paste0("'", c(z_part$var, fixed_part$var)[!(c(z_part$var, fixed_part$var) %in% colnames_data)], sep = "', "),
                   "not found in 'data'"), call. = FALSE)
    }

    return_list <- list(y_part = y_part, x_part = x_part, z_part = z_part,
                        w_part = w_part, fixed_part = fixed_part,
                        kz = kz, kx = kx, kw = kw, kfixed = kfixed)
    return(return_list)

}

#' Order parser
#'
#' It parses the order and checks the integrity of the order input.
#'
#' @param orders A numeric vector of the same length as the total number of
#'   variables (excluding the fixed ones). If the input is \code{order} or
#'   \code{max_order} it should only contain positive integers or 0. If the
#'   input is \code{fixed_order} it should also contain the value '-1'
#'   indicating that a specific order should not be fixed. An integer could be
#'   provided if all variables are of the same order (or all '-1' in the case of
#'   \code{fixed_order}).
#' @param order_name The name of the function argument that is passed into
#'   \code{order}.
#' @param var_names The names of the variables corresponding to the orders.
#' @param kz An integer. The number of dependent and independent variables.
#' @param restriction When the input in \code{orders} is either \code{order} or
#'   \code{max_order} it should be \code{FALSE} (default). When the input is
#'   \code{fixed_order} it should be '-1' indicating that the input in
#'   \code{orders} is a restriction for the 'order' of the model (either upper
#'   bound or fixed order).
#'
#' @return A numeric vector of the same length as the total number of variables
#'   (excluding the fixed ones).
#'
#' @seealso \code{\link{parse_formula}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

parse_order <- function(orders, order_name, var_names, kz, restriction = FALSE) {
    if (restriction == FALSE) {
        restriction = 0
    }

    # check if orders length is correct
    if (!(length(orders) %in% c(1, kz))) {
        stop("The length of '", order_name, "' is not correct", call. = FALSE)
    }
    # check for AR(0) models
    if (orders[1] == 0) {
        stop("AR(0) models are not allowed. The order of the dependent variable should be a positive integer", call. = FALSE)
    }
    # check for negatives (or other restriction e.g. -1) and decimals in orders
    if ((any(orders < restriction)) | (sum(orders %% 1) != 0)) {
        if (restriction == 0) {
            stop("'", order_name, "' can only contain positive integers or 0", call. = FALSE)
        } else if (restriction == (-1)) {
            stop("'", order_name, "' can only contain positive integers, 0 or '-1'", call. = FALSE)
        }
    }

    if (length(orders) == 1) {
        orders <- rep(orders[1], kz)
    }
    names(orders) <- var_names

    return(orders)
}

#' Case parser
#'
#' It parses the 'case' and checks the integrity of the 'case' input and the
#' compatibility with the formula.
#'
#' @inherit recm details
#'
#' @param parsed_formula A list containing the formula parts as returned from
#'   \code{\link{parse_formula}}.
#' @inheritParams recm
#'
#' @return An integer from 1-5 representing the case.
#'
#' @inheritSection recm References
#' @seealso \code{\link{parse_formula}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

parse_case <- function(parsed_formula, case) {

    if ((length(case) != 1) | !(case[1]) %in% c(1:5, 'n', 'rc', 'uc', 'ucrt', 'ucut')) {
        stop("'case' must be a number between 1 and 5 or one of the 'n', 'rc', 'uc', 'ucrt' or 'ucut'", call. = FALSE)
    } else if (case == "n") {
        case = 1
    } else if (case == "rc") {
        case = 2
    } else if (case == "uc") {
        case = 3
    } else if (case == "ucrt") {
        case = 4
    } else if (case == "ucut") {
        case = 5
    }

    w_check <- stringr::str_sub(stringr::str_replace_all(parsed_formula$w_part$var, " ", ""), 1, 5)
    if ((case == 1) & (any(c("trend", "time(") %in% w_check ) | !("-1" %in% w_check))) {
        stop("Trying to impose case 1 (no constant, no linear trend) but the underlying ARDL model includes at least one of them", call. = FALSE)
    } else if ((case %in% c(2,3)) & any(c("-1", "trend", "time(") %in% w_check)) {
        stop("Trying to impose case ", ifelse(case == 2,
            paste0(case, " (restricted constant, no linear trend)"),
            paste0(case, " (unrestricted constant, no linear trend)")), " but the underlying ARDL model either doesn't include a constant or includes a trend", call. = FALSE)
    } else if ((case %in% c(4,5)) & (!any(c("-1", "trend", "time(") %in% w_check))) {
        stop("Trying to impose case ", ifelse(case == 4,
            paste0(case, " (unrestricted constant, restricted linear trend)"),
            paste0(case, " (unrestricted constant, unrestricted linear trend)")), " but the underlying ARDL model doesn't include one or both of them", call. = FALSE)
    }

    return(case)
}
