#' ARDL formula specification builder
#'
#' It creates the ARDL specification according to the given "formula" and their
#' corresponding "orders".
#'
#' @param parsed_formula A list containing the formula parts as returned from
#'   \code{\link{parse_formula}}.
#' @param order A numeric vector with the ARDL order as returned from
#'   \code{\link[=parse_order]{parse_order(restriction = FALSE)}}.
#'
#' @return \code{build_ardl_formula} returns a list containing the full formula
#'   and the independent and dependent parts of the formula separated. The full
#'   formula is ready to be used as input in the \code{dynlm} function.
#'
#' @seealso \code{\link{build_uecm_formula}}, \code{\link{build_recm_formula}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

build_ardl_formula <- function(parsed_formula, order) {

    ardl_formula <- list()
    ardl_formula$dep_part <- paste0(parsed_formula$y_part$var, " ~ ")

    ardl_formula$indep_part$w$var <- c()
    if (length(parsed_formula$w_part$var) > 0) {
        ardl_formula$indep_part$w <- paste0(parsed_formula$w_part$var, " + ", collapse = "")
    }

    ardl_formula$indep_part$y <- c()
    ardl_formula$indep_part$y <- paste0("L(", parsed_formula$y_part$var, ", ", 1:order[1], ")", " + ", collapse = "")

    ardl_formula$indep_part$x <- c()
    for(i in 1:parsed_formula$kx) {
        if (order[i + 1] != 0) {
            temp <- paste0(parsed_formula$x_part$var[i], " + ", paste0("L(", parsed_formula$x_part$var[i], ", ", 1:order[i + 1], ")", " + ", collapse = ""), collapse = "")
        } else {
            temp <- paste0(parsed_formula$x_part$var[i], " + ")
        }
        ardl_formula$indep_part$x <- paste(ardl_formula$indep_part$x, temp, collapse = "")
    }
    rm(temp)

    if (parsed_formula$kfixed > 0) {
        ardl_formula$indep_part$fixed <- paste0(parsed_formula$fixed_part$var, " + ", collapse = "")
    } else {
        ardl_formula$indep_part$fixed <- ""
    }

    ardl_formula$full <- paste0(ardl_formula$dep_part, ardl_formula$indep_part$w,
                                ardl_formula$indep_part$y, ardl_formula$indep_part$x,
                                ardl_formula$indep_part$fixed, collapse = "") %>%
        stringr::str_trim(side = "both")
    ardl_formula$full <- ifelse(stringr::str_sub(ardl_formula$full,
                                        start = stringr::str_count(ardl_formula$full),
                                        end = stringr::str_count(ardl_formula$full)) == "+",
                                stringr::str_sub(ardl_formula$full, start = 1, end = stringr::str_count(ardl_formula$full) -1 ),
                                ardl_formula$full) %>%
        stringr::str_trim(side = "both")

    return_list <- list(dep_part = ardl_formula$dep_part, indep_part = ardl_formula$indep_part,
                        full = ardl_formula$full)

    return(return_list)
}

#' UECM formula specification builder
#'
#' It creates the UECM (Unrestricted Error Correction Model) specification
#' according to the given "formula" and the corresponding "order" of the
#' underlying ARDL.
#'
#' @param order A numeric vector with the underlying ARDL order as returned from
#'   \code{\link[=parse_order]{parse_order(restriction = FALSE)}}.
#' @inheritParams build_ardl_formula
#'
#' @return \code{build_uecm_formula} returns a list containing the full formula
#'   and the independent and dependent parts of the formula separated. The full
#'   formula is ready to be used as input in the \code{dynlm} function.
#'
#' @seealso \code{\link{build_ardl_formula}}, \code{\link{build_recm_formula}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

build_uecm_formula <- function(parsed_formula, order) {

    uecm_formula <- list()
    uecm_formula$dep_part <- paste0("d(", parsed_formula$y_part$var, ")", " ~ ")

    uecm_formula$indep_part$w <- c()
    if (length(parsed_formula$w_part$var) > 0) {
        uecm_formula$indep_part$w <- paste0(parsed_formula$w_part$var, " + ", collapse = "")
    }

    uecm_formula$indep_part$levels$y <- c()
    uecm_formula$indep_part$levels$y <- paste0("L(", parsed_formula$y_part$var, ", ", 1, ")", " + ", collapse = "")

    uecm_formula$indep_part$levels$x <- c()
    uecm_formula$indep_part$diff$x <- c()
    for(i in 1:parsed_formula$kx){
        if (order[i + 1] == 0) {
            temp <- paste0(parsed_formula$x_part$var[i], " + ")
            temp1 <- ""
        } else if (order[i + 1] == 1) {
            temp <- paste0("L(", parsed_formula$x_part$var[i], ", ", 1, ")", " + ", collapse = "")
            temp1 <- paste0("d(", parsed_formula$x_part$var[i], ") + ", collapse = "")
        } else {
            temp <- paste0("L(", parsed_formula$x_part$var[i], ", ", 1, ")", " + ", collapse = "")
            temp1 <- paste0("d(", parsed_formula$x_part$var[i], ") + ",
                        paste0("d(L(", parsed_formula$x_part$var[i], ", ", 1:(order[i + 1] - 1), "))", " + ", collapse = ""),
                        collapse = "")
        }
        uecm_formula$indep_part$levels$x <- paste(uecm_formula$indep_part$levels$x, temp, collapse = "")
        uecm_formula$indep_part$diff$x <- paste(uecm_formula$indep_part$diff$x, temp1, collapse = "")
    }
    rm(temp); rm(temp1)

    uecm_formula$indep_part$diff$y <- c()
    if (order[1] == 1) {
        uecm_formula$indep_part$diff$y <- ""
    } else { # AR(0) models are discarded anyway
        uecm_formula$indep_part$diff$y <- paste0("d(L(", parsed_formula$y_part$var, ", ", 1:(order[1] - 1), "))", " + ", collapse = "")
    }

    if (parsed_formula$kfixed > 0) {
        uecm_formula$indep_part$fixed <- paste0(parsed_formula$fixed_part$var, " + ", collapse = "")
    } else {
        uecm_formula$indep_part$fixed <- ""
    }

    uecm_formula$full <- paste0(uecm_formula$dep_part, uecm_formula$indep_part$w,
                                uecm_formula$indep_part$levels$y, uecm_formula$indep_part$levels$x,
                                uecm_formula$indep_part$diff$y, uecm_formula$indep_part$diff$x,
                                uecm_formula$indep_part$fixed, collapse = "") %>%
        stringr::str_trim(side = "both")
    uecm_formula$full <- ifelse(stringr::str_sub(uecm_formula$full,
                                        start = stringr::str_count(uecm_formula$full),
                                        end = stringr::str_count(uecm_formula$full)) == "+",
                                stringr::str_sub(uecm_formula$full, start = 1, end = stringr::str_count(uecm_formula$full) -1 ),
                                uecm_formula$full) %>%
        stringr::str_trim(side = "both")

    return_list <- list(dep_part = uecm_formula$dep_part, indep_part = uecm_formula$indep_part,
                        full = uecm_formula$full)

    return(return_list)
}

#' RECM formula specification builder
#'
#' It creates the RECM (Restricted Error Correction Model) specification
#' according to the given "formula" and the corresponding "order" of the
#' underlying ARDL.
#'
#' @param case An integer from 1-5 or a character string specifying whether the
#'   'intercept' or the 'trend' have to participate in the 
#'   long-run/cointegrating relationship/equation (see 'Details').
#' @inheritParams build_uecm_formula
#'
#' @return \code{build_recm_formula} returns a list containing the full formula
#'   and the independent and dependent parts of the formula separated. The full
#'   formula is ready to be used as input in the \code{dynlm} function providing
#'   also the 'ect' (error correction term) to the data.
#'
#' @seealso \code{\link{build_ardl_formula}}, \code{\link{build_uecm_formula}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords internal
#'

build_recm_formula <- function(parsed_formula, order, case) {

    recm_formula <- list()
    recm_formula$dep_part <- paste0("d(", parsed_formula$y_part$var, ")", " ~ ")
    recm_formula$indep_part$w <- c()
    recm_formula$indep_part$w <- paste0(parsed_formula$w_part$var, " + ", collapse = "")
    if (case %in% c(1, 2)) {
        recm_formula$indep_part$w <- "- 1 + "
    } else if (case %in% c(3, 4)) {
        recm_formula$indep_part$w <- ""
    } else if (case == 5) {
        recm_formula$indep_part$w <- paste0(parsed_formula$w_part$var, " + ", collapse = "")
    }

    recm_formula$indep_part$diff$x <- c()
    for(i in 1:parsed_formula$kx){
        if (order[i + 1] == 0) {
            temp <- ""
        } else if (order[i + 1] == 1) {
            temp <- paste0("d(", parsed_formula$x_part$var[i], ") + ", collapse = "")
        } else {
            temp <- paste0("d(", parsed_formula$x_part$var[i], ") + ",
                        paste0("d(L(", parsed_formula$x_part$var[i], ", ", 1:(order[i + 1] - 1), "))", " + ", collapse = ""),
                        collapse = "")
        }
        recm_formula$indep_part$diff$x <- paste(recm_formula$indep_part$diff$x, temp, collapse = "")
    }
    rm(temp)

    recm_formula$indep_part$diff$y <- c()
    if (order[1] == 1) {
        recm_formula$indep_part$diff$y <- ""
    } else { # AR(0) models are discarded anyway
        recm_formula$indep_part$diff$y <- paste0("d(L(", parsed_formula$y_part$var, ", ", 1:(order[1] - 1), "))", " + ", collapse = "")
    }

    if (parsed_formula$kfixed > 0) {
        recm_formula$indep_part$fixed <- paste0(parsed_formula$fixed_part$var, " + ", collapse = "")
    } else {
        recm_formula$indep_part$fixed <- ""
    }

    recm_formula$indep_part$ect <- "ect"

    recm_formula$full <- paste0(recm_formula$dep_part, recm_formula$indep_part$w,
                                recm_formula$indep_part$diff$y, recm_formula$indep_part$diff$x,
                                recm_formula$indep_part$fixed, recm_formula$indep_part$ect,
                                collapse = "") %>%
        stringr::str_trim(side = "both")
    recm_formula$full <- ifelse(stringr::str_sub(recm_formula$full,
                                        start = stringr::str_count(recm_formula$full),
                                        end = stringr::str_count(recm_formula$full)) == "+",
                                stringr::str_sub(recm_formula$full, start = 1, end = stringr::str_count(recm_formula$full) -1 ),
                                recm_formula$full) %>%
        stringr::str_trim(side = "both")

    return_list <- list(dep_part = recm_formula$dep_part, indep_part = recm_formula$indep_part,
                        full = recm_formula$full)

    return(return_list)
}
