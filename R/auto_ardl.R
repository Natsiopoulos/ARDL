#' Automatic ARDL model selection
#'
#' It searches for the best ARDL order specification, according to the selected
#' criterion, taking into account the constraints provided.
#'
#' @param formula A "formula" describing the linear model. Details for model
#'   specification are given under 'Details' in the help file of the
#'   \code{\link{ardl}} function.
#' @param max_order It sets the maximum order for each variable where the search
#'   is taking place. A numeric vector of the same length as the total number of
#'   variables (excluding the fixed ones, see 'Details' in the help file of the
#'   \code{\link{ardl}} function). It should only contain positive integers. An
#'   integer could be provided if the maximum order for all variables is the
#'   same.
#' @param fixed_order It allows setting a fixed order for some variables. The
#'   algorithm will not search for any other order than this. A numeric vector
#'   of the same length as the total number of variables (excluding the fixed
#'   ones). It should contain positive integers or 0 to set as a constraint. A
#'   -1 should be provided for any variable that should not be constrained.
#'   \code{fixed_order} overrides the corresponding \code{max_order} and
#'   \code{starting_order}.
#' @param starting_order Specifies the order for each variable from which each
#'   search will start. It is a numeric vector of the same length as the total
#'   number of variables (excluding the fixed ones). It should contain positive
#'   integers or 0 or only one integer could be provided if the starting order
#'   for all variables is the same. Default is set to NULL. If unspecified
#'   (\code{NULL}) and \code{grid = FALSE}, then all possible \eqn{ARDL(p)}
#'   models are calculated (constraints are taken into account), where \eqn{p}
#'   is the minimum value in \code{max_order}. Note that where
#'   \code{starting_order} is provided, its first element will be the minimum
#' value of \eqn{p} that the searching algorithm will consider (think of it like
#' a 'minimum p order' restriction) (see 'Searching algorithm' below). If
#' \code{grid = TRUE}, only the first argument (\eqn{p}) will have an effect.
#' @param selection A character string specifying the selection criterion
#'   according to which the candidate models will be ranked. Default is
#'   \code{\link[stats]{AIC}}. Any other selection criterion can be used (a user
#'   specified or a function from another package) as long as it can be applied
#'   as \code{selection(model)}. The preferred model is the one with the smaller
#'   value of the selection criterion. If the selection criterion works the
#'   other way around (the bigger the better), \code{selection_minmax = "max"}
#'   should also be supplied (see 'Examples' below).
#' @param selection_minmax A character string that indicates whether the
#'   criterion in \code{selection} is supposed to be minimized (default) or
#'   maximized.
#' @param grid If \code{FALSE} (default), the stepwise searching regression
#'   algorithm will search for the best model by adding and subtracting terms
#'   corresponding to different ARDL orders. If \code{TRUE}, the whole set of
#'   all possible ARDL models (accounting for constraints) will be evaluated.
#'   Note that this method can be very time-consuming in case that
#'   \code{max_order} is big and there are many independent variables that
#'   create a very big number of possible combinations.
#' @param search_type A character string describing the search type. If
#'   "horizontal" (default), the searching algorithm increases or decreases by 1
#'   the order of each variable in each iteration. When the order of the last
#'   variable has been accessed, it begins again from the first variable until
#'   it converges. If "vertical", the searching algorithm increases or decreases
#'   by 1 the order of a variable until it converges. Then it continues the same
#'   for the next variable. The two options result to very similar top orders.
#'   The default ("horizontal"), sometimes is a little more accurate, but the
#'   "vertical" is almost 2 times faster. Not applicable if \code{grid = TRUE}.
#' @inheritParams ardl
#'
#' @return \code{auto_ardl} returns a list which contains:
#'   \item{\code{best_model}}{An object of class \code{c("dynlm", "lm", "ardl")}}
#'   \item{\code{best_order}}{A numeric vector with the order of the best model selected}
#'   \item{\code{top_orders}}{A data.frame with the orders of the top 20 models}
#'
#' @section Searching algorithm: The algorithm performs the optimization process
#'   starting from multiple starting points concerning the autoregressive order
#'   \eqn{p}. The searching algorithm will perform a complete search, each time
#'   starting from a different starting order. These orders are presented in the
#'   tables below, for \code{grid = FALSE} and different values of
#'   \code{starting_order}.
#'
#'   \code{starting_order = NULL}:
#'   \tabular{ccccccc}{
#'   ARDL(p) \tab -> \tab p \tab q1 \tab q2 \tab ... \tab qk\cr
#'   ARDL(1) \tab -> \tab 1 \tab 1 \tab 1 \tab ... \tab 1\cr
#'   ARDL(2) \tab -> \tab 2 \tab 2 \tab 2 \tab ... \tab 2\cr
#'   : \tab -> \tab : \tab : \tab : \tab : \tab :\cr
#'   ARDL(P) \tab -> \tab P \tab P \tab P \tab ... \tab P
#'   }
#'
#'   \code{starting_order = c(3, 0, 1, 2)}:
#'   \tabular{cccc}{
#'   p \tab q1 \tab q2 \tab q3\cr
#'   3 \tab 0 \tab 1 \tab 2\cr
#'   4 \tab 0 \tab 1 \tab 2\cr
#'   : \tab : \tab : \tab :\cr
#'   P \tab 0 \tab 1 \tab 2
#'   }
#'
#' @seealso \code{\link{ardl}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords optimize models ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## Find the best ARDL order --------------------------------------------
#'
#' # Up to 5 for the autoregressive order (p) and 4 for the rest (q1, q2, q3)
#'
#' # Using the defaults search_type = "horizontal", grid = FALSE and selection = "AIC"
#' # ("Not run" indications only for testing purposes)
#' \dontrun{
#' model1 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                     max_order = c(5,4,4,4))
#' model1$top_orders
#'
#' ## Same, with search_type = "vertical" -------------------------------
#'
#' model1_h <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                       max_order = c(5,4,4,4), search_type = "vertical")
#' model1_h$top_orders
#'
#' ## Find the global optimum ARDL order ----------------------------------
#'
#' # It may take more than 10 seconds
#' model_grid <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), grid = TRUE)
#'
#' ## Different selection criteria ----------------------------------------
#'
#' # Using BIC as selection criterion instead of AIC
#' model1_b <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                       max_order = c(5,4,4,4), selection = "BIC")
#' model1_b$top_orders
#'
#' # Using other criteria like adjusted R squared (the bigger the better)
#' adjr2 <- function(x) { summary(x)$adj.r.squared }
#' model1_adjr2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                            max_order = c(5,4,4,4), selection = "adjr2",
#'                            selection_minmax = "max")
#' model1_adjr2$top_orders
#'
#' # Using functions from other packages as selection criteria
#' if (requireNamespace("qpcR", quietly = TRUE)) {
#'
#' library(qpcR)
#' model1_aicc <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                           max_order = c(5,4,4,4), selection = "AICc")
#' model1_aicc$top_orders
#' adjr2 <- function(x){ Rsq.ad(x) }
#' model1_adjr2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                            max_order = c(5,4,4,4), selection = "adjr2",
#'                            selection_minmax = "max")
#' model1_adjr2$top_orders
#'
#' ## DIfferent starting order --------------------------------------------
#'
#' # The searching algorithm will start from the following starting orders:
#' # p q1 q2 q3
#' # 1 1  3  2
#' # 2 1  3  2
#' # 3 1  3  2
#' # 4 1  3  2
#' # 5 1  3  2
#'
#' model1_so <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), starting_order = c(1,1,3,2))
#'
#' # Starting from p=3 (don't search for p=1 and p=2)
#' # Starting orders:
#' # p q1 q2 q3
#' # 3 1  3  2
#' # 4 1  3  2
#' # 5 1  3  2
#'
#' model1_so_3 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), starting_order = c(3,1,3,2))
#'
#' # If starting_order = NULL, the starting orders for each iteration will be:
#' # p q1 q2 q3
#' # 1 1  1  1
#' # 2 2  2  2
#' # 3 3  3  3
#' # 4 4  4  4
#' # 5 5  5  5
#' }
#'
#' ## Add constraints -----------------------------------------------------
#'
#' # Restrict only the order of IBO to be 2
#' model1_ibo2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), fixed_order = c(-1,-1,2,-1))
#' model1_ibo2$top_orders
#'
#' # Restrict the order of LRM to be 3 and the order of IBO to be 2
#' model1_lrm3_ibo2 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), fixed_order = c(3,-1,2,-1))
#' model1_lrm3_ibo2$top_orders
#'
#' ## Set the starting date for the regression (data starts at "1974 Q1") -
#'
#' # Set regression starting date to "1976 Q1"
#' model1_76q1 <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
#'                         max_order = c(5,4,4,4), start = "1976 Q1")
#' start(model1_76q1$best_model)
#' }

auto_ardl <- function(formula, data, max_order, fixed_order = -1, starting_order = NULL,
                      selection = "AIC", selection_minmax = c("min", "max"),
                      grid = FALSE, search_type = c("horizontal", "vertical"),
                      start = NULL, end = NULL, ...) {

    if (!any(c("ts", "zoo", "zooreg") %in% class(data))) {
        data <- stats::ts(data, start = 1, end = nrow(data), frequency = 1)
    }
    if (missing(max_order) == TRUE) {stop("'max_order' is a mandatory argument.", call. = FALSE)}
    if (missing(selection) == TRUE) {selection <- "AIC"}
    search_type <- match.arg(search_type)
    selection_minmax <- match.arg(selection_minmax)
    parsed_formula <- parse_formula(formula = formula, colnames_data = colnames(data))
    max_order <- parse_order(orders = max_order, order_name = "max_order",
                             var_names = parsed_formula$z_part$var, kz = parsed_formula$kz)
    fixed_order <- parse_order(orders = fixed_order, order_name = "fixed_order",
                               var_names = parsed_formula$z_part$var, kz = parsed_formula$kz, restriction = -1)
    if (!missing(starting_order)) {
        starting_order_null <- FALSE
        if (starting_order[1] < 1) { stop("In 'starting_order', the starting order of p (first argument) can't be less than 1.", call. = FALSE)}
        starting_order <- parse_order(orders = starting_order, order_name = "starting_order",
                                      var_names = parsed_formula$z_part$var, kz = parsed_formula$kz)
        if (any(starting_order > max_order)) {stop("'starting_order' can't be greater than 'max_order'.", call. = FALSE)}
    } else {
        starting_order_null <- TRUE
    }
    if (any(fixed_order > max_order)) {stop("'fixed_order' can't be greater than 'max_order'.", call. = FALSE)}
    start_sample <- start
    end_sample <- end

    # acceptable orders for each p and q
    order_list <- lapply(seq_len(length(max_order)), function(i) {
        # because order of y can't be 0
        if (i == 1) {
            if (fixed_order[i] == (-1)) {
                if (is.null(starting_order)) {
                    1:max_order[i]
                } else {
                    starting_order[1]:max_order[i]
                }
            } else {
                fixed_order[i]
            }
        } else {
            if (fixed_order[i] == (-1)) {
                0:max_order[i]
            } else {
                fixed_order[i]
            }
        }
    })

    # starting_order considering fixed_order
    if (is.null(starting_order)) {
        if (grid == FALSE) {
            starting_order <- ifelse(fixed_order[-1] == -1, 0, fixed_order[-1])
            starting_order <- c(ifelse(fixed_order[1] == -1, 1, fixed_order[1]), starting_order)
            # build orders for VAR
            order_var <- lapply(1:min(max_order), function(i) rep(i, parsed_formula$kz))
            if (any(fixed_order != -1)) { # build orders for var with rectricted terms
                for (j in 1:length(order_var)) {
                    order_var[[j]][which(fixed_order != -1)] <- fixed_order[which(fixed_order != -1)]
                }
            }
        }
    } else {
        starting_order <- ifelse(fixed_order == -1, starting_order, fixed_order)
    }

    if (grid == TRUE) {
        # create the full search grid
        order_grid <- expand.grid(order_list, KEEP.OUT.ATTRS = FALSE)

        # run all models and estimate "selection" (using eval to avoid if for each "selection")
        best_selection <- lapply(seq_len(nrow(order_grid)), function(i) {
            eval(parse(text = paste(selection, "(
                                    ardl(formula = formula, data = data, order = unlist(order_grid[", i, ", ]), start = start_sample, end = end_sample, ...)
            )")))
        }) %>%
            unlist()
        if (selection_minmax == "max") best_selection <- (-1)*best_selection
        # keep top 20 orders
        top_orders <- dplyr::bind_cols(order = order_grid, selection = best_selection) %>%
            dplyr::arrange(selection) %>% dplyr::slice(1:20)
    } else {
        top_orders <- c() #initialization
        if (starting_order_null) {
            for_each_p <- 1:length(order_var) #$#$ or try order_list[[1]]
        } else {
            for_each_p <- order_list[[1]]
        }
        for (i in for_each_p) { # for each parse
            if (starting_order_null) {
                order1 <- order_var[[i]] # access each var model
            } else {
                order1 <- starting_order
                order1[1] <- i
            }
            m1 <- ardl(formula = formula, data = data, order = order1, start = start_sample, end = end_sample, ...)
            selection1 <- eval(parse(text = paste(selection, "(m1)")))
            if (selection_minmax == "max") selection1 <- (-1)*selection1
            top_orders <- rbind(top_orders, c(order1, selection1))

            if (search_type == "vertical") {
                for (j in 1:parsed_formula$kx) {
                    if (length(order_list[[j + 1]]) != 1) { # if fixed order, don't search
                        ardl_converge <- FALSE
                        while ((ardl_converge == FALSE) & (order1[j + 1] < dplyr::last(order_list[[j + 1]]))) {
                            order2_back <- order1
                            order2_forth <- order1
                            if (order1[j + 1] == 0) { # so that when q=0 it doesn't search for q<0.
                                # order2_back is the order1
                                order2_forth[j + 1] <- order1[j + 1] + 1
                            } else {
                                order2_back[j + 1] <- order1[j + 1] - 1
                                order2_forth[j + 1] <- order1[j + 1] + 1
                            }
                            order_back_ready <- unlist(lapply(1:nrow(top_orders), FUN = function(z) sum(order2_back == top_orders[z,-ncol(top_orders)])))
                            order_forth_ready <- unlist(lapply(1:nrow(top_orders), FUN = function(z) sum(order2_forth == top_orders[z,-ncol(top_orders)])))
                            if (parsed_formula$kz %in% order_back_ready) {
                                selection_m2_back <- top_orders[which(order_back_ready == parsed_formula$kz)[1], ncol(top_orders)]
                            } else {
                                m2_back <- ardl(formula = formula, data = data, order = order2_back, start = start_sample, end = end_sample, ...)
                                selection_m2_back <- eval(parse(text = paste(selection, "(m2_back)")))
                                if (selection_minmax == "max") selection_m2_back <- (-1)*selection_m2_back
                            }
                            if (parsed_formula$kz %in% order_forth_ready) {
                                selection_m2_forth <- top_orders[which(order_forth_ready == parsed_formula$kz)[1], ncol(top_orders)]
                            } else {
                                m2_forth <- ardl(formula = formula, data = data, order = order2_forth, start = start_sample, end = end_sample, ...)
                                selection_m2_forth <- eval(parse(text = paste(selection, "(m2_forth)")))
                                if (selection_minmax == "max") selection_m2_forth <- (-1)*selection_m2_forth
                            }
                            selection2 <- min(selection_m2_back, selection_m2_forth)[1]
                            order2 <- list(order2_back, order2_forth)[which(c(selection_m2_back, selection_m2_forth) == min(selection_m2_back, selection_m2_forth))][[1]]
                            top_orders <- rbind(top_orders, c(order1, selection1), c(order2, selection2))
                            if (selection2 < selection1) {
                                selection1 <- selection2
                                order1 <- order2
                            } else {
                                ardl_converge <- TRUE
                            }
                        }
                    }
                }
            } else { # horizontal
                ardl_converge <- FALSE
                failed_orders <- data.frame(matrix(0, 1, parsed_formula$kz)) # initialization
                while (ardl_converge == FALSE) {
                    for(j in 1:parsed_formula$kx) {
                        if ((length(order_list[[j + 1]]) != 1) & (order1[j + 1] < max_order[j + 1])) { # if fixed order or max_order reached, don't search
                            order2_back <- order1
                            order2_forth <- order1
                            if (order1[j + 1] == 0) {
                                # order2_back is the order1
                                order2_forth[j + 1] <- order1[j + 1] + 1
                            } else {
                                order2_back[j + 1] <- order1[j + 1] - 1
                                order2_forth[j + 1] <- order1[j + 1] + 1
                            }
                            order_back_ready <- unlist(lapply(1:nrow(top_orders), FUN = function(z) sum(order2_back == top_orders[z,-ncol(top_orders)])))
                            order_forth_ready <- unlist(lapply(1:nrow(top_orders), FUN = function(z) sum(order2_forth == top_orders[z,-ncol(top_orders)])))
                            if (parsed_formula$kz %in% order_back_ready) {
                                selection_m2_back <- top_orders[which(order_back_ready == parsed_formula$kz)[1], ncol(top_orders)]
                            } else {
                                m2_back <- ardl(formula = formula, data = data, order = order2_back, start = start_sample, end = end_sample, ...)
                                selection_m2_back <- eval(parse(text = paste(selection, "(m2_back)")))
                                if (selection_minmax == "max") selection_m2_back <- (-1)*selection_m2_back
                            }
                            if (parsed_formula$kz %in% order_forth_ready) {
                                selection_m2_forth <- top_orders[which(order_forth_ready == parsed_formula$kz)[1], ncol(top_orders)]
                            } else {
                                m2_forth <- ardl(formula = formula, data = data, order = order2_forth, start = start_sample, end = end_sample, ...)
                                selection_m2_forth <- eval(parse(text = paste(selection, "(m2_forth)")))
                                if (selection_minmax == "max") selection_m2_forth <- (-1)*selection_m2_forth
                            }
                            selection2 <- min(selection_m2_back, selection_m2_forth)[1]
                            order2 <- list(order2_back, order2_forth)[which(c(selection_m2_back, selection_m2_forth) == min(selection_m2_back, selection_m2_forth))][[1]]
                            top_orders <- rbind(top_orders, c(order1, selection1), c(order2, selection2))
                            if (selection2 < selection1) {
                                selection1 <- selection2
                                order1 <- order2
                            } else {
                                # to converge when it reaches an order that has already failed
                                if (nrow(dplyr::distinct(rbind(failed_orders, order2), .keep_all = TRUE)) == nrow(failed_orders)) {
                                    ardl_converge <- TRUE
                                } else {
                                    failed_orders <- rbind(failed_orders, order2)
                                }
                            }
                        } else if (!(order1[j + 1] < max_order[j + 1])) { # to avoid infinite loop when all orders are maxed (the case where best mode is the bigest)
                            # Either all fixed or maxed (except the 1st) either order > max (in case of "both" where max = max + 1)
                            if ((all(((order1 == fixed_order) | (order1 == max_order))[-1] == TRUE)) | (order1[j + 1] > max_order[j + 1])) {
                                ardl_converge <- TRUE
                            }
                        }
                    }
                }
            }
        }

        # keep top 20 orders
        top_orders <- dplyr::as_tibble(data.frame(matrix(top_orders[,-ncol(top_orders)], ncol = ncol(top_orders)-1), selection = top_orders[,ncol(top_orders)]) %>%
            dplyr::distinct(.keep_all = TRUE) %>%
            dplyr::arrange(selection) %>% dplyr::slice(1:20))
    }

    # choose best order
    best_order <- top_orders[1, 1:(ncol(top_orders) - 1)] %>% as.numeric()
    names(best_order) <- parsed_formula$z_part$var
    # choose best model
    best_model <- ardl(formula = formula, data = data, order = best_order, start = start_sample, end = end_sample, ...)


    # prepare objects before exporting ----------------------------------------

    # rename top_orders columns
    temp <- names(top_orders)
    names(temp) <- c(parsed_formula$z_part$var, selection)
    top_orders <- top_orders %>%
        dplyr::rename(!!temp)
    # correct sign of the selection criterion
    if (selection_minmax == "max") {
        top_orders[,ncol(top_orders)] <- -top_orders[,ncol(top_orders)]
    }

    # return list
    return_list <- list(best_model = best_model, best_order = best_order,
                        top_orders = data.frame(top_orders))

    return(return_list)
}
