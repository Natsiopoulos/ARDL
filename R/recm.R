#' Restricted ECM regression
#'
#' Creates the Restricted Error Correction Model (RECM). This is the conditional
#' RECM, which is the RECM of the underlying ARDL.
#'
#' Note that the statistical significance of 'L(ect, 1)' in a RECM should not be
#' tested using the corresponding t-statistic (or the p-value) because it
#' doesn't follow a standard t-distribution. Instead, the
#' \code{\link{bounds_t_test}} should be used.
#'
#' @param object An object of \code{\link[base]{class}} 'ardl' or 'uecm'.
#' @param case An integer from 1-5 or a character string specifying whether the
#'   'intercept' and/or the 'trend' have to participate in the short-run or the
#'   long-run relationship (cointegrating equation) (see section 'Cases' below).
#'
#' @return \code{recm} returns an object of \code{\link[base]{class}}
#'   \code{c("dynlm", "lm", "recm")}. In addition, attributes 'order', 'data',
#'   'parsed_formula' and 'full_formula' are provided.
#'
#' @section Mathematical Formula: The formula of a Restricted ECM conditional to
#'   an \eqn{ARDL(p,q_{1},\dots,q_{k})}{ARDL(p,q1,...,qk)} is: \deqn{\Delta
#'   y_{t} = c_{0} + c_{1}t + \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} +
#'   \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} +
#'   \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \pi_{y}ECT_{t} + \epsilon_{t}}
#' \describe{
#'   \item{Under Case 1:}{\itemize{
#'      \item \eqn{c_{0}=c_{1}=0}
#'      \item \eqn{ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})}}}
#'   \item{Under Case 2:}{\itemize{
#'      \item \eqn{c_{0}=c_{1}=0}
#'      \item \eqn{ECT = y_{t-1} - (\mu + \sum_{j=1}^{k}\theta_{j} x_{j,t-1})}}}
#'   \item{Under Case 3:}{\itemize{
#'      \item \eqn{c_{1}=0}
#'      \item \eqn{ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})}}}
#'   \item{Under Case 4:}{\itemize{
#'      \item \eqn{c_{1}=0}
#'      \item \eqn{ECT = y_{t-1} - (\delta(t-1)+ \sum_{j=1}^{k} \theta_{j} x_{j,t-1})}}}
#'   \item{Under Case 5:}{\itemize{
#'      \item \eqn{ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})}}}
#' }
#'
#' @section Cases: According to \cite{Pesaran et al. (2001)}, we distinguish the
#' long-run relationship (cointegrating equation) (and thus the bounds-test and
#' the Restricted ECMs) between 5 different cases. These differ in terms of
#' whether the 'intercept' and/or the 'trend' are restricted to participate in
#' the long-run relationship or they are unrestricted and so they participate in
#' the short-run relationship.
#'
#' \describe{
#'   \item{Case 1:}{\itemize{
#'     \item No \emph{intercept} and no \emph{trend}.
#'     \item \code{case} inputs: 1 or "n" where "n" stands for none.}}
#'   \item{Case 2:}{\itemize{
#'     \item Restricted \emph{intercept} and no \emph{trend}.
#'     \item \code{case} inputs: 2 or "rc" where "rc" stands for restricted
#'      constant.}}
#'   \item{Case 3:}{\itemize{
#'     \item Unrestricted \emph{intercept} and no \emph{trend}.
#'     \item \code{case} inputs: 3 or "uc" where "uc" stands for unrestricted
#'     constant.}}
#'   \item{Case 4:}{\itemize{
#'     \item Unrestricted \emph{intercept} and restricted \emph{trend}.
#'     \item \code{case} inputs: 4 or "ucrt" where "ucrt" stands for
#'     unrestricted constant and restricted trend.}}
#'   \item{Case 5:}{\itemize{
#'     \item Unrestricted \emph{intercept} and unrestricted \emph{trend}.
#'     \item \code{case} inputs: 5 or "ucut" where "ucut" stands for
#'      unrestricted constant and unrestricted trend.}}
#' }
#'
#' Note that you can't restrict (or leave unrestricted) a parameter that doesn't
#' exist in the input model. For example, you can't compute \code{recm(object,
#' case=3)} if the object is an ARDL (or UECM) model with no intercept. The same
#' way, you can't compute \code{bounds_f_test(object, case=5)} if the object is
#' an ARDL (or UECM) model with no linear trend.
#'
#' @section References: Pesaran, M. H., Shin, Y., & Smith, R. J. (2001). Bounds
#'   testing approaches to the analysis of level relationships. \emph{Journal of
#'   Applied Econometrics}, 16(3), 289-326
#'
#' @seealso \code{\link{ardl}} \code{\link{uecm}}
#' @author Kleanthis Natsiopoulos, \email{klnatsio@@gmail.com}
#' @keywords models ts
#' @export
#' @examples
#' data(denmark)
#'
#' ## Estimate the RECM, conditional to it's underlying ARDL(3,1,3,2) -----
#'
#' # Indirectly from an ARDL
#' ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
#' recm_3132 <- recm(ardl_3132, case = 2)
#'
#' # Indirectly from an UECM
#' uecm_3132 <- uecm(ardl_3132)
#' recm_3132_ <- recm(uecm_3132, case = 2)
#' identical(recm_3132, recm_3132_)
#' summary(recm_3132)
#'
#' ## Error Correction Term (ect) & Speed of Adjustment -------------------
#'
#' # The coefficient of the ect,
#' # shows the Speed of Adjustment towards equilibrium.
#' # Note that this can be also be obtained from an UECM,
#' # through the coefficient of the term L(y, 1) (where y is the dependent variable).
#' tail(recm_3132$coefficients, 1)
#' uecm_3132$coefficients[2]

recm <- function(object, case) {
    # no visible binding for global variable NOTE solution
    y <- NULL; rm(y)

    if (0 %in% object$order) {
        stop("RECMs that their underlying ARDL order contains at least one 0, are currently not supported. This will be fixed in future updates.",
            call. = FALSE)
    }
    class(object)[4] <- "recm_indicator"
    coint_eq_list <- coint_eq(object = object, case = case)
    design_matrix <- coint_eq_list$design_matrix
    coint_eq <- coint_eq_list$coint_eq
    data <- coint_eq_list$data
    parsed_formula <- coint_eq_list$parsed_formula
    case <- coint_eq_list$case
    order <- coint_eq_list$order
    recm_formula <- build_recm_formula(parsed_formula = parsed_formula, order = order, case = case)
    # error term, u
    u <- design_matrix %>%
        dplyr::mutate(u = y - coint_eq) %>%
        dplyr::select(u) %>%
        unlist() %>%
        stats::ts(., start = stats::start(data), frequency = stats::frequency(data))
    # create a new data table with the lagged error term u, naming the error correction term (ect)
    data_full <- zoo::cbind.zoo(data, stats::lag(u, -1))
    colnames(data_full) <- c(colnames(data), "ect")
    data <- data_full; rm(data_full)
    full_formula <- stats::formula(recm_formula$full)
    start <- start(object)
    end  <- end(object)

    recm_model <- dynlm::dynlm(full_formula, data = data, start = start, end = end)
    # for model compatibility in the global env
    attr(recm_model$terms, ".Environment") <- .GlobalEnv
    attr(attr(recm_model$model, "terms"), ".Environment") <- .GlobalEnv
    attr(full_formula, ".Environment") <- .GlobalEnv
    attr(recm_model, "class") <- c(class(recm_model), "recm")
    recm_model$order <- order
    recm_model$data <- data
    recm_model$parsed_formula <- parsed_formula
    recm_model$full_formula <- full_formula

    return(recm_model)
}
