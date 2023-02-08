#' ARDL: ARDL, ECM and Bounds-Test for Cointegration
"_PACKAGE"

#'
#' @keywords internal
#' @docType package
#' @name ARDL-package
#'
#' @importFrom dplyr %>%
#' @importFrom zoo merge.zoo
#'
# no visible binding for global variable '.' (solution line)
globalVariables(c("."))

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(format(utils::citation("ARDL"), bibtex = FALSE)[[1]],
                      "\n\n",
                      format(utils::citation("ARDL"), bibtex = FALSE)[[2]],
                      "\n\n",
                      format(utils::citation("ARDL"), bibtex = FALSE)[[3]])
}
