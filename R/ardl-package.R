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
	local_version = paste("R package version", utils::packageVersion('ARDL'))
	year <- sub("-.*", "", utils::packageDate('ARDL'))
	packageStartupMessage('To cite ARDL in publications use:
  \nKleanthis Natsiopoulos and Nickolaos Tzeremes (',
  year,
  '). ARDL: Autoregressive Distributed Lag Models, Error Correction Models and Bounds-Test for Cointegration. ',
  local_version,
  '. University of Thessaly, Department of Economics. https://github.com/Natsiopoulos/ARDL.')
}