#' The Danish data on money income prices and interest rates
#'
#' This data set contains the series used by S. Johansen and K. Juselius for
#' estimating a money demand function of Denmark.
#'
#' An object of class "zooreg" "zoo".
#'
#' @format A data frame with 55 rows and 5 variables. Time period from 1974:Q1
#' until 1987:Q3.
#' \describe{
#'   \item{LRM}{logarithm of real money, M2}
#'   \item{LRY}{logarithm of real income}
#'   \item{LPY}{logarithm of price deflator}
#'   \item{IBO}{bond rate}
#'   \item{IDE}{bank deposit rate}
#' }
#' @source \url{http://web.math.ku.dk/~sjo/data/danish_data.html}
#' @references Johansen, S. and Juselius, K. (1990), Maximum Likelihood
#' Estimation and Inference on Cointegration -- with Applications to the Demand
#' for Money, \emph{Oxford Bulletin of Economics and Statistics}, \bold{52, 2},
#' 169--210.
#' @keywords datasets
"denmark"