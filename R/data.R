#' The Danish data on money income prices and interest rates
#'
#' This data set contains the series used by S. Johansen and K. Juselius for
#' estimating a money demand function of Denmark.
#'
#' An object of class "zooreg" "zoo".
#'
#' @format A time-series object with 55 rows and 5 variables. Time period from 1974:Q1
#' until 1987:Q3.
#' \describe{
#'   \item{LRM}{logarithm of real money, M2}
#'   \item{LRY}{logarithm of real income}
#'   \item{LPY}{logarithm of price deflator}
#'   \item{IBO}{bond rate}
#'   \item{IDE}{bank deposit rate}
#' }
#' @source \url{https://onlinelibrary.wiley.com/doi/10.1111/j.1468-0084.1990.mp52002003.x}
#' @references Johansen, S. and Juselius, K. (1990), Maximum Likelihood
#' Estimation and Inference on Cointegration -- with Applications to the Demand
#' for Money, \emph{Oxford Bulletin of Economics and Statistics}, \bold{52, 2},
#' 169--210.
#' @keywords datasets
"denmark"

#' The UK earnings equation data from Pesaran et al. (2001)
#'
#' This data set contains the series used by Pesaran et al. (2001) for estimating the UK earnings equation.
#' The clean format of the data retrieved from the Data Archive of Natsiopoulos and Tzeremes (2022).
#'
#' An object of class "zooreg" "zoo".
#'
#' @format A time-series object with 112 rows and 7 variables. Time period from 1970:Q1
#' until 1997:Q4.
#' \describe{
#'   \item{w}{real wage}
#'   \item{Prod}{labor productivity}
#'   \item{UR}{unemployment rate}
#'   \item{Wedge}{wedge effect}
#'   \item{Union}{union power}
#'   \item{D7475}{income policies 1974:Q1-1975:Q4}
#'   \item{D7579}{income policies 1975:Q1-1979:Q4}
#' }
#' @source \url{http://qed.econ.queensu.ca/jae/datasets/pesaran001/}
#' \url{http://qed.econ.queensu.ca/jae/datasets/natsiopoulos001/}
#' @references M. Hashem Pesaran, Richard J. Smith, and Yongcheol Shin, (2001), "Bounds Testing
#' Approaches to the Analysis of Level Relationships", \emph{Journal of Applied
#' Econometrics}, \bold{16, 3}, 289--326.
#'
#' Kleanthis Natsiopoulos and Nickolaos G. Tzeremes, (2022), "ARDL bounds test for
#' Cointegration: Replicating the Pesaran et al. (2001) Results for the
#' UK Earnings Equation Using R",
#' \emph{Journal of Applied Econometrics}, \bold{37, 5}, 1079--1090.
#' \doi{10.1002/jae.2919}
#' @keywords datasets
"PSS2001"

#' The UK earnings equation data from Natsiopoulos and Tzeremes (2022)
#'
#' This data set contains the series used by Natsiopoulos and Tzeremes (2022) for re-estimating the UK earnings equation.
#' The clean format of the data retrieved from the Data Archive of Natsiopoulos and Tzeremes (2022).
#'
#' An object of class "zooreg" "zoo".
#'
#' @format A time-series object with 196 rows and 9 variables. Time period from 1971:Q1
#' until 2019:Q4.
#' \describe{
#'   \item{time}{time variable}
#'   \item{w}{real wage}
#'   \item{Prod}{labor productivity}
#'   \item{UR}{unemployment rate}
#'   \item{Wedge}{wedge effect}
#'   \item{Union}{union power}
#'   \item{D7475}{income policies 1974:Q1-1975:Q4}
#'   \item{D7579}{income policies 1975:Q1-1979:Q4}
#'   \item{UnionR}{union membership}
#' }
#' @source \url{http://qed.econ.queensu.ca/jae/datasets/natsiopoulos001/}
#' @references Kleanthis Natsiopoulos and Nickolaos G. Tzeremes, (2022), "ARDL bounds test for
#' Cointegration: Replicating the Pesaran et al. (2001) Results for the UK Earnings Equation Using R",
#' \emph{Journal of Applied Econometrics}, \bold{37, 5}, 1079--1090.
#' \doi{10.1002/jae.2919}
#' @keywords datasets
"NT2022"
