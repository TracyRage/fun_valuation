% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valuation_funs.R
\name{get_amortization_5}
\alias{get_amortization_5}
\title{Capitalize R&D (-5 years)}
\usage{
get_amortization_5(cash_flows, amortized)
}
\arguments{
\item{amortized}{\strong{Boolean} Calculate amortized R&D}

\item{cashflows}{\strong{List of numbers} R&D spending the last 5 years}
}
\value{
\strong{Number} Amortized or unamortized portion of R&D
}
\description{
Capitalize R&D investments five years back (Light manufacturing, Aluminum, Auto Parts, Building Materials, Computer Peripherals, Gold/Silver, Mining, Office supplies, Petroleum, Textile, Rubber, Tabacco).
}
\examples{
# List of cash flows
# cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666)
# Calculate unamortized portion of R&D
# unamortized_rnd <- get_amortization_5(cash_flows=cash_flow, amortized=FALSE)
# Calculate amortized portion of R&D
# amortized_rnd <- get_amortization_5(cash_flows=cash_flow, amortized=TRUE)
}
