% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_means.R
\name{compare_means}
\alias{compare_means}
\title{Compare estimated means}
\usage{
compare_means(
  object,
  specs,
  upper_margin,
  lower_margin = -upper_margin,
  method = c("sd", "eq"),
  level = 0.99
)
}
\arguments{
\item{object}{model}

\item{specs}{character}

\item{upper_margin}{numeric}

\item{method}{c("sd","eq","noi","sup")}

\item{level}{numeric}
}
\value{
em data.frame
}
\description{
Conduct Significance, Equivalence, Non-inferiority and Superiorty tests between means.
}
\examples{
data(CO2)
fm <- lm(uptake ~ Plant + conc, data = CO2)
compare_means(fm, "Plant", 15.0,
    method = c("sd", "eq"),
    level = 0.99)

}
