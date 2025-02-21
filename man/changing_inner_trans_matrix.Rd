% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inner-transition-functions.R
\name{changing_inner_trans_matrix}
\alias{changing_inner_trans_matrix}
\title{takes a starter inner_transition_matrix and creates a list of future matrices based on their scalar change}
\usage{
changing_inner_trans_matrix(
  inner_trans_matrix_list,
  from_cs,
  to_cs,
  scalar_change,
  over_n_iterations = 1,
  total_time = 20,
  method = "take from no change",
  warnings = T
)
}
\arguments{
\item{inner_trans_matrix_list}{the original inner transition matrix}

\item{from_cs}{the core segment starting from}

\item{to_cs}{the core segment transitioning to}

\item{scalar_change}{how much to scale by, a value of 0.9 is a change to 90\% of what it was. Can't be below 0.}

\item{over_n_iterations}{over how many discrete time steps for the change to happen over. Defaults to 1 ie immediately.}

\item{total_time}{discrete time steps - ie how many transition matrices to produce + 1}

\item{method}{either "take from no change" or "take proportionally from other changes" - how to get the
proportions to add back to 1}

\item{warnings}{boolean whether to output warnings --> default is TRUE}
}
\value{
the set of inner transitions
}
\description{
takes a starter inner_transition_matrix and creates a list of future matrices based on their scalar change
}
