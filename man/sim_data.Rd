% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_data.R
\name{sim_data}
\alias{sim_data}
\title{Simulate the hidden variables X and observable variables Y for particle filters.
The observable variables (observation process) are related to
the hidden variables (state-process) by gaussian distribution in this simulation.}
\usage{
sim_data(time, noise, relation)
}
\arguments{
\item{time}{The number of observations for data simulated}

\item{noise}{The noise of X}

\item{relation}{The relation of X}
}
\value{
A list of simulated X and Y vector, each with `length = time`
}
\description{
Simulate the hidden variables X and observable variables Y for particle filters.
The observable variables (observation process) are related to
the hidden variables (state-process) by gaussian distribution in this simulation.
}
\examples{
sim_data(50,0.4,1.2)
x_true = sim_data(50,0.4,1.2)$x
obs = sim_data(50,0.4,1.2)$y
}
