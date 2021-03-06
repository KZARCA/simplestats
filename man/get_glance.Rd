% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glance.R
\name{get_glance}
\alias{get_glance}
\alias{get_glance.default}
\alias{get_glance.lm}
\alias{get_glance.coxph}
\alias{get_glance.mira}
\alias{get_glance_ba}
\title{Get additional info on a test performed}
\usage{
get_glance(x, ...)

\method{get_glance}{default}(x, y, survival = FALSE, censure = NULL)

\method{get_glance}{lm}(x)

\method{get_glance}{coxph}(x)

\method{get_glance}{mira}(x)

get_glance_ba(x, y)
}
\arguments{
\item{x}{The dependant variable}

\item{...}{Further arguments passed to or from other methods.}

\item{y}{Either the independant variable if survival = FALSE or follow-up time}

\item{survival}{logical, is it a survival analysis}

\item{censure}{The censor variable}
}
\description{
Get additional info on a test performed
}
