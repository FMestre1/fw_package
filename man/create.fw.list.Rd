% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.fw.list.R
\name{create.fw.list}
\alias{create.fw.list}
\title{Download dataset from database}
\usage{
create.fw.list(
  db,
  folder = NULL,
  type = NULL,
  ecosyst = FALSE,
  ref = FALSE,
  spatial = FALSE,
  code = FALSE
)
}
\arguments{
\item{db}{Database: eb (EcoBase), gw (GlobalWeb), wl (Web of Life) and mg (Mangal)}

\item{folder}{Folder in the working directory to get the dataset files (db=gw and wl)}

\item{type}{If db=mg the user should provide the type of interactions to be downloaded}

\item{ecosyst}{Getting ecosystem information (only for db=gw, db=eb)}

\item{ref}{references information}

\item{spatial}{get spatial info (only for db=wl, db=eb and db=mg)}

\item{code}{To get the food web code}
}
\value{
A list of matrices
}
\description{
Download dataset from database
}
\examples{
#mg2 <- create.fw.list(db="mg", ref=TRUE, spatial=TRUE)
}
