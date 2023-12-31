% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microAnnotate.R
\name{microAnnotate}
\alias{microAnnotate}
\title{Genus Annotation}
\usage{
microAnnotate(microbe, species = TRUE, annotated.micro = NULL)
}
\arguments{
\item{microbe}{a genus list to be annotated; a Genus-Species data frame which represents the genera and the included corresponding species is recommended to be provided by users for the more accurate annotations, see \code{\link{tableSpecies}}.}

\item{species}{logical, specifying if the species are provided in the \code{microbe} for the annotations; default to \code{TRUE}.}

\item{annotated.micro}{the annotated genera which are used for the annotation of \code{microbe}, it could either be loaded from the data \code{harmGenera} or defined by users.}
}
\value{
The annotated genera.
}
\description{
Annotates given genera as harmful or harmless based on either our manually curated, harmful Genus-Species table in data \code{harmGenera} of this package or user defined table.
}
\examples{
#load the genera to be annotated
library(eudysbiome)
data(diffGenera)

#load the curated Genus-Species annotation table
data(harmGenera)

microAnnotate(microbe = diffGenera, species = TRUE,
           annotated.micro = harmGenera)
}

