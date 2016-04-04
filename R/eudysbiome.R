#' eudysbiome.
#'
#' @name eudysbiome
#' @docType package
#' @import plyr Biostrings
#' @importFrom R.utils gunzip getAbsolutePath
#' @importFrom Rsamtools indexFa FaFile seqinfo getSeq

NULL

#' Manually curated genera annotation table
#' A data frame containing 235 genera annotated as "harmful" and the harmful species included in these genera.
#'
#' @docType data
#' @keywords datasets
#' @name harmGenera
#' @usage data(harmGenera)
#' @format A data frame with 851 rows and 3 columns specifying for \code{Genus} and \code{Species} and the references.
NULL

#' Differential microbes in Genus-Species table
#' A data frame containing 10 differential genera and the species included, which was to be annotated as \code{"harmful"} or \code{"harmless"}.
#'
#' @docType data
#' @keywords datasets
#' @name diffGenera
#' @usage data(diffGenera)
#' @format A data frame with 26 rows and 2 columns specifying for \code{Genus} and \code{Species}.
NULL


#' Differential annotated genera with abundance variations among pair-wise condition comparisons
#'
#' A list containing: i) a data frame of 10 differentila genera with abundance differences among 3 condition comparisons, in which row represents the differential microbes and column represents the comparisons; ii) Genera annotations for the 10 differential genera; iii) pre-defined condition comparison names
#'
#' @docType data
#' @keywords datasets
#' @name microDiff
#' @usage data(microDiff)
#' @format A list
NULL


#' Microbial count contingency table
#'
#' A matrix containing the counts of differential microbe classified into each condition-eubiotic/dysbiotic impact couple. Rows represent the condition comparisons, columns represent the eubiotic and dysbiotic impacts:
#'
#' \itemize{
#'   \item EI. eubiotic impact
#'   \item DI. dysbiotic impact
#' }
#'
#'The table can be produced by \code{\link{microCount}} function.
#'
#' @docType data
#' @keywords datasets
#' @name microCount
#' @usage data(microCount)
#' @format A data frame with 2 rows and 2 variables
NULL
