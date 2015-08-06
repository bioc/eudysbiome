#' Contingency Table Construction
#' 
#' Computes the frequencies of the contingency table as the accumulated microbial abundance difference classified into each condition and eubiotic/dysbiotic impact term for examining the significance of the association (contingency) between conditions and impacts by  \code{\link{contingencyTest}}.
#' @usage contingencyCount(x, micro.anno=NULL, comp.anno=NULL)
#'                  
#' @param x See \code{x} in \code{\link{pseudoCartesian}}, the x values should be difference values without log converted.
#' @param micro.anno See \code{micro.anno} in \code{\link{pseudoCartesian}}.
#' @param comp.anno See \code{comp.anno} in \code{\link{pseudoCartesian}}.
#' @details Eubiotic impact is measured by variations of increased harmless and decreased harmful microbes, while the dysbiotic impact is measured by the decreased harmless and increased harmful microbes.
#' @return The frequencies of condition-impact terms in contingency table
#' @export
#' @examples 
#' data(microDiff)
#' attach(microDiff)
#' 
#' microCount = contingencyCount(x=data, micro.anno=micro.anno, 
#'                       comp.anno = comp.anno)
#'                 
#' detach(microDiff)


contingencyCount = function(x, micro.anno=NULL,comp.anno =NULL)
{
  
  if (!is.data.frame(x) && !is.matrix(x))
    stop("'x' must be a data frame or numeric matrix.")
  
  if (is.null(comp.anno))
    stop ("Comparsion annotations are missing!")
  else if (length(comp.anno) != ncol(x))
    stop ("Comparison annotations don't match columns of 'x'.")
  
  
  if (is.null(micro.anno)) 
    stop ("Microbial annotations are missing")
  else if (length(micro.anno) != nrow(x))
    stop ("Microbe annotations don't match rows of x")
  
  if ("unknown" %in% micro.anno){
    x$micro.anno = micro.anno
    x= x[x$micro.anno != "unknown",]
  }
  else
    x$micro.anno = micro.anno
  
  colnames(x) = c(comp.anno,"micro.anno") 
  
  ## make the count matrix for the association test
  microCount = matrix(NA,length(comp.anno),2)
  rownames(microCount) = comp.anno
  colnames(microCount) = c("EI","DI")
  
    for(i in 1:nrow(microCount))
    {
      pc1 = x[x$micro.anno == "harmless",colnames(x) %in% rownames(microCount)[i]]
      pc2 = x[x$micro.anno == "harmful",colnames(x) %in% rownames(microCount)[i]]
      pcount1 = sum(pc1[pc1>0],na.rm=TRUE)
      pcount2 =abs(sum(pc2[pc2<0],na.rm =TRUE))
      microCount[i,"EI"] = pcount1 + pcount2
      ncount1 = abs(sum(pc1[pc1 < 0],na.rm=TRUE))
      ncount2 =sum(pc2[pc2 >0],na.rm =TRUE)
      microCount[i,"DI"] = ncount1 + ncount2
    }
    return(microCount) 
}
