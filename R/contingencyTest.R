#' Contingency test for count data
#' 
#' Performs Chi-squared test or Fisher's exact test for testing the significance of association between conditions and eubiotic/dysbiotic impacts in a contingency table.
#' @usage contingencyTest(microCount, chisq = TRUE, fisher =TRUE,
#'                 alternative=c("greater"))
#' @param microCount a \emph{m by 2} data frame or numeric matrix of contingency table with frequencies under each condition-impact term; could be produced from \code{\link{contingencyCount}}.
#' @param chisq,fisher logical indicating if the Chi-squared test or Fisher's exact test should be performed.
#' @param alternative parameter specifying for alternative hypothesis, only used when \code{fisher} is \code{TRUE}; see \code{\link[stats]{fisher.test}}.
#' @details Chi-squared test for testing the probability that  the proportions of eubiotic frequencies are different between two conditions; furtherly, the Fisher's exact test for testing whether one condition is more likely to be associated to eubiotic impact. More details, refer to \code{\link[stats]{chisq.test}} and \code{\link[stats]{fisher.test}}
#' @return A list with following components:
#' Chisq Chi-squared test results for each pair-wise condition.
#' Chisq.p the p-values of the Chi-squared tests for all pair-wise conditions.
#' Fisher Fisher's exact test results for each pair-wise condition.
#' Fisher.p the p-values of the Fisher's exact tests for all pair-wise conditions.
#' @import 
#' @export
#' @seealso \code{\link{contingencyCount}}, \code{\link[stats]{fisher.test}}, \code{\link[stats]{chisq.test}}
#' @examples 
#' data(microCount)
#' 
#' contingencyTest(microCount,chisq=TRUE,fisher =TRUE,
#'            alternative ="greater")

contingencyTest = function(microCount, chisq = TRUE, fisher =TRUE,
                           alternative=c("greater"))
{ 
  if(!is.data.frame(microCount))
    microCount = data.frame(microCount)
  if ( nrow(microCount) == 2){
    condition.pair = cbind(microCount[1,],microCount[2,])
    rownames(condition.pair) = paste(rownames(microCount),collapse=":")
  }
  else {
    conditions = rownames(microCount)
    condition_pairs = combn(nrow(microCount),2)
    condition.pair = cbind(microCount[condition_pairs[1,],],microCount[condition_pairs[2,],])
    for ( i in 1:nrow(condition.pair)){
      row.names(condition.pair)[i] = paste(conditions[condition_pairs[,i]],collapse =":")  
    }
  }
  
  #choose the method before association test
 
  if(chisq)
  {
    chi.test = apply(condition.pair,1,function(x){
      sum = sum(x)
      y= matrix(x,nrow=2,byrow=TRUE)
      if( sum >= 40 && all(x >= 5))
        chisq.test(y)
      else if (sum >= 40 && all(x >=1) && any(x < 5))
        chisq.test(y,correct=TRUE) 
      else
        NA
    })
    
    chi.p = apply(condition.pair,1,function(x){
      y= matrix(x,nrow=2,byrow=TRUE)
      sum = sum(x)
      if( sum >= 40 && all(x >= 5))
        chisq.test(y)$p.value
      else if (sum >= 40 && all(x >=1) && any(x < 5))
        chisq.test(y,correct=TRUE)$p.value
      else
        NA
    })
    Chisq.p= data.frame("Chisq.Pvalue" = chi.p)  
  }
  
  
  if(fisher){
    fisher.test = apply(condition.pair,1,function(x){
      sum = sum(x)
      y= matrix(x,nrow=2,byrow=TRUE)
      fisher.test(y,alternative =alternative)
    })
    
    fisher.p = apply(condition.pair,1,function(x){
      y= matrix(x,nrow=2,byrow=TRUE)
      sum = sum(x)
      fisher.test(y,alternative =alternative)$p.value
    })
    name = paste("Fisher.Pvalue",alternative,sep="_")
    Fisher.p= data.frame(fisher.p)
    colnames(Fisher.p) = name
    
  }
  RESULT = list(Chisq = chi.test,Chisq.p = Chisq.p, Fisher = fisher.test,Fisher.p=Fisher.p)
  return (RESULT)
}