#' Genus Annotation
#' 
#' Annotates given genera as harmful or harmless based on either our manually curated, harmful Genus-Species table in data \code{harmGenera} of this package or user defined table.
#' @usage microAnnotate(microbe,species=TRUE, annotated.micro=NULL)
#'                  
#' @param microbe a genus list to be annotated; a Genus-Species data frame which represents the genera and the included corresponding species is recommended to be provided by users for the more accurate annotations.
#' @param species logical, specifying if the species are provided in the \code{microbe} for the annotations; default to \code{TRUE}.
#' @param annotated.micro the annotated genera which are used for the annotation of \code{microbe}, it could either be loaded from the data \code{harmGenera} or defined by users.
#' @return The annotated genera.
#' @export
#' @examples  
#' #load the genera to be annotated
#' data(diffGenera)
#' 
#' #load the curated Genus-Species annotation table
#' data(harmGenera)
#' 
#' microAnnotate(microbe = diffGenera, species =TRUE, 
#'            annotated.micro= harmGenera)


microAnnotate = function(microbe,species =TRUE,annotated.micro = NULL){
  
  if(is.null(annotated.micro)){
    annotated.micro = data(harmGenera)
  }
  
  if (species){
    if(!is.data.frame(microbe))
      microbe = data.frame(microbe)
    
    #annotate harmful genera matched to the annotated.micro table
    match = data.frame(lapply(1:ncol(microbe),function(i)intersect(microbe[,i],annotated.micro[,i])))
    colnames(match) = colnames(microbe)
    if (nrow(match) == 0)
      match
    else
      match$micro.anno = "harmful"
    
    #annotate harmless genera
    difference= data.frame(lapply(1:ncol(microbe),function(i)setdiff(microbe[,i],annotated.micro[,i])))
    colnames(difference) = colnames(microbe)
    difference$micro.anno ="harmless"
    
    #integrate all annotated genera
    annotated.microbe = rbind(match,difference)
    annotated.genera = unique(annotated.microbe[,c("Genus","micro.anno")])
  }
  else{
    
    if(is.data.frame(microbe))
      microbe = microbe$Genus
    
    match = intersect(microbe,unique(annotated.micro$Genus))
    match = data.frame(Genus = match, micro.anno ="harmful")
    
    difference = setdiff(microbe,unique(annotated.micro$Genus))
    difference = data.frame(Genus = difference,micro.anno="harmless")
    
    annotated.genera = rbind(match,difference)
  }  
  return (annotated.genera)
}
