#' Construct a Genus-Species Data Frame
#'
#' This function is used to extract only Genus-Species data from the assigned taxonomic paths outputted by \code{\link{assignTax}} and construct a table containing the classified genera and species included in these genera correspondingly.
#' @usage tableSpecies(tax.file, microbe)
#' @param tax.file a taxonomy file with SSU rRNA sequence names and assigned taxonomic paths, see "*.taxonomy" file outputted by \code{\link{assignTax}}.
#' @param microbe a character vector specifying the genera used to construct the Genus-Species data frame
#' @return a Genus-Species data frame, only with the genera specified by \code{microbe} and the included corresponding species .
#' @details The outputted Genus-Species table can be used as input for the more accurate genus annotation, which annotates genera as \code{harmful} or \code{harmless} based on their ability to contribute to mammals' host diseases by \code{\link{microAnnotate}}.
#' @export
#' @examples
#' #a table with "Lactobacillus" and "Bacteroids" genera and the included species
#' genera = c("Lactobacillus","Bacteroides")
#' #not excute
#' #tableSpecies(tax.file = "test.taxExtract.wang.taxonomy", microbe = genera)


tableSpecies = function(tax.file,microbe){
  tax =read.table(tax.file,as.is=T,sep="\t")
  colnames(tax) = c("SeqID","Tax")

  split = strsplit(as.character(tax$Tax),";")
  tax.all = do.call(rbind,split)
  tax.all = data.frame(tax.all,stringsAsFactors = F)
  colnames(tax.all) = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")

  #replace "_" to " " in species column
  tax.all$Species = gsub("(_)([[:alpha:]])", " \\U\\2", tax.all$Species, perl=TRUE)

  #get genus-species data frame for the differential microbes
  species = tax.all[,c("Genus","Species")]
  species = unique(species)

  #filter the ambiguous classified species
  iterms = c("nculture","Sp[.]", " Gut Metagenome","nidentified","Bacterium_","nclassified","^bacterium"," fecal$")
  matches = grepl(paste(iterms,collapse="|"), species$Species)
 
  species = species[!matches,]
  species$Species = sapply(species$Species, function(x){
  	split = unlist(strsplit(x," "))
  	len = length(split)
  	if(len == 1){
  		y = split[[1]]
  	}
  	else{
  		y = paste( split[1:2],collapse =" ")
  	}
  	return(y)
  	})

  diffSpecies = species[species$Genus %in% microbe,]
  unmatched = setdiff(microbe, diffSpecies$Genus)
  if(length(unmatched) != 0){
  	unmatched_genus = data.frame(Genus=unmatched,Species=NA)
  }
  else{
  	unmatched_genus = NULL
  	}
  
  diffSpecies = rbind(diffSpecies,unmatched_genus)
  diffSpecies$Species = gsub("( )([[:upper:]])","\\1\\L\\2",diffSpecies$Species,perl=TRUE)
  rownames(diffSpecies) = NULL
  
  matches = grepl(paste(iterms,collapse="|"), diffSpecies$Species)
  diffSpecies = diffSpecies[!matches,]
  diffSpecies = unique(diffSpecies)
  
  return(species=diffSpecies)
}


