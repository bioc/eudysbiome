#' Extract Matched Taxonomic Paths
#'
#' Extract the taxonomic paths from the taxonomy mapping file by pattern matching with given microbes.
#' @usage taxExtract(tax.file, microbe, taxExtract.file)
#'
#' @param tax.file a taxonomy mapping file with taxonomic paths, downloaded by \code{\link{taxDownload}}.
#' @param microbe a character vector of selected microbes, classified at any taxonomic rank of kingdom, phylum, class, order, family, genus or species.
#' @param taxExtract.file a taxonomy file where the extracted taxonomic paths are saved.
#' @return the absolute path of the \code{taxExtract.file}
#' @export
#' @seealso \code{\link{taxDownload}}, \code{\link{seqExtract}}
#' @examples
#' #extract taxonomic paths matched to "Firmutes"
#' phyla = "Firmutes"
#' #not excute
#' #taxExtract(tax.file = "taxmap_slv_ssu_123.txt", microbe = phyla, taxExtract.file = "taxExtract.tax")
#'
#' #extract taxonomic paths matched to "Lactobacillus" and "Bacteroides" genera
#' genera = c("Lactobacillus","Bacteroides")
#' #not excute
#' #taxExtract(tax.file = "taxmap_slv_ssu_123.txt", microbe = genera, taxExtract.file = "taxExtract.tax")


taxExtract = function(tax.file, microbe, taxExtract.file){

  tax =read.csv(tax.file,header=F,as.is=T,sep="\t")

  #construct a tax table consistent with the sequence id in fasta file
  tax$ID = do.call(paste, c(tax[,1:3], sep="."))
  tax$Taxon <- do.call(paste, c(tax[,4:5], sep=""))

  tax$Taxon = gsub(" ","_",tax$Taxon)
  tax$Taxon = paste(tax$Taxon,";",sep="")
  tax = tax[,c("ID","Taxon")]

  microbes = paste(microbe,collapse="|")
  matchtax = lapply(tax$Taxon, function(x) grepl(microbes,x) )
  out.tax = tax[matchtax ==TRUE,]

  write.table(out.tax, file=taxExtract.file, quote=FALSE, col.names=FALSE, row.names=FALSE,sep="\t")
  taxExtractFile.path = getAbsolutePath(taxExtract.file)

  return(taxExtractFile.path)
}
