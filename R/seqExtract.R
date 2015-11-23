#' Extract Matched Sequences
#'
#' Extract the sequences matched by the given sequence headers.
#' @usage seqExtract(seq.file, tax.file, as ="RNAStringSet", seqExtract.file)
#'
#' @param seq.file a sequence file where to extract the sequences.
#' @param tax.file a file with the headers of the extracted sequences. Note: the headers start behind '>' as shown in sequence file.
#' @param as a character vector indicating the type of object to return, including \code{"RNAStringSet"}, \code{"DNAStringSet"}, and \code{"AAStringSet"}, default to \code{"RNAStringSet"}, see \code{\link[Rsamtools]{scanFa}}.
#' @param seqExtract.file a sequence file where the extracted sequences are saved.
#' @details The function was specially used to extract sequences from the SILVA SSU rRNA Ref sequence dataset downloaded by \code{\link{seqDownload}}, based on the taxonomy mapping file with the extracted taxonomic paths, constructed by the \code{\link{taxExtract}} based on the given microbes.
#' @return the absolute path of \code{seqExtract.file}.
#' @export
#' @seealso \code{\link{seqDownload}},\code{\link{taxExtract}}
#' @examples
#' #not excute, extract sequences matched with taxonomic paths in taxExract.tax file
#' #seqExtract(seq.file = "SILVA_123_SSURef_Nr99_tax_silva_trunc.fasta", tax.file = "taxExtract.tax", 
#' #as = "RNAStringSet", seqExtract.file = "seqExtract.fasta")

seqExtract = function(seq.file,tax.file, as="RNAStringSet",seqExtract.file){

  # create a reference to an indexed seq.file
  indexFa(seq.file)
  fasta = FaFile(seq.file)
  info = as(seqinfo(fasta),"GRanges")

# read the tax.file
  tax = read.table(tax.file,header = F,as.is=T,sep="\t",quote="")
  index = pmatch(tax[,1],names(info))
  seq = getSeq(fasta, info[index],as=as)

  writeXStringSet(seq,seqExtract.file)
  seqExtractFile.path = getAbsolutePath(seqExtract.file)
  return(seqExtractFile.path)
}
