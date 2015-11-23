#' Download SILVA SSU rRNA Sequences
#'
#' Download the SILVA Small subunit (16S/18S, SSU) ribosomal RNA (rRNA) reference (Ref) dataset for all three domains of life (Bacteria, Archaea and Eukarya) from \href{http://www.arb-silva.de/}{SILVA}, default to download a non-redundant (NR) SSU Ref dataset (v123) built by a dereplication of the full SSU Ref using a 99% identify criterion.
#' @usage seqDownload(file, version, destdir)
#'
#' @param file the name of the sequence file to be downloaded, default to \code{SILVA_123_SSURef_Nr99_tax_silva_trunc.fasta.gz}.
#' @param version a number or character to specify the version of the downloaded file, default to version \code{123}.
#' @param destdir a directory path where the downloaded file is saved, default to the current directory.
#' @details The Ref dataset is downloaded from SILVA Archive, please make sure the file you downloaded and the version you choose are listed in the archive.
#' @return The absolute path of the uncompressed downloaded file.
#' @export
#' @seealso \code{\link{seqExtract}}
#' @examples
#' #download the non-redundant SILVA SSU Ref(v123) dataset to the current directory
#' library("eudysbiome")
#' seqDownload()


seqDownload= function(file="SILVA_123_SSURef_Nr99_tax_silva_trunc.fasta.gz",version=123,
                      destdir="."){

  release = paste("release",version,sep="_")
  t.file = paste("http://www.arb-silva.de/fileadmin/silva_databases",
                 release,"Exports",file,sep="/")
  if(grepl("/$",destdir)){
    destdir = gsub("/$","",destdir)
  }
  dest.file = file.path(destdir,file)
  download.file(t.file,destfile = dest.file,method="curl")
  if(grepl("[.]tgz$",file)){
    untar(dest.file)
    dest.file = gsub("[.]tgz$","",dest.file)
  }
  else if(grepl("[.]gz$",file)){
    gunzip(dest.file)
    dest.file = gsub("[.]gz$","",dest.file)
  }
   destFile.path = getAbsolutePath(dest.file)
   return(destFile.path)
}
