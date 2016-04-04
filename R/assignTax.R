#' Taxonomic Classification
#'
#' Assign taxonomic paths to unclassified SSU rRNA sequences, by executing \code{classify.seqs} in \href{http://www.mothur.org/}{Mothur} with the 'Wang' approach.
#' @usage assignTax(fasta, template = NULL, taxonomy = NULL, ksize = 8, iters = 100, cutoff = 80,
#' processors = 1, dir.out = "assignTax_out")
#'
#' @param fasta a fasta file of rRNA sequences to be assigned with taxonomies, e.g. a set of sequences picked as the representatives of OTUs.
#' @param template a faste file of rRNA reference sequences, default to download "SILVA_119.1_SSURef_tax_silva_trunc.fasta.gz" from SILVA archive, a representative set of SILVA rRNA references of version119 at 97\% sequence identity dow.
#' @param taxonomy a taxonomic path file mapping to the template file, default to "Silva_119_rep_set97_taxonomy.txt".
#' @param ksize,iters,cutoff,processors parameters used in \href{http://www.mothur.org/wiki/Classify.seqs}{Classify.seqs} by Mothur. \code{ksize}, kmer size which is a search option with the 'Wang' method and by default to 8.  \code{iters}, iterations by default 100 to calculate the bootstrap confidence score for the assigned taxonomy. \code{cutoff}, a bootstrap confidence score for the taxonomy assignment, by default 80, which means a minimum 80\% sequences were assigned by the same taxonomy, a higher value gives a more strict taxonomy assignment. \code{processors}, the number of central processing units you use to run the command, by default to 1.
#' @param dir.out a directory where the assigned files were outputted, by default to create \code{assignTax_out} directory and output assigned files under this directory.
#' @details This function performs 'classify.seqs' by running Mothur in command line mode, hence the executable Mothur on your computer is needed. For unix users, the absolute path of Mothur should be added to the PATH environmental variable and exported. For Windows users, the executable Mothur with extension .exe is required under your disks.
#'
#' @return two files under \code{dir.out}, a \code{*.taxonomy} file which contains a taxonomic path for each sequence and a \code{*.tax.summary} file which contains a taxonomic outline indicating the number of sequences that were found at each level (kingdom to species).
#' a list containing the following components:
#' exitStatus  an error code ('0' for success) given by the execution of the system Mothur commands, see \code{\link[base]{system}}.
#' stderr, stdout standard errors and outputs by executing Mothur command 'classify.seqs'.

assignTax = function(fasta, template=NULL, taxonomy=NULL, ksize = 8,
iters = 100, cutoff = 80, processors=1, dir.out = "assignTax_out") {
  #dir.out = "assignTax_out"
	if (!is.na(dir.out) | !is.null(dir.out) | !is.nan(dir.out)) {
		dir.create(dir.out, recursive = T, showWarnings = F)
	}
	
	#download the template file
	if(is.null(template)){
		file ="SILVA_119.1_SSURef_tax_silva_trunc.fasta.gz"
		t.file = paste("http://www.arb-silva.de/fileadmin/silva_databases/release_119_1",
		"Exports",file,sep="/")
		temp = tempfile()
		download.file(t.file,destfile = temp, method="curl",quiet=TRUE)
		gunzip(temp, "SILVA_119.1_SSURef_tax_silva_trunc.fasta")
		template = "SILVA_119.1_SSURef_tax_silva_trunc.fasta"
	}
	if(is.null(taxonomy)){
		taxonomy = system.file("Rep/Silva_119_rep_set97_taxonomy.txt",package="eudysbiome")
	}
	if (.Platform$OS.type == "unix") {
		exec = "mothur"
		exec.path = Sys.which(exec)
		if (exec.path == "") {
		stop("Executable mothur is not found.")
		}
		cmd1 = paste(exec.path, " \"#set.dir(output=", dir.out, ")", sep = "")
		}
		else{
			exec = paste(exec,".exe",sep="")
			disks = system("cmd.exe /c wmic logicaldisk caption", intern=TRUE)
			disks = gsub("(.*:).*","\\1",disks)
			matched = grepl(":",disks)
			disks = disks[matched]

		for(i in seq_along(disks)){
			cmd = paste("cmd.exe /c where /r ",disks[i], "\\ ", exec ,sep="")
			cmd.out = system(cmd,show.output.on.console=FALSE)
			if(cmd.out == 0){
				break
			}
		}
		if(cmd.out !=0){
			stop("Executable mothur is not found.")
		}
		else{
		exec.path=system(cmd,intern=TRUE)
		cmd1 = paste("cmd.exe /c ",exec.path, " \"#set.dir(output=", dir.out, ")", sep = "")
		}
		}

	stderr.file = tempfile(pattern = "stderr", fileext = as.character(Sys.getpid()))
	stdout.file = tempfile(pattern = "stdout", fileext = as.character(Sys.getpid()))

	cmd2 = paste("classify.seqs(fasta=", fasta,
	",template=", template,
	",taxonomy=", taxonomy,
	",ksize=", ksize,
	",iters=", iters,
	",cutoff=", cutoff,
	",processors=", processors,
	",probs=FALSE", ")\"", " 2> ", stderr.file, " > ", stdout.file, sep = "")

	cmds = paste(cmd1, cmd2, sep = "; ")

	retval = list()

	retval$exitStatus = system(cmds)

	#if (retval$exitStatus != 0) {
	#	stop("Error in mothur execution.")
	#}

	retval$stderr = readLines(stderr.file)
	retval$stdout = readLines(stdout.file)
	unlink(c(stdout.file, stderr.file))
	return(retval)

}
