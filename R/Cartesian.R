#' 2-D Cartesian plane Plots
#'
#' This function plots a Cartesian plane of genus abundance difference across the tested conditions (y-axis) and their harmful/harmless nature (negative/positive x-axis), giving rise to up-utmost right and bottom-utmost left quadrants of microbial eubiotic impact and bottom-utmost right and up-utmost left quadrants of dysbiotic impact.
#' @usage Cartesian(x,log2 = TRUE,micro.anno = NULL, comp.anno = NULL,
#'              pch = 16, point.col = NULL,point.alpha = 0.6,ylim = NULL, 
#'              xlab = NULL,ylab = NULL,vlty = 2, hlty = 1, srt = 60,
#'              font = 3, adj = c(1,1), xaxis = NULL, yaxis = NULL, legend = TRUE,
#'              box = TRUE,box.col = c("darkblue","yellow"),
#'              ...)
#'
#' @param x a data frame or numeric matrix of microbial abundance variations from which the plot is produced. Rows indicate the differential microbes, columns indicate the pair-wise conditions. \code{x} values can either be difference values or be \emph{log2} converted, specified with \code{log2} parameter.
#' @param log2 logical, specifying if x values should be \emph{log2} converted; default to \code{TRUE}.
#' @param micro.anno a character vector to annotate all row microbes in \code{x}; e.g. \code{"harmless"},\code{"harmful"}, should be in same length with the microbes. It can be given by the output of \code{\link{microAnnotate}}
#' @param comp.anno a character vector of conditions pre-defined from the column pair-wise comparisons, should be in same length with the comparisons; default to the pair-wise comparisons.
#' @param pch a vector of point types. Graphical parameters:see \code{\link[graphics]{par}}.
#' @param point.col a vector of colors for the points.
#' @param point.alpha alpha value for points; see \code{\link[grDevices]{adjustcolor}}.
#' @param ylim limits for the y axis.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param vlty,hlty types of vertical and horizontal lines to divide the plane with x-axis and y-axis, respectively.
#' @param srt,font,adj graphical parameters for the text on x-axis, see \code{\link[graphics]{par}}.
#' @param xaxis a character or expression vector specifying the labels of x axis by \emph{text}; default to row names of \code{x}.
#' @param yaxis a character or expression vector specifying the labels of y axis by \emph{axis}; default to \code{at} values in \code{\link[graphics]{axis}}.
#' @param legend logical, specifying if the legend should be added to the plot; default to \code{TRUE}.
#' @param box logical, specifying if the quadrants should be highlighted by boxes; default to \code{TRUE}.
#' @param box.col a vector of colors for the  up-utmost right, bottom-utmost left quadrants and bottom-utmost right, up-utmost left quadrants, respectively; default to "darkblue" and "yellow". If only one color is specified, the other one can be \code{NA}.
#' @param ... additional parameters passed to the default method, or by it to \code{\link[graphics]{plot.window}}, \code{\link[graphics]{text}},\code{\link[graphics]{mtext}},\code{\link[graphics]{axis}}, and \code{\link[graphics]{title}} to control the appearance of the plot.
#' @return The Cartesian plane plot
#' @export
#' @examples
#' data(microDiff)
#' attach(microDiff)
#'
#' newpar = par()
#' par(mar = c(6,5.1,4.1,6))
#' Cartesian(x = data,log2 = TRUE,micro.anno = micro.anno,pch = 16,
#'          comp.anno = comp.anno,point.col = c("blue","purple","orange"))
#'
#' par(newpar)
#' detach(microDiff)

Cartesian = function(x,log2 =TRUE,micro.anno=NULL, comp.anno=NULL,pch=16, point.col= NULL,
                           point.alpha =0.6, ylim=NULL, xlab=NULL,ylab=NULL,
                           vlty= 2, hlty = 1, srt =60, font=3, adj=c(1,1),
                           xaxis=NULL, yaxis=NULL, legend = TRUE, box=TRUE,
                           box.col =c("darkblue","yellow"),...)
{
  #check the input x
  if (!is.data.frame(x) && !is.matrix(x))
    stop("'x' must be a data frame or numeric matrix")

  if (is.null(colnames(x))) stop ("Comparison names are missing!")
  if (is.null(rownames(x))) stop ("Genera names are missing!")

  #micro.anno and comp.anno
  if (is.null(micro.anno)){
    stop ("Microbial annotations are missing")
  } else if(length(micro.anno) != nrow(x)){
    stop ("Microbe annotations don't match rows of x")
  } else {
    x$micro.anno= micro.anno
    ano = c("harmful","harmless")
    x = x[with(x,order(match(micro.anno,ano))),]

    micro.anno = x$micro.anno
    x = x[,-which(colnames(x) %in% "micro.anno")]
  }

  di <- dim(x)
  nr <- di[1]
  nc <- di[2]
  if(nr <= 1 || nc <= 1)
    stop("'x' must have at least 2 rows and 2 columns")

  x[is.na(x)]= NULL

  # log2 convert the abundanace difference
  if(log2){
    for (m in 1:nr){
      for ( n in 1:nc){
        if (x[m,n] ==0)
          x[m,n] = 0
        else if (x[m,n] < 0 )
          x[m,n]= -log(abs(as.numeric(x[m,n])),base =2)
        else
          x[m,n] = log(as.numeric(x[m,n]),base =2)
      }
    }
  }

  min = floor(range(x,na.rm=TRUE))[1]
  max = ceiling(range(x,na.rm=TRUE))[2]


  if (is.null(comp.anno)){
    comp.anno = colnames(x)
  } else if (length(comp.anno) != nc){
    stop ("Comparison pre-definations don't match columns of x")
  }


  # micro.tab <- table(factor(micro.anno,levels<-unique(micro.anno)))


  if(is.null(point.col)){
    point.col = adjustcolor(sample(colors(),nc),alpha.f = point.alpha)
  } else
    point.col =  adjustcolor(point.col,alpha.f =point.alpha)

  if (length(point.col) && length(point.col) != nc)
    stop ("Incorrect number of point.col")

  if (is.null(ylim))
    ylim <- c(min,max)

  # set the vertical and horizontal lines
  vline = length(micro.anno[micro.anno == "harmful"]) + 0.5
  hline <- 0

  if(is.null(xlab))
    xlab = c(rep("",nr))
  if (is.null(ylab))
    ylab =expression(paste('log'[2],Delta*g,sep=""))


  ## Output Graphics
  dev.hold()
  on.exit(dev.flush())
  
  #circle diameter is represented by log2-x values
  cex = abs(x)
  # par(mar=c(13,5.1,4.1,6), adj= 0.5, cex.axis= 1,cex.lab =1.1)

  plot(x[,1],ylim = ylim,pch =pch,cex = cex[,1],col = point.col[1], xaxt ="n",yaxt="n",las=0,xlab="", ylab=ylab,cex.lab=1.2,...)
  for(i in 2:nc){
    points(x[,i],cex=cex[,i],pch =pch,col= point.col[i])
  }
  abline(h=hline,v=vline,lty=c(hlty,vlty))

  #add ylabs
  if (!is.null(yaxis)){
    axis(2,at=yaxis,labels =yaxis,cex.axis=1.2,...)
  } else{
    axis(2,at=seq(0,min(ylim),-2),labels=seq(0,min(ylim),-2),cex.axis=1.2,...)
    axis(2,at=seq(0,max(ylim),2),labels=seq(0,max(ylim),2),cex.axis=1.2,...)
  }

  #add xlabs
  axis(1,at=seq(1,nr,1),labels=xlab)
  if(!is.null(xaxis)){
    text(seq(1,nr,1),par("usr")[3]-0.3,labels=xaxis,srt=srt,xpd=TRUE,font=font,adj=adj,cex=1.2,...)
  } else
    text(seq(1,nr,1),par("usr")[3]-0.3,labels=rownames(x),srt=srt,xpd=TRUE,font=font,adj=adj,cex=1.2,...)


  mtext("harmful",side=3,at= 1,adj=0,cex=1.3,...)
  mtext("harmless",side=3,at=nr, adj=1,cex=1.3,...)

  #add title or legend
  title(...)
  border = par("usr")
  if(legend){
    legend(border[2],border[4],legend=comp.anno,bty="n",col=point.col,pch=pch,xpd=TRUE,pt.cex=1.5,cex=1)
  }


  #darkblue box for the eubiotic impact and yellow box for the dysbiotic impact
  if(box){
  	 rect(border[1]+0.1,border[3]+0.1,vline-0.1,0-0.1,lwd=3.5,border=box.col[1])
    rect(vline+0.1,0+0.1,border[2]-0.1,border[4]-0.1,lwd=3.5,border= box.col[1])
    rect(border[1]+0.1,0+0.1,vline-0.1,border[4]-0.1,lwd=3.5,border= box.col[2])
    rect(vline+0.1,border[3]+0.1,border[2]-0.1,0-0.1,lwd=3.5,border= box.col[2])
  }
   }
