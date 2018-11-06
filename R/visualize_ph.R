#' Title
#'
#' @param diag
#'
#' @return
#'
#' @examples
rawdiag <- function(diag){
  if(class(diag)=="list") return(diag$diagram)
  else return(diag)
}

#' Title
#'
#' @param diag
#' @param maxdimention
#' @param scale
#' @param point
#'
#' @return
#' @export
#'
#' @examples
showPersistentDiagram <- function(diag = diagram,maxdimention = NA,scale = NA,point = 1.5){
  diag <- rawdiag(diag)
  if(missing(maxdimention)) maxdimention <- attr(diag,"maxdimension")
  if(missing(scale)) scale <- attr(diag,"scale")
  maxdimention <- maxdimention + 1
  diag[is.infinite(diag)] <- scale[2]

  graphics::plot(diag[,2], diag[,3], xlim=scale, ylim=scale,
                 cex=point, cex.axis=point, col=diag[,1] + 1,
                 pch=diag[,1] + 1, xlab="Birth", ylab="Death",
                 cex.lab=point, cex.main=2)
  graphics::abline(0,1)

  legends <- 0
  for (i in (0:(maxdimention-1))){
    # legends <- c(legends,bquote(H[.(i)]))
    legends <- c(legends,paste("dim",i))
  }
  legends <- legends[-1]

  graphics::legend(scale[2]/2*1.2,scale[2]/2,legend=sapply(legends,as.expression),
                   col=1:maxdimention,pch=1:maxdimention,cex=point,pt.cex=point)
}


#' Title
#'
#' @param diag
#' @param band
#'
#' @return
#' @export
#'
#' @examples
showPhomBand <- function(diag = diagram,band){
  diag <- rawdiag(diag)
  max.scale <- attr(diag,"scale")[2]

  graphics::plot(1,type = "n",ylim = c(0,max.scale),xlim = c(0,max.scale),ann = F,axes = F)
  graphics::polygon(c(max.scale+1,max.scale-band+1,-1,-1),c(max.scale+1,max.scale+1,band-1,0-1),col = "pink",border = 0)
  graphics::par(new=T)
  showPersistentDiagram(diag)
}
