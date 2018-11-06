#' Title
#'
#' @param n
#' @param depth
#'
#' @return
#' @export
#'
#' @examples
appleUnif <- function(n,depth=2.5){
  apple <- TDA::sphereUnif(n,2)
  apple <- apple * sin(apple[,3]*depth)
  apple
}

#' Title
#'
#' @param n
#' @param curveness
#' @param length
#' @param width
#'
#' @return
#' @export
#'
#' @examples
swissUniff <- function(n,curveness=2,length=2,width=4){
  if(curveness<length) stop("curveness must be equal or larger than length")
  p = sqrt(curveness + length * seq(-1, 1 - 2/n, 2/n))
  y = width/2 * stats::runif(n, -1, 1)
  d_sr = cbind(p * cos(2*pi*p), y, p * sin(2*pi*p))
  d_sr
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
anulusUnif <- function(n,r.in=1,r.out=2){
  theta <- stats::runif(n,0,2*pi)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  cbind(r*cos(theta),r*sin(theta))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
vonMisesUnif <- function(){

}

#' Title
#'
#' @param n
#' @param height
#'
#' @return
#' @export
#'
#' @examples
cylinderUnif <- function(n,height=1){
  theta <- stats::runif(n,min = 0,max = 2*pi)
  l <- stats::runif(n,min = -height/2,height/2)
  cbind(cos(theta),sin(theta),l)
}

#' Title
#'
#' @param n
#' @param width
#'
#' @return
#' @export
#'
#' @examples
scurveUnif <- function(n,width=1){
  t <- 3*pi*stats::runif(n,-0.5,0.5)
  x <- sin(t)
  y <- width*stats::runif(n)
  z <- sign(t) * (cos(t)-1)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
sanulusUniff <- function(n,r.in=1,r.out=2){
  an <- anulusUnif(n,r.in = r.in,r.out = r.out)
  f <- function(x) ifelse(x>0,-3/4*pi*x+2*pi,3/4*pi*x+pi)
  t <- f(an[,1])*sign(an[,1])
  x <- sin(t)*sign(an[,1])
  z <- (cos(t)-1) - sign(an[,1])
  cbind(x,an[,2],z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
xplaneUnif <- function(n,r.in,r=1,r.out=2){
  t <- 4*pi*stats::runif(n,-0.5,0.5)
  x <- cos(t)
  y <- sin(t)
  z <- sign(t) * (cos(t*2)-1)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
xanulusUnif <- function(n,r.in=1,r.out=2){
  t <- 4*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- cos(t)*r
  y <- sin(t)*r
  z <- sign(t) * (cos(t*2)-1)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
torsionScurveUnif <- function(n,r.in=1,r.out=2){
  t <- 3*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- sign(t)*r-sign(t)*1.5
  y <- sin(t)*r
  z <- sign(t) * (cos(t)-1)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
strangeAnulusUnif <- function(n,r.in=1,r.out=2){
  t <- 2*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- cos(t)*r
  y <- sin(t)*r
  z <- sin(y) * (cos(t)) * sign(t)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
torsionAnulusUnif <- function(n,r.in=1,r.out=2){
  t <- 2*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- cos(t)*r
  y <- sin(t)*r
  z <- sin(y) * (cos(t))
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
twistAnulusUnif <- function(n,r.in=1,r.out=2){
  t <- 2*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- cos(t)*r * sin(t)^2
  y <- sin(t)*r
  z <- sin(y) * (cos(t))
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
aHoleRollUnif <- function(n,r.in=1,r.out=2){
  an <- anulusUnif(n,r.in,r.out)
  f <- function(x) 3/4*pi*x + pi/2
  t <- f(an[,1])*sign(an[,1])
  x <- sin(t)
  z <- sign(t) * (cos(t)-1)
  cbind(x,an[,2],z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
s3dUnif <- function(n,r.in=1,r.out=2){
  theta <- stats::runif(n,0,2*pi)
  t <- 3*pi*stats::runif(n,-0.5,0.5)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  x <- r*cos(t)*sin(t)
  y <- r*sin(t)
  z <- sign(t)*(cos(t)-1)
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
holledRollUnif <- function(n,r.in=1,r.out=2){
  an <- anulusUnif(n,r.in,r.out)
  f <- function(x) 3/4*pi*x + pi/2
  t <- f(an[,1])*sign(an[,1])+(an[,1]>0*pi)
  hist(t/pi)
  x <- sin(t)
  z <- sign(t) * (cos(t)-2)
  cbind(x,an[,2],z)
}

#' Title
#'
#' @param n
#' @param spin
#'
#' @return
#' @export
#'
#' @examples
helixAnulusUnif <- function(n,spin = 1){
  an <- anulusUnif(n)
  theta <- an[,1] * pi / 2 * spin
  return(cbind(an[,1],cos(theta)*an[,2],sin(theta)*an[,2]))
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
anulusUnif <- function(n,r.in=1,r.out=2){
  theta <- sort(stats::runif(n,0,2*pi))
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  cbind(r*cos(theta),r*sin(theta))
}

# spiral ------------------------------------------------------------------
#' Title
#'
#' @param theta
#' @param l
#'
#' @return
#' @export
#'
#' @examples
cylinder <- function(theta,l){
  x <- cos(theta)
  y <- sin(theta)
  z <- l
  cbind(x,y,z)
}

#' Title
#'
#' @param n
#' @param r.in
#' @param r.out
#'
#' @return
#' @export
#'
#' @examples
anulusReg <- function(n,r.in=1,r.out=2){
  theta <- seq(0,2*pi,length.out = n)
  r <- sqrt(2*stats::runif(n)/(2/(r.out^2-r.in^2))+r.in^2)
  cbind(r*cos(theta),r*sin(theta))
}

#' Title
#'
#' @param n
#' @param length
#' @param width
#'
#' @return
#' @export
#'
#' @examples
panelReg <- function(n,length=2,width=1){
  x <- stats::runif(n,min = -length/2,max = length/2)
  y <- stats::runif(n,min = -width/2,max = width/2)
  return(cbind(sort(x),y))
}
