#' Euclidean
#' 
#' Find the GCD of \code{a} and \code{b}.
#' 
#' @param a, a numeric number.
#' @param b, a numeric number.
#' @return The GCD of \code{a} and \code{b}.
#' @examples
#' euclidean(123612, 13892347912)    
#' euclidean(100, 1000)
#'   

euclidean<-function(a,b){
  if(!is.numeric(a) | !is.numeric(b)) stop("Input should be numerical!")
  #ma=max(a,b);mi=min(a,b)
  yu=a%%b;zheng=b;tem=0
  while(yu!=0){
    tem=yu
    yu=zheng%%yu
    zheng=tem
  }
  return (zheng)
}