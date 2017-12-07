



#create a S4 class "NR" 
setClass("NR",slots=list(f="function",method="character",tol="numeric",max.iter="numeric",
                         iterations="data.frame",num.iter="numeric",root="numeric",objective="numeric"))
#set 3 methods for class "NR": summary, print and plot

#write two functions to solve univariate equaitons f(x)=0 by Newton-Raphson method
#one function calculates derivative numerically; the other requires users to imput a derivative function 
#outputs of these two functions are in our defined class "NR" 

#system calculates derivative of f(x) numerically

#' A newton raphson Function
#'
#' This function allows you to calculate zeros of different functions
#' @keywords newton
#' @keywords raphson
#' @keywords roots
#' @export
#' @examples
#' newton.raphson()
newton.raphson <- function(f, x0, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  k <- n # Initialize for iteration results
  res<-data.frame(iter=0,root=x0, outcome=f(x0))
  if (f(x0) == 0.0) 
  { NR.res<-new("NR",f=f,method="Newton-Raphson with numeric derivative",
                tol=tol,max.iter=n,iterations=res,num.iter=0,root=x0,objective=f(x0))
  return(NR.res)}
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    res<-rbind(res,c(i,x1,f(x1)))
    
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) 
    { NR.res<-new("NR",f=f,method="Newton-Raphson with numeric derivative",
                  tol=tol,max.iter=n,iterations=res,num.iter=i,root=x1,objective=f(x1))
    return(NR.res)}
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0<-x1}
  warnings("Cannot find solution within maximum iteration number")
}

