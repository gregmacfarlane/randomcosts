#' Financial equivalence functions
#' 
#' @param P Present value
#' @param FV Future value (`F` is reserved by R)
#' @param A Annuity value
#' @param G Gradient annuity value
#' @param i Interest / discount rate in decimal terms; that is, a 12% discount
#'   rate is `i = 0.12`.
#' @param n Time periods
#' 
#' @return 
ptof <- function(P, i, n, k = 1){
  P * (1 + (i/k))^(n * k)
}

#' convert F to P
#' 
#' @param FV Future value
#' @param i interest rate per period, as a proportion (e.g. 4\% is 0.04)
#' @param n number of periods
#' 
#' @return P Present value
#' 
ftop <- function(FV, i, n){
  FV / (1 + i)^n
}

#' convert P to A
#' 
#' @param P Present value
#' @param i interest rate per period, as a proportion (e.g. 4\% is 0.04)
#' @param n number of periods
#' 
#' @return A Annuity value 
#' 
ptoa <- function(P, i, n, k = 1){
  P * ((i/k) * (1 + (i/k))^(n*k)) / ( (1 + (i/k))^(n*k) - 1)
}

#' Convert F to A
#' @param FV Future value
#' @param i interest rate per period, as a proportion (e.g. 4\% is 0.04)
#' @param n number of periods
#' 
#' @return A Annuity value 
#
ftoa <- function(FV, i, n){
  FV * i / ( (1+i)^n - 1)
}

#' Convert A to P
#' @param A Annuity value
#' @param i interest rate per period, as a proportion (e.g. 4\% is 0.04)
#' @param n number of periods
#' 
#' @return P Present value
#' 
atop <- function(A, i, n){
  A * (1 - 1/( (1 + i)^n)) / (i)
}

#' Convert A to F
#' @param A Annuity value
#' @param i interest rate per period, as a proportion (e.g. 4\% is 0.04)
#' @param n number of periods
#' 
#' @return F Future value
atof <- function(A, i, n){
  A*( (1 + i)^n - 1 ) / (i)
}

gtop <- function(G, i, n){
  G*( ((i + 1)^n - 1) / (i^2 * (1 + i)^n)  - n / (i*(1 + i)^n ) )
}
gtof <- function(G, i, n){
  G*(((1+i)^n-1)/(i^2)- n/i)
}
gtoa <- function(G, i, n){
  G*(1/i - n / ((1+i)^n-1))
}