# Define R function for calculate change with increasing FiFu events 

# Adjust proportions function
adjust_proportions <- function(numbers, value) {
  total <- sum(numbers)
  x <- value / total
  adjusted_proportions <- x * numbers
  return(adjusted_proportions)
}


# Define the lamda_fi function
lamda_fi <- function(Fi) {
  A_IHJ <- IHJ + Fi
  IFi_change <- Fi
  
  A_CHJ <- CHJ + Fi
  CFi_change <- Fi
  
  A_JGJ <- JGJ
  A_JGI <- JGI
  A_JFuI <- JFuI
  A_JGC <- JGC
  A_JFuC <- JFuC
  A_JFuJ <- JFuJ
  A_JFiI <- JFiI
  A_JFiJ <- JFiJ
  A_JFiC <- JFiC
  A_JSJ <- JSJ
  
  numbers_I <- c(IGI, IFuJ, IFuC, IGC, IFuI, IFiC, IFiI, ISJ, ISC, ISI, IFiJ)
  value_I <- sum(numbers_I) - IFi_change
  adjusted_proportions_I <- adjust_proportions(numbers_I, value_I)
  
  A_IGI <- adjusted_proportions_I[1]
  A_IFuJ <- adjusted_proportions_I[2]
  A_IFuC <- adjusted_proportions_I[3]
  A_IGC <- adjusted_proportions_I[4]
  A_IFuI <- adjusted_proportions_I[5]
  A_IFiC <- adjusted_proportions_I[6]
  A_IFiI <- adjusted_proportions_I[7]
  A_ISJ <- adjusted_proportions_I[8]
  A_ISC <- adjusted_proportions_I[9]
  A_ISI <- adjusted_proportions_I[10]
  A_IFiJ <- adjusted_proportions_I[11]
  
  numbers_C <- c(CGC, CGI, CFuI, CFuC, CFuJ, CSJ, CSI, CSC, CFiJ, CFiI, CFiC)
  value_C <- sum(numbers_C) - CFi_change
  adjusted_proportions_C <- adjust_proportions(numbers_C, value_C)
  
  A_CGC <- adjusted_proportions_C[1]
  A_CGI <- adjusted_proportions_C[2]
  A_CFuI <- adjusted_proportions_C[3]
  A_CFuC <- adjusted_proportions_C[4]
  A_CFuJ <- adjusted_proportions_C[5]
  A_CSJ <- adjusted_proportions_C[6]
  A_CSI <- adjusted_proportions_C[7]
  A_CSC <- adjusted_proportions_C[8]
  A_CFiJ <- adjusted_proportions_C[9]
  A_CFiI <- adjusted_proportions_C[10]
  A_CFiC <- adjusted_proportions_C[11]
  
  # Leslie matrix
  mx_L <- matrix(c(
    A_JGJ*rJGJ + A_JSJ*rJSJ + A_JFiJ*rJFiJ + A_JFuJ*rJFuJ, A_ISJ*rISJ + A_IFuJ*rIFuJ + A_IFiJ*rIFiJ + IRJ*rIRJ + A_IHJ*rIHJ, A_CSJ*rCSJ + A_CFiJ*rCFiJ + A_CFuJ*rCFuJ + CRJ*rCRJ + A_CHJ*rCHJ,
    A_JGI*rJGI + A_JFuI*rJFuI + A_JFiI*rJFiI, A_IGI*rIGI + A_ISI*rISI + A_IFuI*rIFuI + A_IFiI*rIFiI, A_CGI*rCGI + A_CSI*rCSI + A_CFuI*rCFuI + A_CFiI*rCFiI,
    A_JGC*rJGC + A_JFiC*rJFiC + A_JFuC*rJFuC, A_IGC*rIGC + A_ISC*rISC + A_IFuC*rIFuC + A_IFiC*rIFiC, A_CGC*rCGC + A_CSC*rCSC + A_CFuC*rCFuC + A_CFiC*rCFiC
  ), nrow = 3, byrow = TRUE)
  
  # Calculate eigenvector and eigenvalue
  eigen_result <- eigen(mx_L)
  w <- eigen_result$values
  w_abs <- abs(w)
  order <- order(w_abs, decreasing = TRUE)
  lamda <- w[order[1]]
  
  return(lamda)
}
