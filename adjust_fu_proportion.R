adjust_proportions <- function(numbers, value) {
  total <- sum(numbers)
  x <- value / total
  adjusted_proportions <- x * numbers
  return(adjusted_proportions)
}

# Calculate adjusted areal proportion after fu events increasing. Fu = adjusted ratio, a = original dataframe
ad_fu_prop <- function(Fu, a) {
  # 從 a 中提取原始值
  JGJ <- a["GJ", "J"]
  JGI <- a["GI", "J"]
  JGC <- a["GC", "J"]
  JSJ <- a["SJ", "J"]
  JSI <- a["SI", "J"]
  JSC <- a["SC", "J"]
  JFiJ <- a["FiJ", "J"]
  JFiI <- a["FiI", "J"]
  JFiC <- a["FiC", "J"]
  JFuJ <- a["FuJ", "J"]
  JFuI <- a["FuI", "J"]
  JFuC <- a["FuC", "J"]
  JMJ <- a["mort", "J"]
  JRJ <- a["rec", "J"]
  
  IGJ <- a["GJ", "I"]
  IGI <- a["GI", "I"]
  IGC <- a["GC", "I"]
  ISJ <- a["SJ", "I"]
  ISI <- a["SI", "I"]
  ISC <- a["SC", "I"]
  IFiJ <- a["FiJ", "I"]
  IFiI <- a["FiI", "I"]
  IFiC <- a["FiC", "I"]
  IFuJ <- a["FuJ", "I"]
  IFuI <- a["FuI", "I"]
  IFuC <- a["FuC", "I"]
  IMI <- a["mort", "I"]
  IRJ <- a["rec", "I"]
  
  CGJ <- a["GJ", "C"]
  CGI <- a["GI", "C"]
  CGC <- a["GC", "C"]
  CSJ <- a["SJ", "C"]
  CSI <- a["SI", "C"]
  CSC <- a["SC", "C"]
  CFiJ <- a["FiJ", "C"]
  CFiI <- a["FiI", "C"]
  CFiC <- a["FiC", "C"]
  CFuJ <- a["FuJ", "C"]
  CFuI <- a["FuI", "C"]
  CFuC <- a["FuC", "C"]
  CMC <- a["mort", "C"]
  CRJ <- a["rec", "C"]
  
  # Change fusion rate by increasing proportion
  A_JFuI <- JFuI * Fu
  A_JFuC <- JFuC * Fu
  J_Fu_change <- (JFuI + JFuC) * (Fu - 1)
  
  A_IFuI <- IFuI * Fu
  A_IFuC <- IFuC * Fu
  I_Fu_change <- (IFuI + IFuC) * (Fu - 1)
  
  A_CFuC <- CFuC * Fu
  C_Fu_change <- CFuC * (Fu - 1)
  
  # Adjust the other proportion of fates for J
  numbers_J <- c(JFiI, JSJ, JFuJ, JFiJ, JFiC)
  value_J <- sum(numbers_J) - J_Fu_change
  adjusted_proportions_J <- adjust_proportions(numbers_J, value_J)
  
  A_JFiI <- adjusted_proportions_J[1]
  A_JSJ <- adjusted_proportions_J[2]
  A_JFuJ <- adjusted_proportions_J[3]
  A_JFiJ <- adjusted_proportions_J[4]
  A_JFiC <- adjusted_proportions_J[5]
  A_JGJ <- JGJ
  A_JGI <- JGI
  A_JGC <- JGC
  
  # Adjust the other proportion of fates for I
  numbers_I <- c(ISJ, IFiJ, ISC, ISI, IFiI, IFuJ, IFiC)
  value_I <- sum(numbers_I) - I_Fu_change
  adjusted_proportions_I <- adjust_proportions(numbers_I, value_I)
  
  A_ISJ <- adjusted_proportions_I[1]
  A_IFiJ <- adjusted_proportions_I[2]
  A_ISC <- adjusted_proportions_I[3]
  A_ISI <- adjusted_proportions_I[4]
  A_IFiI <- adjusted_proportions_I[5]
  A_IFuJ <- adjusted_proportions_I[6]
  A_IFiC <- adjusted_proportions_I[7]
  A_IGI <- IGI
  A_IGC <- IGC
  
  # Adjust the other proportion of fates for C
  numbers_C <- c(CSJ, CFiJ, CSI, CSC, CFiI, CFiC, CFuI, CFuJ)
  value_C <- sum(numbers_C) - C_Fu_change
  adjusted_proportions_C <- adjust_proportions(numbers_C, value_C)
  
  A_CSJ <- adjusted_proportions_C[1]
  A_CFiJ <- adjusted_proportions_C[2]
  A_CSI <- adjusted_proportions_C[3]
  A_CSC <- adjusted_proportions_C[4]
  A_CFiI <- adjusted_proportions_C[5]
  A_CFiC <- adjusted_proportions_C[6]
  A_CFuI <- adjusted_proportions_C[7]
  A_CFuJ <- adjusted_proportions_C[8]
  A_CGC <- CGC
  A_CGI <- CGI
  
  
  # 創建並返回一個包含計算結果的 data.frame
  a_a <- data.frame()
  
  a_a["GJ", "J"] <- A_JGJ
  a_a["GI", "J"] <- A_JGI
  a_a["GC", "J"] <- A_JGC
  a_a["SJ", "J"] <- A_JSJ
  a_a["SI", "J"] <- JSI
  a_a["SC", "J"] <- JSC
  a_a["FiJ", "J"] <- A_JFiJ
  a_a["FiI", "J"] <- A_JFiI
  a_a["FiC", "J"] <- A_JFiC
  a_a["FuJ", "J"] <- A_JFuJ
  a_a["FuI", "J"] <- A_JFuI
  a_a["FuC", "J"] <- A_JFuC
  a_a["mort", "J"] <- JMJ
  a_a["rec", "J"] <- JRJ
  
  a_a["GJ", "I"] <- IGJ
  a_a["GI", "I"] <- A_IGI
  a_a["GC", "I"] <- A_IGC
  a_a["SJ", "I"] <- A_ISJ
  a_a["SI", "I"] <- A_ISI
  a_a["SC", "I"] <- A_ISC
  a_a["FiJ", "I"] <- A_IFiJ
  a_a["FiI", "I"] <- A_IFiI
  a_a["FiC", "I"] <- A_IFiC
  a_a["FuJ", "I"] <- A_IFuJ
  a_a["FuI", "I"] <- A_IFuI
  a_a["FuC", "I"] <- A_IFuC
  a_a["mort", "I"] <- IMI
  a_a["rec", "I"] <- IRJ
  
  a_a["GJ", "C"] <- CGJ
  a_a["GI", "C"] <- A_CGI
  a_a["GC", "C"] <- A_CGC
  a_a["SJ", "C"] <- A_CSJ
  a_a["SI", "C"] <- A_CSI
  a_a["SC", "C"] <- A_CSC
  a_a["FiJ", "C"] <- A_CFiJ
  a_a["FiI", "C"] <- A_CFiI
  a_a["FiC", "C"] <- A_CFiC
  a_a["FuJ", "C"] <- A_CFuJ
  a_a["FuI", "C"] <- A_CFuI
  a_a["FuC", "C"] <- A_CFuC
  a_a["mort", "C"] <- CMC
  a_a["rec", "C"] <- CRJ
  
  return(round(a_a,3)) 
}