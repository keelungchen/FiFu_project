adjust_proportions <- function(numbers, value) {
  total <- sum(numbers)
  x <- value / total
  adjusted_proportions <- x * numbers
  return(adjusted_proportions)
}

# Calculate adjusted areal proportion after fi events increasing. Fi = adjusted ratio, a = original dataframe
ad_fi_prop <- function(Fi, a) {
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
  
  # Change fission rate by increasing proportion (ex. x1 ~X2)
  JFi_change = 0 *(Fi-1) # total change by adjustment
  
  A_IFiJ = IFiJ *Fi
  IFi_change = (IFiJ)*(Fi-1) # total change by adjustment
  
  A_CFiJ = CFiJ *Fi
  A_CFiI = CFiI *Fi
  A_CFiC = CFiC *Fi
  CFi_change = (CFiJ+CFiI+CFiC)*(Fi-1) # total change by adjustment
  
  
  # 調整 J 組的比例
  numbers_J <- c(JGJ, JGI, JFuI, JGC, JFuC, JFuJ, JFiI, JFiJ, JFiC)
  value_J <- sum(numbers_J) - JFi_change
  adjusted_proportions_J <- adjust_proportions(numbers_J, value_J)
  
  A_JGJ <- adjusted_proportions_J[1]
  A_JGI <- adjusted_proportions_J[2]
  A_JFuI <- adjusted_proportions_J[3]
  A_JGC <- adjusted_proportions_J[4]
  A_JFuC <- adjusted_proportions_J[5]
  A_JFuJ <- adjusted_proportions_J[6]
  A_JFiI <- adjusted_proportions_J[7]
  A_JFiJ <- adjusted_proportions_J[8]
  A_JFiC <- adjusted_proportions_J[9]
  A_JSJ <- JSJ # Shrinkage fixed when Fi increasing
  
  # 調整 I 組的比例
  numbers_I <- c(IGI, IFuJ, IFuC, IGC, IFuI, IFiC, IFiI)
  value_I <- sum(numbers_I) - IFi_change
  adjusted_proportions_I <- adjust_proportions(numbers_I, value_I)
  
  A_IGI <- adjusted_proportions_I[1]
  A_IFuJ <- adjusted_proportions_I[2]
  A_IFuC <- adjusted_proportions_I[3]
  A_IGC <- adjusted_proportions_I[4]
  A_IFuI <- adjusted_proportions_I[5]
  A_IFiC <- adjusted_proportions_I[6]
  A_IFiI <- adjusted_proportions_I[7]
  A_ISJ <- ISJ
  A_ISC <- ISC
  A_ISI <- ISI # Shrinkage fixed when Fi increasing
  
  # 調整 C 組的比例
  numbers_C <- c(CGC, CGI, CFuI, CFuC, CFuJ)
  value_C <- sum(numbers_C) - CFi_change
  adjusted_proportions_C <- adjust_proportions(numbers_C, value_C)
  
  A_CGC <- adjusted_proportions_C[1]
  A_CGI <- adjusted_proportions_C[2]
  A_CFuI <- adjusted_proportions_C[3]
  A_CFuC <- adjusted_proportions_C[4]
  A_CFuJ <- adjusted_proportions_C[5]
  A_CSJ <- CSJ
  A_CSI <- CSI
  A_CSC <- CSC # Shrinkage fixed when Fi increasing
  
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
