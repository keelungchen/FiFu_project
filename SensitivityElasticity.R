# Sensitivity and Elasticity
library(popbio)
library(matlib)

a <- read.csv("data/areal_circ_even.csv", row.names=1)
m <- read.csv("data/multiplier_circ_even.csv", row.names=1)

# Fate of Juvs
mJJ <- a["GJ", "J"] * m["GJ", "J"] + a["SJ", "J"] * m["SJ", "J"] + a["FiJ", "J"] * m["FiJ", "J"] + a["FuJ", "J"] * m["FuJ", "J"] + a["mort", "J"] * m["mort", "J"] + a["rec", "J"] * m["rec", "J"]
mIJ <- a["GI", "J"] * m["GI", "J"] + a["SI", "J"] * m["SI", "J"] + a["FiI", "J"] * m["FiI", "J"] + a["FuI", "J"] * m["FuI", "J"]
mCJ <- a["GC", "J"] * m["GC", "J"] + a["SC", "J"] * m["SC", "J"] + a["FiC", "J"] * m["FiC", "J"] + a["FuC", "J"] * m["FuC", "J"]

# Fate of irregular adults 
mJI <- a["GJ", "I"] * m["GJ", "I"] + a["SJ", "I"] * m["SJ", "I"] + a["FiJ", "I"] * m["FiJ", "I"] + a["FuJ", "I"] * m["FuJ", "I"] + a["rec", "I"] * m["rec", "I"]
mII <- a["GI", "I"] * m["GI", "I"] + a["SI", "I"] * m["SI", "I"] + a["FiI", "I"] * m["FiI", "I"] + a["FuI", "I"] * m["FuI", "I"] + a["mort", "I"] * m["mort", "I"]
mCI <- a["GC", "I"] * m["GC", "I"] + a["SC", "I"] * m["SC", "I"] + a["FiC", "I"] * m["FiC", "I"] + a["FuC", "I"] * m["FuC", "I"]

# Fate of compact adults 
mJC <- a["GJ", "C"] * m["GJ", "C"] + a["SJ", "C"] * m["SJ", "C"] + a["FiJ", "C"] * m["FiJ", "C"] + a["FuJ", "C"] * m["FuJ", "C"] + a["rec", "C"] * m["rec", "C"]
mIC <- a["GI", "C"] * m["GI", "C"] + a["SI", "C"] * m["SI", "C"] + a["FiI", "C"] * m["FiI", "C"] + a["FuI", "C"] * m["FuI", "C"]
mCC <- a["GC", "C"] * m["GC", "C"] + a["SC", "C"] * m["SC", "C"] + a["FiC", "C"] * m["FiC", "C"] + a["FuC", "C"] * m["FuC", "C"] + a["mort", "C"] * m["mort", "C"]

# Transition matrix
M_circ <- matrix(c(mJJ, mJI, mJC, 
              mIJ, mII, mIC, 
              mCJ, mCI, mCC), 3, 3, byrow=TRUE)


lam <- Re(eigen(M)$values[1])


w <- eigen(M)$vectors  # right eigen vectors
v <- Conj(solve(w))    # complex conjugate of left eigen vectors

# Transition matrix in high Fi and Fu environment - P/A ratio
#M_high_Fi <- matrix(c(0.56936, 0.10982, 0.11782, 
#              1.29343, 0.65344, 0.20225, 
 #             0.0271, 0.28581, 0.63951), 3, 3, byrow=TRUE)
#M_high_Fu <- matrix(c(0.4812, 0.07639, 0.10588, 
  #            1.49506, 0.73031, 0.21219, 
   #           0.0271, 0.4223, 0.68827), 3, 3, byrow=TRUE)

#Calculate sensitivity matrix  
senmat <- Re(v[1,] %*% t(w[,1])) 
senmat  

#Calculate elasticity matrix
emat <- (1/(Re(eigen(M)$values[1]))) * senmat * M
emat

M_sens <- round(Re(sensitivity(M)),4)
M_elas <- round(Re(elasticity(M)),4)
M_sens
M_elas

#Or use function in popbio
M_pa_sens <- Re(sensitivity(M_pa))
M_pa_elas <- Re(elasticity(M_pa))

M_size_sens <- Re(sensitivity(M_size))
M_size_elas <- Re(elasticity(M_size))

M_circ_sens <- Re(sensitivity(M_circ))
M_circ_elas <- Re(elasticity(M_circ))


#Check the difference in High Fi Fu environment
M_trans_pa_sens <- round(M_pa_sens - M_size_sens,4)
M_trans_circ_sens <- round(M_circ_sens - M_size_sens,4)

M_trans_pa_elas <- round(M_pa_elas - M_size_elas,4)  
M_trans_circ_elas <- round(M_circ_elas - M_size_elas,4)

M_trans_pa_sens
M_trans_pa_elas

M_trans_circ_sens
M_trans_circ_elas
