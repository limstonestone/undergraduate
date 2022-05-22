#### CODE 3.2.1
# PCFA for Spearman Intelligence Data

R=matrix(c(1.00, 0.83, 0.78, 0.70, 0.66, 0.63,
           0.83, 1.00, 0.67, 0.67, 0.65, 0.57,
           0.78, 0.67, 1.00, 0.64, 0.54, 0.51,
           0.70, 0.67, 0.64, 1.00, 0.45, 0.51,
           0.66, 0.65, 0.54, 0.45, 1.00, 0.40,
           0.63, 0.57, 0.51, 0.51, 0.40, 1.00), byrow=T, nrow=6)
subjects=c("고전", "프랑스어", "영어", "수학", "음감", "음악")
#Spectral Decomposition
eigen.R = eigen(R)
V = eigen.R$vectors # Eigenvectors
gof = eigen.R$values / sum(eigen.R$values)*100
round(gof, 2)

# Estimation of Loadings Matrix : L=VD and Specific Factor
E = diag(sqrt(eigen.R$values[1:2])) # squared root eigenvalues
V = V[1:2]
L = round(V%*%E, 2) # Loadings Matrix
L

# PCFA using the principal()
library(psych)
pcfa = principal(R, nfactor=2, rotate="none")
summary(pcfa)
pcfa
round(pcfa$loadings[,1:2], 2)