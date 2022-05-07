### CODE 1.5.1
# 유클리드 거리, 표준화 유클리드 거리, 마할라노비스 거리, 시티블럭 거리 계산
# Euclidean distance => Cluster Analysis / Multi Dimensional Scaling 등에 사용
# Mahalanobis distance => Hotelling T_square test / Discriminant Analysis 등에 사용
Data1.1.1 = read.table('Multivariate_Analysis_1/data/3subjects.txt', header=T)
X = Data1.1.1
X = as.matrix(Data1.1.1)

n = nrow(X)
xbar = t(X)%*%matrix(1, n, 1)/n   # 평균벡터
I = diag(n)
J = matrix(1, n, n)
H = I-1/n*J                       # 중심화행렬
Y = H%*%X                         # 중심화 자료행렬
S = t(Y)%*%Y/(n-1)                # 공분산행렬
D = diag(1/sqrt(diag(S)))         # 표준편차행렬의 역
Z = Y%*%D                         # 표준화자료행렬
colnames(Z) = colnames(X)

# 유클리드 거리
de = as.matrix(dist(X, method="euclidean"))
de = as.dist(de)
round(de, 3)

# 표준화 유클리드 거리
ds = as.matrix(dist(Z, method="euclidean"))
ds = as.dist(ds)
round(ds, 3)

# 마할라노비스 거리
# install.packages('biotools')
library(biotools)
dm = D2.dist(X, S)
round(sqrt(dm), 3)

# 시티블럭 거리
dc = as.matrix(dist(X, method="manhattan"))
dc = as.dist(dc)
round(dc, 3)