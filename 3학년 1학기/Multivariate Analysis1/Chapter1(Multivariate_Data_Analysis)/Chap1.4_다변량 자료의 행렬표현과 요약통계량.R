### CODE 1.4.1
# 공분산행렬과 상관행렬 대수적 계산
Data1.1.1 = read.table('Multivariate_Analysis_1/data/3subjects.txt', header=T)
X = Data1.1.1
X = as.matrix(X)                # 자료행렬
n = nrow(X)
xbar = t(X)%*%matrix(1, n, 1)/n # 평균벡터
I = diag(n)
j = matrix(1, n, n)
H = I - 1/n*j                   # 중심화행렬
Y = H%*%X                       # 중심화 자료행렬
S = t(Y)%*%Y/(n-1)              # 공분산행렬
D = diag(1/sqrt(diag(S)))       # 표준편차행렬의 역
Z = H%*%X%*%D                   # 표준화자료행렬
colnames(Z) = colnames(X)
R = t(Z)%*%Z/(n-1)              # 상관행렬
R_S = D%*%S%*%D                 # 상관행렬과 공분산행렬의 대수적 관계
detS = det(S)                   # 일반화분산 (det() = 행렬식 구하는 내장함수)
detR = det(R)
trS = sum(diag(S))              # 총분산
trR = sum(diag(R))

# 결과 출력
X; Y; Z; S; R; detS; trS; detR; trR

### CODE 1.4.2
# 일반화분산과 총분산의 계산
XX = read.table('Multivariate_Analysis_1/data/irisflower.txt', header=T, fileEncoding="euc-kr")
XX = XX[, 2:5]
XX = as.matrix(XX)

SandR = array(NA, c(4, 4, 6))
rownames(SandR) = colnames(XX)
colnames(SandR) = colnames(XX)
variation = matrix(NA, 3, 4)
rownames(variation) = c("setosa", "versicolor", "virginica")
colnames(variation) = c("detS", "trS", "detR", "trR")

for(i in 1:3) {
  X = XX[(50*(i-1) + 1):(50*i), ]
  
  S = cov(X)  # 공분산 행렬 계산
  R = cor(X)  # 상관 행렬 계산
  
  detS = det(S)
  detR = det(R)
  trS = sum(diag(S))
  trR = sum(diag(R))
  
  SandR[, , i*2-1] = S
  SandR[, , i*2] = R
  
  variation[i, 1] = detS
  variation[i, 2] = trS
  variation[i, 3] = detR
  variation[i, 4] = trR
}

SandR = round(SandR, 3)
variation = round(variation, 6)
setosa = list(SandR[, , 1], SandR[, , 2], variation[1,])
versicolor = list(SandR[, , 3], SandR[, , 4], variation[2,])
virginica = list(SandR[, , 5], SandR[, , 6], variation[3,])

setosa
versicolor
virginica