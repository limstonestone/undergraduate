# Practice Time 1 for Multivariate Statistics

# ctrl + l : consol 창 내용 지우기
# ctrl + r : 라인별 스크립트 실행
# ?함수 ex) ?apply
# 함수의 코드 구성을 알고 싶을때 : F5

### R 기초 사용법 ###
# scalar
x = 2
x
x <- 4
x

# vector
y = c(1, 2, 3)
y

seq(1, 10)
seq(1, 10, 0.5)

rep(1, 10)
rep(1:3, 10)

# matrix
matrix(1, nrow=3, ncol=3)
matrix(1, 3, 3)
matrix(c(1,2,3,1,2,3,1,2,3), 3, 3)
matrix(c(1,2,3,1,2,3,1,2,3), 3, 3, byrow=T)

A = matrix(rep(c(1,2,3), 3), 3, 3, byrow=T)
A
colnames(A) = c("일", "이", "삼")
A
rownames(A) = c(1,2,3)
A

# 작업경로 설정
getwd()
setwd(getwd())