# theta <- c(thetaA,thetaB); n <- c(nA,nB,nAB,nO). 
nlikelihood <- function(theta,n){# 負の対数尤度を計算
  a <- theta[1]; b <- theta[2]; o <- 1-a-b
  l <- log(c(a^2+2*a*o, b^2+2*b*o, 2*a*b, o^2))
  -sum(n*l)
}

# 最尤推定量(mle)を求める
# n <- c(nA,nB,nAB,nO). 
mle <- function(n){
  # 最適化の計算
  op <- optim(c(1/3,1/3),nlikelihood,n=n) 
  # 解を出力
  list(a=op$par[1],b=op$par[2],o=1-op$par[1]-op$par[2])
} 


n <- c(40,30,10,20)
r <- mle(n); ta <- r$a; tb <- r$b; to <- r$o

cat('observation. A, B, AB, O:',n,'\n\n')
cat('estimate. a, b, o:', c(ta,tb,to),'\n\n')
res <- cbind(n/sum(n),c(ta^2+2*ta*to, tb^2+2*tb*to, 2*ta*tb, to^2))
dimnames(res)[[1]] <- c('A','B','AB','O')
dimnames(res)[[2]] <- c('obs','estimate')
print(res)







