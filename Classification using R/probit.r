dat <- read.csv("https://dvats.github.io/assets/data/180893.csv")
y=as.vector(dat[,1])
x=as.matrix(dat[,-1],nrows=1000,ncols=50)
p=dim(x)[2]
n=dim(x)[1]
f.gradient <- function(y, x, beta) {
  beta <- matrix(beta, ncol = 1)
  pi <- y*dnorm(x%*%beta)/(pnorm(x%*%beta)*(1-pnorm(x%*%beta)))-dnorm(x%*%beta)/(1-pnorm(x%*%beta))
  rtn <- colSums(x*as.numeric(pi))
  return(rtn)
}
X=x
beta_k <- rep(0, p) # start at all 0s
t <- 1e-6
tol <- 1e-8
iter <- 0
diff <- 100
while(diff > tol && iter < 1e5) {
  iter <- iter+1
  old <- beta_k
  beta_k = old + t* f.gradient(y = y, x= x, beta = old)
  diff <- sum( (beta_k - old)^2)
  if(iter%%1e3==0){
      print(iter)
  }
}
iter
# beta_k #Value of beta estimates
ycap=X%*%beta_k
ycap.hardmax=ycap>.5
corr.mle=sum(ycap.hardmax==y)
pct.mle=corr.mle/n*100
pct.mle
# 70%