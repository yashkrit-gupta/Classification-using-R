dat <- read.csv("https://dvats.github.io/assets/data/180893.csv")
y=as.vector(dat[,1])
x=as.matrix(dat[,-1],nrows=1000,ncols=50)
# head(x)
p=dim(x)[2]
n=dim(x)[1]
X=x
################################################
## MLE for logistic regression
## Using gradient ascent
################################################
f.gradient <- function(y, X, b) {
  b <- matrix(b, ncol = 1)
  pi <- exp(X %*% b) / (1 + exp(X%*%b))  
  rtn <- colSums(X* as.numeric(y - pi))
  return(rtn)
}

store.beta <- matrix(0, nrow = 1, ncol = p)
beta <- rep(0, p) # start at all 0s
t <- 1e-4
tol <- 1e-7
iter <- 0
diff <- 100
#not too many iterations
maxiter=1e4
while((diff > tol) && iter < maxiter)  {
  iter <- iter+1
  old <- beta
  beta = old + t* f.gradient(y = y, X= X, b = old)
  store.beta <- rbind(store.beta, beta)
  diff <- sum( (beta - old)^2)
  if(iter%%1e3==0){
      print(iter)
  }
}
iter # number of iterations
beta # last estimate
X.new=X

#ycap=X%*%beta
#ycap.hardmax=ycap>.5

est.y <- function(X.new, beta)
{
  yc=X.new%*%beta
  y.pred=yc>.5 
  return(y.pred)
}
ypred <- est.y(X,beta)
corr.mle=sum(ypred==y)
pct.mle=corr.mle/n*100
pct.mle
save(est.y, beta, file = "180893.Rdata")