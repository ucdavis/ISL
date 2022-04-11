swiss$CatholicD=ifelse(swiss$Catholic <= 50, "no", "yes")
swiss

library(ggplot2)

ggplot(swiss, aes(x=Fertility, y = Agriculture, color=CatholicD)) +geom_point()

## searchgrid
x1=seq(min(swiss$Fertility),max(swiss$Fertility), length.out = 100)
x2=seq(min(swiss$Agriculture),max(swiss$Agriculture), length.out = 100)

knnfield=matrix(NA, ncol = length(x1), nrow = length(x2))

cbind(x1, x2)
swiss[, c("Fertility", "Agriculture")]

## Euclidian distance for reference point x0 and target x1
ed=function(x0, x1){
  ed=(x1[1]-x0[1])^2 + (x1[2] - x0[2])^2
  return(ed)
}

swiss[45, c("Fertility", "Agriculture")]

K = 3

for(i in 1:100){
  for(j in 1:100){
    ## Compute Euclidian distance for given x0 to observations
    euclidiandst = ed(x0=c(x1[i], x2[j]), x1=swiss[, c("Fertility", "Agriculture")])
    euclidiandst
    ## Order dataframe object (hence the [,1])
    ordeucl=order(euclidiandst[,1])
    ordeucl
    ## Create logical vector of lenght K with closest ED's obtained with ordeucl[1:k] index
    ## and save as probability for "yes"
    Pr_ref=sum(swiss$CatholicD[ ordeucl[1:K] ]=="yes")/K
    Pr_ref
    ##
    knnfield[i,j]=Pr_ref
  }
}

rownames(knnfield) = x1
colnames(knnfield) = x2
knnfield
dich=ifelse(knnfield>=.5, 1, 0)
dich
image( dich )
knnfield

plot(swiss$Fertility, swiss$Agriculture, col=as.numeric(factor(swiss$CatholicD)), pch = 15)
for(i in 1:100){
  for(j in 1:100){
    points(x1[i], x2[j], col=dich[i,j]+1, cex=.5)
  }
}

## fix some errors