set.seed(1000)
noOfSimulations = 1000
n = 40
lambda = 0.2

means = NULL
for(i in 1:noOfSimulations)
{
  sample = rexp(n,lambda)
  means = c(means,mean(sample))
}

# Compare Mean Of Sample Means To Theoretical Mean
hist(means,main='Distribution Of Sample Means',xlab='Sample Mean',col='blue')
abline(v=1/lambda,col='black',lwd=3)
abline(v=mean(means),col='grey',lwd=3)
legend(x='topright',col=c('black','grey'),legend=c("Theoretical Mean","Observed Mean"),lwd=3)

# Theoretical Mean
m1 = 1/lambda
print(m1)
# Observed Mean
m2 = mean(means)
print(m2)
# Absolute Difference
adiff = abs(m1 - m2)
print(adiff)


# Compare Variance Of Sample Means To Theoretical Variance

# Theoretical Variance
v1 = (1/(lambda*lambda*n))
print(v1)
# Observed Variance
v2 = var(means)
print(v2)
# Absolute Difference
adiff = abs(v1-v2)
print(adiff)

# Showing Approximate Normality
hist(means,main='Distribution Of Sample Means',xlab='Sample Mean',col='blue',prob=T)
x = seq(-3,8,0.1)
y = dnorm(x,mean=m1,sd=sqrt(v1))
lines(x,y,lwd=3)



