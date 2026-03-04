################### Simulating Central Limit Theorem #######################
# BERNOULLI 
n_bern = 200000
xx = vector(length=n_bern)
for(j in 1:n_bern){
  # Assign X = 1 if u < p = 0.25 else X = 0 
  xx[j] = as.numeric(runif(1) < 0.25)}
# You can use the rbinom() with size = 1

# Analysis 1: Group into sets of 10 observations and calculate the mean
gpe10 = matrix(xx,ncol = 10)
gpe10mean = apply(gpe10,1,mean)

# Analysis 2: Group into sets of 100 observations and calculate the mean
gpe100 = matrix(xx,ncol = 100)
gpe100mean = apply(gpe100,1,mean)

# Plot the observed frequencies/densities
par(mfrow = c(1,2),bg = "wheat")
hist(gpe10mean,col = "blue",main = "Distr of means \n (n = 10, N = 20000)")
hist(gpe100mean,col = "blue",breaks=30,main = "Distr of means \n (n = 100, N = 2000)")
dev.off()

################### R Exercise #######################
rnd = rnorm(45000,mean=0,sd=3)
gpe9 = matrix(rnd,ncol=9,byrow=T)
gpe9mean=apply(gpe9,1,mean)

# Distribution of the sample means and superimpose a standard normal
hist(gpe9mean,col='blue',main="Distribution of sample mean \n(n = 9, N = 5000)",
     freq = F,xlab=expression(bar(X)))
curve(dnorm(x,0,sd=sqrt(9/9)),col = "green",lwd = 2,add = TRUE)
legend('topright',legend = expression(N(0,1)),lwd=2,col="green",bty = 'n')

# Distribution of the sample standard deviation
gpe9sd = apply(gpe9,1,sd)
hist(gpe9sd,col="blue",main="Distribution of S",xlab ='S')

# Distribution of T-statistics
Te = gpe9mean/(gpe9sd/3)
hist(Te,col="blue",main="Distribution of t-statistic",freq=F,breaks = 20)
curve(dt(x, df = 8), col = "black",lwd = 2,add = TRUE)
legend('topright',legend = c(expression(t[8])),lwd=2,col=c('black'),bty = 'n')

# Compare properties with the empirical results of the T-statistic
# Pr(Te > |2.5|)
cat('Empirical prob. = ',sum(abs(Te) > 2.5)/length(Te)*100,'%',
    '\nTheoretical prob. = ',round((pt(2.5,8,lower.tail = FALSE)*2)*100,2),'%',sep ='')
# Kurtosis
library(moments)
kurtosis(Te) # If it was normal , kurtosis = 3
