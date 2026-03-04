######################################## CHAPTER 1.s1 ###########################################
##################################################################################################

# Load the dataset
pdelays = c(37,42,38,44,39,35,43,41,42,38,36,34,37,42,36,
            38,41,39,39,37,34,34,41,41,40,38,38,46,38,42)

## Exploratory data analysis
obs_mean = mean(pdelays)
sd_pd = sd(pdelays)
fivesum = summary(pdelays)
obs_median = median(pdelays)

boxplot(pdelays,main ="Box-and-Whisker of payment delays",horizontal =TRUE,xlab ='Days')
hist(pdelays,breaks =10,col ='wheat',xlab ='Days',main ="Histogram of payment delays")

## Bootstrapping
# number of bootstrap samples
B = 5000
# matrix to store the bootstrap samples
bstr <- matrix(0,ncol =length(pdelays),nrow =B)
for(i in 1:B){
  samp = sample(pdelays,size =length(pdelays),replace =TRUE)
  bstr[i,]  = samp
}
# Bootstrapped means
bstrm = apply(bstr,1,mean)

# Distribution of the bootstrapped means
hist(bstrm,main = 'Bootstrapped means',xlab =expression(bar(X)[b]),breaks =30)
abline(v = c(obs_mean,mean(bstrm)),col =c('red','green'),lwd =2)

####### 95% Confidence Intervals ##########################
sortedmeans = sort(bstrm)
alpha = 0.05
# positions of the quantiles
pos1 = (alpha/2)*B  ; pos2 = (1-(alpha/2))*B
q1 = sortedmeans[pos1] 
q2 = sortedmeans[pos2]
cat("(",q1," , ",q2,")",sep ='')

# Sampling errors
se1 = q1-obs_mean
se2 = q2-obs_mean
 
# population mean CI bounds
ubound = obs_mean-se1
lbound = obs_mean-se2
cat("95% CI for mu: ","(",lbound," , ",ubound,")",sep ='')

####### Hypothesis testing ##########################
count = length(sortedmeans[sortedmeans > 40])
bstr_pval = count/length(bstrm)
bstr_pval

# Compare to p-value using normal theory
TestSt = (obs_mean-38)/(sd(pdelays)/sqrt(30))
NormalTh_pval = 1-pt(TestSt,29)
NormalTh_pval