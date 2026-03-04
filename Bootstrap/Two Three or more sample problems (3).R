#######################################################################
################### TWO SAMPLES #######################################
pdelays_c = c(39,35,44,33,19,6,27,24,40,13,35,34,34,33,61,
              56,43,19,34,40,34,28,29,45,46,28,41,46,25,17)

pdelays_z = c(39,32,22,32,55,34,46,37,31,45,41,40,46,66,36,
              42,37,43,35,62,48,47,34,42,43,33,47,41,34,41)

# observed F_ratio and mean differ.
o_meandiff = mean(pdelays_c)-mean(pdelays_z)
o_Fratio = var(pdelays_c)/var(pdelays_z)

## Bootstrapping
# combine the two data sets
all_c_z = c(pdelays_c,pdelays_z)
# number of bootstrap samples
B = 5000
bstrc = matrix(0,ncol = length(pdelays_c),nrow = B)
bstrz = matrix(0,ncol = length(pdelays_z),nrow = B)
for(j in 1:B){
  samp = sample(all_c_z,size = length(all_c_z),replace=TRUE)
  bstrc[j,] = samp[1:30]
  bstrz[j,] = samp[31:60]
}
# bootstrap means
bstrcmean = apply(bstrc,1,mean)
bstrzmean = apply(bstrz,1,mean)
bstr_meandiff = bstrcmean-bstrzmean

# bootstrap Fratio
bstrc_var = apply(bstrc,1,var)
bstrz_var = apply(bstrz,1,var)
bstrFratio = bstrc_var/bstrz_var

hist(bstr_meandiff,col="blue",main = "Distribution of means difference",
     xlab=expression(bar(X)[c]^'*'-bar(X)[z]^'*'))
hist(bstrFratio,col="yellow",main='Distr of ratio of variances')

########### CONFIDENCE INTERVALS #################
sortmeandiff = sort(bstr_meandiff)
lb = sortmeandiff[0.025*B] 
ub = sortmeandiff[0.975*B]

cat("BMCI = ","(",round(lb,2)," , ",round(ub,2),")",sep='')
cat("The Sampling Errors are given by",round(lb,2),"and ",round(ub,2))
cat("THUS")
cat("95% CI for mu1 - mu2  = ","(",o_meandiff-ub," , ",o_meandiff-lb,")",sep ='')

########### HYPOTHESIS TESTING #################
pvalue_Mdiff = (length(bstr_meandiff[bstr_meandiff < o_meandiff])+
                  length(bstr_meandiff[bstr_meandiff > -o_meandiff]))/B
pvalue_Mdiff
p_val_var = length(bstrFratio[bstrFratio > o_Fratio])/B
p_val_var

#########################################################################################################
#################################### THREE OR MORE SAMPLES ##############################################

# Store all the locations in a list LOC
loc1 = c(344,382,353,395,207,312,407,421,366,222)
loc2 = c(365,391,538,471,431,450,299,371,442,343)
loc3 = c(261,429,402,391,239,295,129,301,317,386)
loc4 = c(422,408,470,523,398,387,433,440)
loc5 = c(367,445,480,323,366,325,316,381,407,339)
LOC  = list(loc1,loc2,loc3,loc4,loc5)

N = length(unlist(LOC))
n = sapply(LOC,length)
k = length(LOC)
B = 5000
Yi. = sapply(LOC,mean)
Y.. = mean(unlist(LOC))

# Calculate SSE and SST
SSE = SST = 0
for(j in 1:k){
  SSE = SSE+sum((LOC[[j]]-Yi.[j])^2)
  SST = SST+n[j]*(Yi.[j]-Y..)^2
}
# observed F-ratio
Fr_obs = (SST/(k-1))/(SSE/(N-k))

# Bootstrapping 
RATIOS = vector("numeric")
ALL = unlist(LOC)
for (b in 1:B) {
  DRAWS = list()
  draw = sample(ALL,N,TRUE)
  # What if you had 20 locations, can you think of a better way to code lines 91 - 96 below?
  DRAWS[[1]]  = draw[1:10]
  DRAWS[[2]]  = draw[11:20]
  DRAWS[[3]]  = draw[21:30]
  DRAWS[[4]]  = draw[31:38]
  DRAWS[[5]]  = draw[39:48]

  Y.. = mean(unlist(DRAWS))
  Yi. = sapply(DRAWS,mean)
  SSE = SST = 0
  for(j in 1:k){
    SSE = SSE+sum((DRAWS[[j]]-Yi.[j])^2)
    SST = SST+n[j]*(Yi.[j]-Y..)^2
  }
  RATIOS[b] = (SST/(k-1))/(SSE/(N-k))
}

hist(RATIOS,col = "blue",main = "Distribution of F-ratios",breaks=30)
abline(v = Fr_obs,col = 'red',lwd = 2)
legend('topright',legend = c('Observed Fratio'),col='red',lwd=2,bty='n')

### HYPOTHESIS TESTING ####
pvalue = length(RATIOS[RATIOS > Fr_obs])/B

# compare to pvalue obtained using normal theory
theory_pvalue = pf(Fr_obs,k-1,N-k,lower.tail = FALSE)
pvalue
theory_pvalue
