  # set variables
  mm=0
  mm1=0
  mm2=0
  s1=0
  set.seed(123)
  x1=runif(50,0,1) # original sample Tas(0,1)
  for (n in 1:50000){
    xp=sample(x1,50,replace = T) #bootstrap sample (non parametric)
    mm[n]=max(xp) # pick ML-estimates
    x2=runif(50,0,max(x1)) # parametric sample
    xp2=sample(x2,50,replace = T) # parametric bootstrap sample

    mm1[n]=max(xp) #create vector of SU-estimates (non parametric)
    mm2[n]=max(xp2) #create vector or SU-estimates (parametric)
  }
  # non-parametric bootstrap
  plot(density(mm1)) # density of bootstrap
  hist(mm1,main="Histogram of ML-estimate (non parametric)",xlab = "mean = 0.981 (red line)") #histogram of bootsrap estimates
  abline(v=mean(mm1),col='red')
  boxplot(mm1) # boxplot for estimates
  # parametric boostrap
  hist(mm2,200,main="Histogram of ML-estimate (parametric)",xlab = "mean = 0.963 (red line)")
  abline(v=mean(mm2),col='red')
  boxplot(mm2)
  # original data sample
  hist(x1,,main="Histogram of original sample",xlab = "mean = 0.520 (red line)")
  abline(v=mean(x1),col='red')
  mean(x1)
  # 95% confidence intervals for parameritric Ml-estimate
  mean(mm2)-1.96* sqrt(var(mm2)) # lower limit
  mean(mm2)+1.96* sqrt(var(mm2)) # upper limit
  # 95% quantile intervals
  quantile(mm2,0.975)
  quantile(mm2,0.025)
  var(mm2) # variance of Ml-estimate
  sd(mm2)/(sqrt(n)) # standard error of estimate
  max(x1)-mean(mm2) #bias for ML-estimate
  
  
