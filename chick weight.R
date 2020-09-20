set.seed(42)
#The ChickWeight data frame has 578 rows and 4 columns from an experiment on the effect of diet on early growth of chicks.
#EXPLORATORY VARIABLES :: TIME ,DIET 
#GROUPING VARIABLES    ::  Chick
#RESPONCE VARIABLE     ::   WEIGHT OF THE CChicK


                       #OUR AIM IS THE TELL WHICH DIET IS MOST SUITABLE FOR THE CHICKS"

#MODELS :  BASIC POISSION REGRESSION AND A Hierarchical Model
#HIERARCHIAL MODEL :
#          here i used the intercept method for creating seperate intercept for each diet 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>DATA EXPLORATION<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#set.seed(61)
#traceplot(mod_csim)
#boxplot(Time~Diet,data = ChickWeight)
#boxplot(weight~Diet,data = ChickWeight)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
head(ChickWeight)
summary(ChickWeight)
unique(ChickWeight$Chick)
model_string= "model{
  for (i in 1:length(Diet)){
    weight[i]  ~dpois(lam[i])
    log(lam[i])=alpha[Diet[i]] +b[1]*Time[i]  
  }
  for (i in 1:max(Diet)){

    alpha[i]~dnorm(theta,prec)
  }
  theta~dnorm(0.0,1/1.0e6)
  prec~dgamma(1/2.0,1*10/2.0)
  tau=sqrt(1/prec)
  
    b[1]~dnorm(0.0,1/1e6)

}"
model_string1="model{
  for (i in 1:length(Diet)){
    weight[i]  ~dpois(lam[i])
    log(lam[i])=alpha +b[1]*Time[i]  + b[2]*Diet[i] +b[3]*Chick[i]
  }
  
    alpha~dnorm(0.0,1/1.0e6)

  for (j in 1:3){
    b[j]~dnorm(0.0,1/1e6)
  }
}"

x=na.omit(ChickWeight)
data_jags=as.list(x)
params2=c("alpha","b")
params=c("tau","theta","b","alpha")
library("coda")
library("rjags")
mod1=jags.model(textConnection(model_string),data = data_jags,n.chains = 3)         #
update(mod1,1e3)         
mod_sim=coda.samples(model = mod1,variable.names = params,n.iter = 5e3)             # MODEL 1 INTERCEPT MODEL based on the diet of the chicken 
mod_csim=as.mcmc(do.call(rbind,mod_sim))

mod2=jags.model(textConnection(model_string1),data = data_jags,n.chains = 3)        ##
update(mod2,1e3)                                                                    ##MODEL 2 basic bayesian model
mod_sim1=coda.samples(model = mod2,variable.names = params2,n.iter = 5e3)            ##
mod_csim1=as.mcmc(do.call(rbind,mod_sim1))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>m.<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>CONVERGIANCE VERIFICATION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# though the hierarchial model seems tow have a larger deviance the number of effective parameters are roughly the same. and the bacis model
#doesnt seem to converge for some coefficients this can be seen by some of the convergance diagnostics



summary(mod_sim)
plot(mod_sim)
                                  
                                           autocorr.diag(mod_csim)

alpha[1]    alpha[2]    alpha[3]     alpha[4]            b          tau        theta
Lag 0   1.000000000 1.000000000  1.00000000  1.000000000  1.000000000  1.000000000  1.000000000
Lag 1   0.700922295 0.624588489  0.66131783  0.633475028  0.870872505  0.252380174 -0.023952514
Lag 5   0.422988524 0.347215488  0.37529610  0.363880576  0.563613197  0.009311632 -0.005537330
Lag 10  0.255438948 0.195359391  0.20558086  0.217985810  0.332763903  0.014754027  0.013397318
Lag 50 -0.003507712 0.006970212 -0.00523329 -0.008602539 -0.001181296 -0.014317068  0.007642579



                                    dic.samples(model = mod1,n.iter = 1e3)

dic.samples(model = mod1,n.iter = 1e3)
|**************************************************| 100%
Mean deviance:  7860 
penalty 4.83 
Penalized deviance: 7865 

#the penalty is 4.8 which says the effective number of parameters are 4 but we used 7 parameters this is a positive sign .




                                          gelman.diag(mod_sim)

Potential scale reduction factors:
  
  Point est. Upper C.I.
alpha[1]          1          1
alpha[2]          1          1
alpha[3]          1          1
alpha[4]          1          1
b                 1          1
tau               1          1
theta             1          1

Multivariate psrf
1
#the scale rediction factor is <1.1 which shows that the chain has converged 






                                          raftery.diag(mod_csim)

Quantile (q) = 0.025
Accuracy (r) = +/- 0.005
Probability (s) = 0.95 

Burn-in  Total Lower bound  Dependence
(M)      (N)   (Nmin)       factor (I)
alpha[1] 10       12880 3746         3.44      
alpha[2] 12       14793 3746         3.95      
alpha[3] 8        10404 3746         2.78      
alpha[4] 12       13581 3746         3.63      
b        20       21114 3746         5.64      
tau      2        3802  3746         1.01      
theta    3        4197  3746         1.12 

#though the effective sample size for the alpha variables is relative large its still sufficient to find the posterior mean and preditions 
#but if its not enough for finding the 95%confidence interval


#raftery.diag(mod_sim1)
summary(mod_sim1)
plot(mod_sim1)
autocorr.diag(mod_csim1)
dic.samples(model = mod2,n.iter = 1e3)
#gelman.diag(mod_sim1)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>M<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SUMMARY<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
summary(mod_sim)

#alpha[1] 3.70467 0.0110675 9.037e-05      3.361e-04
#alpha[2] 3.84970 0.0123050 1.005e-04      3.399e-04
#alpha[3] 4.00314 0.0117445 9.589e-05      3.381e-04
#alpha[4] 3.96195 0.0117348 9.581e-05      3.275e-04
#b        0.07612 0.0006253 5.105e-06      2.211e-05
#tau      1.99491 1.0448915 8.532e-03      1.172e-02
#theta    3.87448 1.1313456 9.237e-03      9.392e-03
 
#judging by the alpha coefficients which correspond to the each diet plan the diet plan number 3 seems to be doing pretty good job at increasing 
#weight of the chicks







