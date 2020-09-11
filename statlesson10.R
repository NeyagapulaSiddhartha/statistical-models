library("COUNT")
data("badhealth")
mod_string1="model{
  for (i in 1:length(numvisit)){
  numvisit[i]~dpois(lam[i])
  log(lam[i])=b1+b3*age[i]+b2*badh[i]
  
  }
  
    b1~dnorm(0.0,1.0/1e4)
    b2~dnorm(0.0,1.0/1e4)
    b3~dnorm(0.0,1.0/1e4)

  }"


mod_string2="model{
  for (i in 1:length(numvisit)){
 numvisit[i]~dpois(lam[i])
  log(lam[i])=b[1]+b[2]*age[i]+b[3]*badh[i]+b[4]*age[i]*badh[i]
  
  }
  for (i in 1:4){
    b[i]~dnorm(0.0,1/1e6)
  }}"
mod_string3="model{
for (i in 1:length(x0)) {
  x0[i] ~ dpois( y[i]*lam[i] )
  log(lam[i]) = a[1] + a[2]*x1[i] + a[3]*x2[i]}
  for ( i in 1:3){
    a[i]~dnorm(0.0,1/1e2)
    
  }
}"



set.seed(42)
dat = read.csv(file="callers.csv", header=TRUE)
x=na.omit(as.numeric(callers$V1))
y=na.omit(as.numeric(callers$V2))
z=na.omit(as.numeric(callers$V3))
w=na.omit(as.numeric(callers$V4))
data=list("x1"=w,"x2"=z,"x0"=x)
params3=c("a")
mod3=jags.model(textConnection(mod_string3),data = data,n.chains = 3)
update(mod3,1000)
mod3_sim=coda.samples(model = mod3,variable.names = params3,n.iter = 5000)
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)
effectiveSize(mod3_sim)

mod3_csim=as.mcmc(do.call(rbind,mod3_sim))


data_jags=as.list(badhealth)
library("rjags")
library("coda")
params1=c("b1","b2","b3")
params2=c("b")
mod1=jags.model(textConnection(mod_string1),data = data_jags,n.chains = 3)
mod2=jags.model(textConnection(mod_string2),data = data_jags,n.chains = 3)
update(mod1,1000)
update(mod2,1e3)
mod1_sim=coda.samples(model = mod1,variable.names = params1,n.iter = 5000)
mod2_sim=coda.samples(model = mod2,variable.names = params2,n.iter = 5000)
x=dic.samples(model = mod1,n.iter = 5000)
y=dic.samples(model=mod2,n.iter = 5e3)
dat = read.csv(file="callers.csv", header=TRUE)
x=na.omit(as.numeric(callers$V1))
y=na.omit(as.numeric(callers$V2))
z=na.omit(as.numeric(callers$V3))
plot(z,x/y)