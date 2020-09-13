data=iris3
head(data)
iris$Species
t=as.numeric(iris$Species)
for (i in 1:length(t)){
  if(t[i]==1){
    iris$set[i]=1
    iris$ver[i]=0
    iris$vir[i]=0
  }else if(t[i]==2){
    
      iris$set[i]=0
      iris$ver[i]=1
      iris$vir[i]=0
    
  }else
  {
    
    iris$set[i]=0
    iris$ver[i]=0
    iris$vir[i]=1
    
  }
}
X=scale(iris[,1:4],center = TRUE,scale = TRUE)
model_string=" model{
  for (i in 1:150){
    y[i]~dbern(p[i])
    logit(p[i])=b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i]+b[5]*x4[i]
    
  }
  
  for (i in 1:5){
    b[i]~dnorm(0.0,1.0/1e6)
  }
}"
set.seed(42)
library("rjags")
library("coda")

#                                 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ONE VERSUS REST MODEL FOR MULTI CLASSIFICATION<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data1_jags=list(y=iris$set,x1=iris$Sepal.Length,x2=iris$Sepal.Width,x3=iris$Petal.Length,x4=iris$Petal.Width)
data1=list(y=iris$set,  x1=X[,1], x2=X[,2], x3=X[,3],  x4=X[,4])
params=c("b")
mod1=jags.model(textConnection(model_string),data = data1_jags,n.chains = 3)
update(mod1,1e3)
mod1_sim=coda.samples(model = mod1,variable.names = params,n.iter = 5e3)
mod1_csim=as.mcmc(do.call(rbind,mod1_sim))


data2_jags=list(y=iris$ver,x1=iris$Sepal.Length,x2=iris$Sepal.Width,x3=iris$Petal.Length,x4=iris$Petal.Width)
data2=list(y=iris$ver,  x1=X[,1],  x2=X[,2],  x3=X[,3],  x4=X[,4])
params=c("b")
mod2=jags.model(textConnection(model_string),data = data2_jags,n.chains = 3)
update(mod2,1e3)
mod2_sim=coda.samples(model = mod2,variable.names = params,n.iter = 5e3)
mod2_csim=as.mcmc(do.call(rbind,mod2_sim))
effectiveSize(mod2_sim)


data3_jags=list(y=iris$vir,x1=iris$Sepal.Length,x2=iris$Sepal.Width,x3=iris$Petal.Length,x4=iris$Petal.Width)
data3=list(y=iris$vir,  x1=X[,1],  x2=X[,2],  x3=X[,3],  x4=X[,4])
params=c("b")
mod3=jags.model(textConnection(model_string),data = data3_jags,n.chains = 3)
update(mod3,1e3)
mod3_sim=coda.samples(model = mod3,variable.names = params,n.iter = 5e3)
mod3_csim=as.mcmc(do.call(rbind,mod3_sim))
effectiveSize(mod3_sim)



#               >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>checking the convergance of the monti carlo chain<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

plot(mod1_sim,ask = TRUE)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)
dic.samples(mod1,n.iter = 1e3)
par(mfrow=c(3,2))
densplot(mod1_csim[,1:5],xlim=c(-1.0,1.0))


#                            >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.calculating residuals<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pm_coff1=colMeans(mod1_csim[,1:5])
pm_coff2=colMeans(mod2_csim[,1:5])
pm_coff3=colMeans(mod3_csim[,1:5])

x=c()
y=c()
z=c()
for (i in 1:length(iris$Sepal.Length)){
  x[i]=1.0/(1.0+exp(-1*((pm_coff1[1]+  pm_coff1[2]*iris$Sepal.Length[i]  + pm_coff1[3]*iris$Sepal.Width[i]  +  pm_coff1[4]*iris$Petal.Length[i]  +  pm_coff1[5]*iris$Petal.Width[i]))))
  y[i]=1.0/(1.0+exp(-1*((pm_coff2[1]+  pm_coff2[2]*iris$Sepal.Length[i]  + pm_coff2[3]*iris$Sepal.Width[i]  +  pm_coff2[4]*iris$Petal.Length[i]  +  pm_coff2[5]*iris$Petal.Width[i]))))
  z[i]=1.0/(1.0+exp(-1*((pm_coff3[1]+  pm_coff3[2]*iris$Sepal.Length[i]  + pm_coff3[3]*iris$Sepal.Width[i]  +  pm_coff3[4]*iris$Petal.Length[i]  +  pm_coff3[5]*iris$Petal.Width[i]))))
  #b=1.0/(1+exp(-a))
  }

a=c()
for ( i in 1:length(iris$Sepal.Length)){
  if(x[i]>y[i])
  {
    if (x[i]>z[i]){
      a[i]=1
    }else
    {
      a[i]=3
    }
  
  }else if(y[i]>z[i]){
    a[i]=2
  }else
  {
    a[i]=3
  }
}
#                          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>calculating the accuracy<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
b=t==a
c=mean(as.numeric(b))