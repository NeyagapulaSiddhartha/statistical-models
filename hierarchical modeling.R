
library("MASS")
data("OME")
library("coda")
library("rjags")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

##reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> hierarchial  model<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<,,
mod_string = "model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]

	}
		
	for (j in 1:max(ID)){
	b0[j]~dnorm(mu,prec)
	}
	mu~dnorm(0,1/10^2)
	
	prec~dgamma(1/2.0,1*1/2.0)
	sig=sqrt(1/prec)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "
set.seed(42)
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID
mod1=jags.model(textConnection(mod_string),data = data_jags,n.chains = 3)
update(mod1,1e3)
params=c("b","b0","mu","sig")
mod1_sim=coda.samples(model = mod1,variable.names = params,n.iter = 5e3)
mod1_csim=as.mcmc(do.call(rbind,mod1_sim))
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###convergance diagnostics<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
raftery.diag(mod1_csim)
autocorr(mod1_sim)
gelman.diag(mod1_sim)
plot(mod1_sim,ask=TRUE)

dic.samples(model = mod1,n.iter = 1e3)
summary(mod1_sim)
