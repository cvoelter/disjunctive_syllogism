##Code simulation Motes-Rodrigo et al.
##load package needed to fit models:
library(lme4)
##generate object controling optimization with glmer:
contr=glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=10000))
###set working directory (needs to be adjusted to the current computer)
setwd("/home/imitation_sim")
##load a couple of functions needed:
source("functions.r")
##basic settings:
n.subj=16##number individuals
n.demos=4##number demonstrations
n.actions=6##number actions
##create vector with maximum imitation probabilities (Pmax in Table 3)
imit.probs=c(0.0001, 0.0005, 0.001, 0.005, seq(0.01, 0.1, by=0.01), 0.2, 0.4, 0.6, 0.8)
##create object to store the results of the simulation:
all.res=vector("list", length(imit.probs))
##create data frame defining the combinations familiarity, contact involvement, and environmental effect present in the data:
to.sim=data.frame(
	fam=rep(c("yes", "no"), each=3), 
	contact=rep(rep(c("yes", "no"), times=c(2, 1)), times=2), 
	env.eff=rep(rep(c("yes", "no"), times=c(1, 2)), times=2))
##add column to to.sim which labels the combination of all three factors (action type):
xx=lapply(1:3, function(x){
	paste(substr(colnames(to.sim)[x], start=1, stop=3), toupper(substr(as.character(to.sim[, x]), start=1, stop=1)), sep="")
})
to.sim$action=as.factor(apply(matrix(unlist(xx), ncol=3, byrow=F), 1, paste, collapse="."))
##add column to to.sim indicating the probabilities (relative to Pmax) to be simulated:
to.sim$mean=c(1, 0.8, 0.4, 0.7, 0.5, 0.1)
to.sim=to.sim[order(to.sim$action), ]##sort to.sim
rownames(to.sim)=to.sim$action##and add rownames to to.sim

##prepare for the simulation:
##load library needed for parallelization:
library(parallel)
cl <- makeCluster(getOption("cl.cores", detectCores()))
cl=cl[1:(length(cl)-1)]
##load package lme4 in all the nodes available:
parLapply(cl=cl, X=1:length(cl), function(x){library(lme4)})
n.simus=1000##define number simulations
##now the simulation itself:
for(ip in 1:length(imit.probs)){##for each value if Pmax (here entry in imit.probs)
	##load all the objects and functions to each of the nodes:
	clusterExport(cl=cl, 
		varlist=c("n.subj", "n.demos", "n.actions", "to.sim", "ip", "imit.probs", "drop1p", "contr", "lmer.warns", "aic.c.fac"))
	##run the simulations
	ires=parLapply(cl=cl, X=1:n.simus, fun=function(s){
    set.seed(s)##setting the seed makes it entirely replicable
    print(s)##just to see progress
    ##generate basic data wrt the precictors:
		xdata=data.frame(
			subj=as.factor(paste("s", 1:n.subj, sep=".")),
			mother.reared=as.factor(sample(rep(c("no", "yes"), each=n.subj/2), n.subj, replace=F)),
			age=round(runif(n=n.subj, min=5, max=47), 1)
		)
		##add action types and effects to be simulated to xdata:
		xdata=data.frame(xdata[rep(1:nrow(xdata), each=n.actions), ], to.sim[rep(1:nrow(to.sim), times=nrow(xdata)), 1:4])
		##add action order to xdata:
		xdata$action.order=unlist(lapply(rep(n.actions, n.subj), sample, size=n.actions, replace=F))
		##add demonstration number to xdata:
		xdata=data.frame(xdata[rep(1:nrow(xdata), each=n.demos), ], demo=1:n.demos)
		#xdata=data.frame(xdata[rep(1:nrow(xdata), each=n.trials), ], trial=1:n.trials)
		##z-transform the covariates:
		xdata$z.age=as.vector(scale(xdata$age))
		xdata$z.demo=as.vector(scale(xdata$demo))
		xdata$z.action.order=as.vector(scale(xdata$action.order))
		##dummy code and center action type (needed for including them as random slopes):
		d.action.all=lapply(levels(xdata$action)[-1], function(x){
			xx=as.numeric(xdata$action==x)
			xx-mean(xx)
		})
		d.action.all=matrix(unlist(d.action.all), ncol=length(d.action.all), byrow=F)
		colnames(d.action.all)=paste("d.aa", levels(xdata$action)[-1], sep=".")
		##add centered dummy variables to xdata:
		xdata=data.frame(xdata, d.action.all)
		##dummy code and center familiarity, contact involvement, and environmental effect (needed for including them as random slopes):
		d.ct.type=lapply(c("fam", "contact", "env.eff"), function(x){
			xx=as.numeric(xdata[, x]==levels(xdata[, x])[2])
			xx-mean(xx)
		})
		d.ct.type=matrix(unlist(d.ct.type), ncol=length(d.ct.type), byrow=F)
		colnames(d.ct.type)=paste(substr(c("fam", "contact", "env.eff"), start=1, stop=3), "Y", sep=".")
		##add centered dummy variables to xdata:
		xdata=data.frame(xdata, d.ct.type)
		##generate response:
		xdata$rv=rbinom(n=nrow(xdata), size=1, prob=as.vector(imit.probs[ip]*to.sim[as.character(xdata$action), "mean"])*
      (c(1, 0.5)[1+as.numeric(xdata$mother.reared=="no")]))##response is affected by action and mother reared
		##initialize/create objects to store the results (there are created because then they also exist and can be 'touched' and
			##have a 'value' in case a model failed with an error or didn't converge)
		ii.res=NULL
		tests.full.main.only=NULL; tests.full.w.fam.cont.i=NULL; tests.full.w.fam.env.i=NULL
			tests.full.w.all.2wi=NULL; tests.full.w.env.cont=NULL; tests.full.w.all.i=NULL; fn.full.w.all.i=NULL
		fn.main.only=NULL; fn.full.w.fam.cont.i=NULL; fn.full.w.fam.env.i=NULL
			fn.full.w.env.cont=NULL; fn.full.w.all.2wi=NULL; fn.full.w.all.i=NULL
		coefs.full.main.only=NULL; coefs.full.w.fam.cont.i=NULL; coefs.full.w.fam.env.i=NULL
			coefs.full.w.env.cont=NULL; coefs.full.w.all.2wi=NULL; coefs.full.w.all.i=NULL
		re.coefs.full.main.only=NULL; re.coefs.full.w.fam.cont.i=NULL; re.coefs.full.w.fam.env.i=NULL
			re.coefs.full.w.env.cont=NULL; re.coefs.full.w.all.2wi=NULL; re.coefs.full.w.all.i=NULL
		warns.full.main.only=NULL; warns.full.w.fam.cont.i=NULL; warns.full.w.fam.env.i=NULL 
			warns.full.w.env.cont=NULL; warns.full.w.all.2wi=NULL; warns.full.w.all.i=NULL
		warns.null.main.only=NULL; warns.null.w.fam.cont.i=NULL; warns.null.w.fam.env.i=NULL
			warns.null.w.env.cont=NULL; warns.null.w.all.2wi=NULL; warns.null.w.all.i=NULL
    AICc.full.main.only=NULL; AICc.full.w.fam.cont.i=NULL; AICc.full.w.fam.env.i=NULL
			AICc.full.w.env.cont=NULL; AICc.full.w.all.2wi=NULL; AICc.full.w.all.i=NULL
		if(length(unique(xdata$rv))==2){##if both imitaiton and non-imitation aare present in the response: try to fit models
			##(otherwise one couldn't fit the model at all)
			##what follows are attempts to fit the six different models
			##extensive comments are only added to the first model; all others are treated correspondingly)
			##model: main only (see Table x and Figures 1 to 8):
			##try to fit full model:
			full.main.only=try(glmer(rv~fam+contact+env.eff+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+##fixed effects
				(1+fam.Y+con.Y+env.Y+z.action.order*z.demo||subj), ##random intercept and slopes (but no correlations among them)
				data=xdata, family=binomial, control=contr), silent=T)##control structure etc.
			if(class(full.main.only)!="try-error"){##if the full model didn't fail with an error:
				##try to fit null model:
				null.main.only=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+fam.Y+con.Y+env.Y+z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				##test 'dropable' terms in full model:
				tests.full.main.only=drop1p(model.res=full.main.only, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				##extract estimated coefficients, SE, etc.:
				coefs.full.main.only=summary(full.main.only)$coefficients
				##extract estimated contribution of random effects:
				re.coefs.full.main.only=as.data.frame(summary(full.main.only)$varcor)
				warns.full.main.only=lmer.warns(full.main.only)##extract warnings for full model
				##extract AICc for of full model
				AICc.full.main.only=summary(full.main.only)$AICtab["AIC"]+##AIC
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.main.only))+nrow(re.coefs.full.main.only))##+correction for small samples
				if(class(null.main.only)!="try-error"){##if the null model didn't fail with an error:
					##conduct full-null model comparison:
					fn.main.only=as.data.frame(anova(null.main.only, full.main.only, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.main.only=lmer.warns(null.main.only)
				}
			}
			##model: familiarity*contact
			full.w.fam.cont.i=try(glmer(rv~fam*contact+env.eff+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+
				(1+fam.Y*con.Y+env.Y+z.action.order*z.demo||subj), 
				data=xdata, family=binomial, control=contr), silent=T)
			if(class(full.w.fam.cont.i)!="try-error"){
				null.w.fam.cont.i=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+fam.Y*con.Y+env.Y+z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				tests.full.w.fam.cont.i=drop1p(model.res=full.w.fam.cont.i, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				coefs.full.w.fam.cont.i=summary(full.w.fam.cont.i)$coefficients
				re.coefs.full.w.fam.cont.i=as.data.frame(summary(full.w.fam.cont.i)$varcor)
				warns.full.w.fam.cont.i=lmer.warns(full.w.fam.cont.i)
				AICc.full.w.fam.cont.i=summary(full.w.fam.cont.i)$AICtab["AIC"]+
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.w.fam.cont.i))+nrow(re.coefs.full.w.fam.cont.i))
				if(class(null.w.fam.cont.i)!="try-error"){
					fn.full.w.fam.cont.i=as.data.frame(anova(null.w.fam.cont.i, full.w.fam.cont.i, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.w.fam.cont.i=lmer.warns(null.w.fam.cont.i)
				}
			}
			##model: familiarity*environment:
			full.w.fam.env.i=try(glmer(rv~fam*env.eff+contact+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+
				(1+fam.Y*env.Y+con.Y+z.action.order*z.demo||subj), 
				data=xdata, family=binomial, control=contr), silent=T)
			if(class(full.w.fam.env.i)!="try-error"){
				null.w.fam.env.i=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+fam.Y*env.Y+con.Y+z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				tests.full.w.fam.env.i=drop1p(model.res=full.w.fam.env.i, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				coefs.full.w.fam.env.i=summary(full.w.fam.env.i)$coefficients
				re.coefs.full.w.fam.env.i=as.data.frame(summary(full.w.fam.env.i)$varcor)
				warns.full.w.fam.env.i=lmer.warns(full.w.fam.env.i)
				AICc.full.w.fam.env.i=summary(full.w.fam.env.i)$AICtab["AIC"]+
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.w.fam.env.i))+nrow(re.coefs.full.w.fam.env.i))
				if(class(null.w.fam.env.i)!="try-error"){
					fn.full.w.fam.env.i=as.data.frame(anova(null.w.fam.env.i, full.w.fam.env.i, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.w.fam.env.i=lmer.warns(null.w.fam.env.i)
				}
			}
			##model: environment*contact
			full.w.env.cont=try(glmer(rv~fam+env.eff*contact+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+
				(1+fam.Y+env.Y*con.Y+z.action.order*z.demo||subj), 
				data=xdata, family=binomial, control=contr), silent=T)
			if(class(full.w.env.cont)!="try-error"){
				null.w.env.cont=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+fam.Y+env.Y*con.Y+z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				tests.full.w.env.cont=drop1p(model.res=full.w.env.cont, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				coefs.full.w.env.cont=summary(full.w.env.cont)$coefficients
				re.coefs.full.w.env.cont=as.data.frame(summary(full.w.env.cont)$varcor)
				warns.full.w.env.cont=lmer.warns(full.w.env.cont)
				AICc.full.w.env.cont=summary(full.w.env.cont)$AICtab["AIC"]+
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.w.env.cont))+nrow(re.coefs.full.w.env.cont))
				if(class(null.w.env.cont)!="try-error"){
					fn.full.w.env.cont=as.data.frame(anova(null.w.env.cont, full.w.env.cont, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.w.env.cont=lmer.warns(null.w.env.cont)
				}
			}
			##model: all two-way:
			full.w.all.2wi=try(glmer(rv~(fam+env.eff+contact)^2+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+
				(1+(fam.Y+con.Y+env.Y)^2+z.action.order*z.demo||subj), 
				data=xdata, family=binomial, control=contr), silent=T)
			if(class(full.w.all.2wi)!="try-error"){
				null.w.all.2wi=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+(fam.Y+con.Y+env.Y)^2+z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				tests.full.w.all.2wi=drop1p(model.res=full.w.all.2wi, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				coefs.full.w.all.2wi=summary(full.w.all.2wi)$coefficients
				re.coefs.full.w.all.2wi=as.data.frame(summary(full.w.all.2wi)$varcor)
				warns.full.w.all.2wi=lmer.warns(full.w.all.2wi)
				AICc.full.w.all.2wi=summary(full.w.all.2wi)$AICtab["AIC"]+
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.w.all.2wi))+nrow(re.coefs.full.w.all.2wi))
				if(class(null.w.all.2wi)!="try-error"){
					fn.full.w.all.2wi=as.data.frame(anova(null.w.all.2wi, full.w.all.2wi, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.w.all.2wi=lmer.warns(null.w.all.2wi)
				}
			}
			##model action type:
			full.w.all.i=try(glmer(rv~action+mother.reared+z.age+I(z.age^2)+z.action.order*z.demo+
				(1+d.aa.famN.conY.envN+d.aa.famN.conY.envY+d.aa.famY.conN.envN+d.aa.famY.conY.envN+d.aa.famY.conY.envY+
					z.action.order*z.demo||subj), 
				data=xdata, family=binomial, control=contr), silent=T)
			if(class(full.w.all.i)!="try-error"){
				null.w.all.i=try(glmer(rv~z.age+I(z.age^2)+z.action.order*z.demo+
					(1+d.aa.famN.conY.envN+d.aa.famN.conY.envY+d.aa.famY.conN.envN+d.aa.famY.conY.envN+d.aa.famY.conY.envY+
						z.action.order*z.demo||subj), 
					data=xdata, family=binomial, control=contr), silent=T)
				tests.full.w.all.i=drop1p(model.res=full.w.all.i, para=F, data=xdata, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F, 
					load.lib=F)$drop1.res
				coefs.full.w.all.i=summary(full.w.all.i)$coefficients
				re.coefs.full.w.all.i=as.data.frame(summary(full.w.all.i)$varcor)
				warns.full.w.all.i=lmer.warns(full.w.all.i)
				AICc.full.w.all.i=summary(full.w.all.i)$AICtab["AIC"]+
					aic.c.fac(N=nrow(xdata), k=length(fixef(full.w.all.i))+nrow(re.coefs.full.w.all.i))
				if(class(null.w.all.i)!="try-error"){
					fn.full.w.all.i=as.data.frame(anova(null.w.all.i, full.w.all.i, test="Chisq"))[2, c("Chisq", "Chi Df", "Pr(>Chisq)")]
					warns.null.w.all.i=lmer.warns(null.w.all.i)
				}
			}
			##wrap results in a list:
			ii.res=list(
				fn=list(fn.main.only=fn.main.only, fn.full.w.fam.cont.i=fn.full.w.fam.cont.i,
					fn.full.w.fam.env.i=fn.full.w.fam.env.i, fn.full.w.env.cont=fn.full.w.env.cont, 
					fn.full.w.all.2wi=fn.full.w.all.2wi, fn.full.w.all.i=fn.full.w.all.i),
				tests=list(tests.full.main.only=tests.full.main.only, tests.full.w.fam.cont.i=tests.full.w.fam.cont.i, 
          tests.full.w.fam.env.i=tests.full.w.fam.env.i, tests.full.w.env.cont=tests.full.w.env.cont, 
          tests.full.w.all.2wi=tests.full.w.all.2wi, tests.full.w.all.i=tests.full.w.all.i),
				fe.coef=list(coefs.full.main.only=coefs.full.main.only, coefs.full.w.fam.cont.i=coefs.full.w.fam.cont.i,
					coefs.full.w.fam.env.i=coefs.full.w.fam.env.i, coefs.full.w.env.cont=coefs.full.w.env.cont, 
					coefs.full.w.all.2wi=coefs.full.w.all.2wi, coefs.full.w.all.i=coefs.full.w.all.i),
				re.coef=list(re.coefs.full.main.only=re.coefs.full.main.only, re.coefs.full.w.fam.cont.i=re.coefs.full.w.fam.cont.i,
					re.coefs.full.w.fam.env.i=re.coefs.full.w.fam.env.i, re.coefs.full.w.env.cont=re.coefs.full.w.env.cont, 
					re.coefs.full.w.all.2wi=re.coefs.full.w.all.2wi, re.coefs.full.w.all.i=re.coefs.full.w.all.i),
				full.probl=list(warns.full.main.only=warns.full.main.only, warns.full.w.fam.cont.i=warns.full.w.fam.cont.i,
					warns.full.w.fam.env.i=warns.full.w.fam.env.i, warns.full.w.env.cont=warns.full.w.env.cont, 
					warns.full.w.all.2wi=warns.full.w.all.2wi, warns.full.w.all.i=warns.full.w.all.i),
        null.probl=list(warns.null.main.only=warns.null.main.only, warns.null.w.fam.cont.i=warns.null.w.fam.cont.i,
          warns.null.w.fam.env.i=warns.null.w.fam.env.i, warns.null.w.env.cont=warns.null.w.env.cont, 
          warns.null.w.all.2wi=warns.null.w.all.2wi, warns.null.w.all.i=warns.null.w.all.i),
				AIC=list(AICc.full.main.only=AICc.full.main.only, AICc.full.w.fam.cont.i=AICc.full.w.fam.cont.i, 
					AICc.full.w.fam.env.i=AICc.full.w.fam.env.i, AICc.full.w.env.cont=AICc.full.w.env.cont, 
					AICc.full.w.all.2wi=AICc.full.w.all.2wi, AICc.full.w.all.i=AICc.full.w.all.i),
				n.imit=sum(xdata$rv),
				rv=table(xdata$action, xdata$rv))
			return(ii.res)##and return the list
		}else{##if the response was constant no models could be fitted; return empty object
			return(NULL)
		}
	})
	all.res[[ip]]=ires##store the results in list all.res
	##and also save them (to secure them):
  save(file=paste(c("ip", ip, ".RData"), collapse=""), list="ires")
	print(paste(c(ip, " out of ", length(imit.probs), ": done"), collapse=""))##show progress
}
names(all.res)=imit.probs

##what follows is the evaluation of the results
	##these are not extensively commented as the code is highly specific to the idiosyncracies of the
	##list generated in the simulation and also because they are not essential for understanding what we have done
##determine number full models that could be fitted (i.e., did not fail with an error):
	xnames=c("main.only", "fam.cont", "fam.env", "env.cont", "all.2wi", "all.i")
	xx=lapply(all.res, function(x){
		x=lapply(x, function(y){
			if(!is.null(y)){
				!unlist(lapply(y[["fe.coef"]], is.null))
			}
		})
		xx=matrix(unlist(x), ncol=length(xnames), byrow=T)
		n=nrow(xx)
		colnames(xx)=xnames
		xx=apply(xx, 2, sum)
		return(c(xx, n=n))
	})
	xx=matrix(unlist(xx), nrow=length(xx), byrow=T)
	colnames(xx)=c(xnames, "n")
	rownames(xx)=imit.probs
	wt2(xx)
	xx.not.fail=xx
	##plot the result:
	dev.off()
	X11(width=10.4, height=5)
	par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
	options(scipen=5)
	plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1000), 
		xlab="maximum imitation probability", ylab="number data sets", xaxt="n")
	axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
	points(x=1:length(imit.probs), y=xx.not.fail[, "n"], pch=19, col="grey")
	pchs=c(1:6)
	for(i in 1:length(xnames)){
		points(x=1:length(imit.probs), y=xx.not.fail[, xnames[i]], pch=pchs[i])
	}
	legend("topleft", legend=c("nr. models attempted", "main only","familarity*contact","familarity*environment", "environment*contact", "all two-way", "action type"),
		pch=c(19, pchs), bty="n", col=c("grey", rep("black", length(xnames))))
	savePlot(file="n_not_fail.png", type="png")
	dev.copy2pdf(file="n_not_fail.pdf")

##determine number full models that converged without a warning:
	options(warn=2)
	xx=lapply(all.res, function(X){
		x=lapply(X, function(y){
			if(!is.null(y)){
				if("full.probl"%in%names(y)){y[["full.probl"]]}
			}
		})
		x=x[!unlist(lapply(x, is.null))]
		x=lapply(x, function(xx){
			lapply(xx, function(xxx){
				if(!is.null(xxx)){
					return(paste(xxx, collapse=""))
				}else{
					return(NA)
				}
			})
		})
		xx=try(matrix(unlist(x), ncol=length(xnames), byrow=T), silent=T)
		if(class(xx)=="try-error"){browser()}
		n=nrow(xx)
		colnames(xx)=xnames
		xx[xx=="boundary (singular) fit: see ?isSingular"]=""
		xx=apply(xx=="", 2, sum, na.rm=T)
		return(c(xx, n=n))
	})
	xx=matrix(unlist(xx), nrow=length(xx), byrow=T)
	colnames(xx)=c(xnames, "n")
	rownames(xx)=imit.probs
	wt2(xx)
	xx.conv=xx
	##plot the result:
	dev.off()
	X11(width=10.4, height=5)
	par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
	options(scipen=5)
	plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1000), 
		xlab="maximum imitation probability", ylab="number data sets", xaxt="n")
	axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
	points(x=1:length(imit.probs), y=xx.conv[, "n"], pch=19, col="grey")
	pchs=c(1:6)
	for(i in 1:length(xnames)){
		points(x=1:length(imit.probs), y=xx.conv[, xnames[i]], pch=pchs[i])
	}
	legend("topleft", legend=c("nr. models attempted", "main only","familarity*contact","familarity*environment", "environment*contact", "all two-way", "action type"),
		pch=c(19, pchs), bty="n", col=c("grey", rep("black", length(xnames))))
	savePlot(file="n_conv.png", type="png")
	dev.copy2pdf(file="n_conv.pdf")

##power of the full-null model comparisons:
	options(warn=2)
	xx=lapply(all.res, function(X){
		x=lapply(1:length(X), function(y){
			#browser()
			if(!is.null(X[[y]])){
				if("fn"%in%names(X[[y]])){
					X[[y]]$fn
				}
			}
		})
		sel=which(!unlist(lapply(x, is.null)))
		x=x[sel]
		x=lapply(x, function(y){
			lapply(y, function(yy){
				if(!is.null(yy)){yy[1, "Pr(>Chisq)"]}else{1}
			})
		})
		P=matrix(unlist(x), ncol=length(xnames), byrow=T)
		colnames(P)=xnames
		f.warns=lapply(X[sel], "[", "full.probl")
		f.warns=lapply(f.warns, function(xx){
			#browser()
			yy=lapply(xx[[1]], function(xxx){
				if(!is.null(xxx)){
					#browser()
					return(paste(xxx, collapse=""))
				}else{
					return(NA)
				}
			})
		})
		f.warns=matrix(unlist(f.warns), ncol=length(xnames), byrow=T)
		f.warns[f.warns=="boundary (singular) fit: see ?isSingular"]=""
		f.warns[f.warns!=""]="W"

		n.warns=lapply(X[sel], "[", "null.probl")
		n.warns=lapply(n.warns, function(xx){
			#browser()
			yy=lapply(xx[[1]], function(xxx){
				if(!is.null(xxx)){
					#browser()
					return(paste(xxx, collapse=""))
				}else{
					return(NA)
				}
			})
		})
		n.warns=matrix(unlist(n.warns), ncol=length(xnames), byrow=T)
		n.warns[n.warns=="boundary (singular) fit: see ?isSingular"]=""
		n.warns[n.warns!=""]="W"
		P[is.na(n.warns) | n.warns=="W"]=NA
		P[is.na(f.warns) | f.warns=="W"]=NA
		return(list(count=apply(P<=0.05, 2, sum, na.rm=T), n.models.cons=apply(!is.na(P), 2, sum, na.rm=T)))
	})
	power.fn=lapply(xx, "[", "count")
	power.fn=matrix(unlist(power.fn), ncol=length(xnames), byrow=T)
	colnames(power.fn)=xnames
	rownames(power.fn)=imit.probs
	wt2(power.fn)
	power.fn=power.fn/1000

	###plotting (power out of 1000 simulated data sets; i.e., considering models that failed with an error as non-significant):
	dev.off()
	X11(width=10.4, height=5)
	par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
	options(scipen=5)
	plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1), 
		xlab="maximum imitation probability", ylab="power", xaxt="n")
	axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
	pchs=c(1:6)
	for(i in 1:length(xnames)){
		points(x=1:length(imit.probs), y=power.fn[, xnames[i]], pch=pchs[i])
	}
	legend("topleft", legend=c("main only","familarity*contact","familarity*environment", "environment*contact", "all two-way", "action type"),
		pch=pchs, bty="n", ncol=2)#, col=c("grey", rep("black", length(xnames))))
	abline(h=0.8, lty=3)
	savePlot(file="power_full_null.png", type="png")
	dev.copy2pdf(file="power_full_null.pdf")
	
	##power (determining it as the proportion out of the models converged):
	rel.power.fn=lapply(xx, "[", "count")
	rel.power.fn=matrix(unlist(rel.power.fn), ncol=length(xnames), byrow=T)
	colnames(rel.power.fn)=xnames
	rownames(rel.power.fn)=imit.probs
	n.models.considered=lapply(xx, "[", "n.models.cons")
	n.models.considered=matrix(unlist(n.models.considered), ncol=length(xnames), byrow=T)
	colnames(n.models.considered)=xnames
	rownames(n.models.considered)=imit.probs
	rel.power.fn=rel.power.fn/n.models.considered
	wt2(rel.power.fn)
	###plotting:
	dev.off()
	X11(width=10.4, height=5)
	par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
	options(scipen=5)
	plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1), 
		xlab="maximum imitation probability", ylab="power", xaxt="n")
	axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
	pchs=c(1:6)
	for(i in 1:length(xnames)){
		points(x=1:length(imit.probs), y=rel.power.fn[, xnames[i]], pch=pchs[i])
	}
	legend("topleft", legend=c("main only","familarity*contact","familarity*environment", "environment*contact", "all two-way", "action type"),
		pch=pchs, bty="n", ncol=2)#, col=c("grey", rep("black", length(xnames))))
	abline(h=0.8, lty=3)
	savePlot(file="relat_power_full_null.png", type="png")
	dev.copy2pdf(file="relat_power_full_null.pdf")


##power of tests of indivdual effects:
	test.names=c("tests.full.main.only", "tests.full.w.fam.cont.i", "tests.full.w.fam.env.i", "tests.full.w.env.cont",
		"tests.full.w.all.2wi", "tests.full.w.all.i")
	all.tests2=lapply(all.res, function(m){
		xx=lapply(m, function(y){
			if(!is.null(y)){
				if(length(unlist(y[["tests"]]))>0){
					#browser()
					y[["full.probl"]][unlist(lapply(y[["full.probl"]], is.null))]=""
					w=unlist(lapply(y[["full.probl"]], paste, collapse=""))
					w=gsub(w, pattern="boundary (singular) fit: see ?isSingular", replacement="", fixed=T)
					y[["tests"]][w!=""]=NULL
					return(y)
				}
			}
		})
		xx=xx[!unlist(lapply(xx, is.null))]
		return(xx)
	})
		
	all.tests=lapply(all.tests2, function(x){
		xx=lapply(x, function(y){
			if(!is.null(y[["tests"]])){
				lapply(y[["tests"]], function(it){
					if(!is.null(y[["tests"]])){
						#browser()
						xx=it[-1, "Pr..Chisq."]
						names(xx)=rownames(it)[-1]
						return(xx)
					}else{
						return(NULL)
					}
				})
			}else{
				return(NULL)
			}
		})
		xx=xx[!unlist(lapply(xx, is.null))]
	})

	all.i.power=lapply(all.tests, function(x){
		x=lapply(1:length(test.names), function(i){
			xx=lapply(x, "[[", test.names[i])
			xnames=unique(unlist(lapply(xx, names)))
			xx=lapply(xx, "[", xnames)
			xx=matrix(unlist(xx), nrow=sum(unlist(lapply(xx, length))>0), byrow=T)
			colnames(xx)=xnames
			return(list(P=apply(xx<=0.05, 2, sum, na.rm=T)/1000, N=apply(!is.na(xx), 2, sum, na.rm=T)))
		})
		names(x)=test.names
		return(x)
	})
	all.ind.power=lapply(test.names, function(i){
		#browser()
		xx=lapply(all.i.power, "[[", i)
		xx=lapply(xx, "[[", "P")
		xnames=unique(names(unlist(xx)))
		xx=lapply(xx, "[", xnames)
		xx=matrix(unlist(xx), ncol=length(xnames), byrow=T)
		colnames(xx)=xnames
		rownames(xx)=imit.probs
		return(xx)
	})
	names(all.ind.power)=test.names
	##plotting:
		##main effects:
			##mother reared:
				dev.off()
				X11(width=10.4, height=5)
				par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
				options(scipen=5)
				plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1), 
					xlab="maximum imitation probability", ylab="power", xaxt="n")
				axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
				pchs=c(1:6)
				for(i in 1:length(test.names)){
					points(x=1:length(imit.probs), y=all.ind.power[[test.names[i]]][, "mother.reared"], pch=pchs[i])
				}
				legend("topleft", legend=c("main only","familarity*contact","familarity*environment", "environment*contact", "all two-way", "action type"),
					pch=pchs, bty="n", ncol=2)#, col=c("grey", rep("black", length(xnames))))
				abline(h=0.8, lty=3)
				savePlot(file="power_mother_reared.png", type="png")
				dev.copy2pdf(file="power_mother_reared.pdf")
			##main effects of familarity, contact involvement, and environmental effect:
				dev.off()
				X11(width=10.4, height=5)
				par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
				options(scipen=5)
				plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1), 
					xlab="maximum imitation probability", ylab="power", xaxt="n")
				axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
				pchs.main.wi=15:17
				names(pchs.main.wi)=c("fam", "contact", "env.eff")
				for(i in names(pchs.main)){
					points(x=1:length(imit.probs), y=all.ind.power[[test.names[1]]][, i], pch=pchs.main.wi[i], col=grey(level=0.3, alpha=0.5))
				}
				pchs.main=c(22, 1, 2)
				names(pchs.main)=c("fam", "contact", "env.eff")
				p.names=c("tests.full.w.env.cont", "tests.full.w.fam.env.i", "tests.full.w.fam.cont.i")
				for(i in 1:length(pchs.main)){
					points(x=1:length(imit.probs), y=all.ind.power[[p.names[i]]][, names(pchs.main)[i]], pch=pchs.main[i])
				}
				legend("topleft", legend=c(paste(c("familiarity in", "contact in", "environment in"), rep("main only", 3), sep=" "), "familiarity in environment*contact", "contact familarity*environment", "environment in in familarity*contact"),
					pch=c(pchs.main.wi, pchs.main), col=rep(c(grey(level=0.3, alpha=0.5), "black"), each=3), bty="n", ncol=2)#, col=c("grey", rep("black", length(xnames))))
				abline(h=0.8, lty=3)
				savePlot(file="power_main effects.png", type="png")
				dev.copy2pdf(file="power_main effects.pdf")
		##two-way interactions:
			dev.off()
			X11(width=10.4, height=5)
			par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
			options(scipen=5)
			plot(1, 1, type="n", xlim=c(1, length(imit.probs)), ylim=c(0, 1), 
				xlab="maximum imitation probability", ylab="power", xaxt="n")
			axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8)
			pchs.a2w=15:17
			names(pchs.a2w)=c("fam:env.eff", "fam:contact", "env.eff:contact")
			for(i in names(pchs.a2w)){
				points(x=1:length(imit.probs), y=all.ind.power[["tests.full.w.all.2wi"]][, i], pch=pchs.a2w[i], col=grey(level=0.3, alpha=0.5))
			}
			pchs.i2w=c(22, 1, 2)
			names(pchs.i2w)=c("tests.full.w.fam.env.i", "tests.full.w.fam.cont.i", "tests.full.w.env.cont")
			p.names=c("tests.full.w.fam.env.i", "tests.full.w.fam.cont.i", "tests.full.w.env.cont")
			for(i in 1:length(pchs.i2w)){
				points(x=1:length(imit.probs), y=all.ind.power[[p.names[i]]][, names(pchs.a2w)[i]], pch=pchs.i2w[i])
			}
			points(x=1:length(imit.probs), y=all.ind.power[["tests.full.w.all.i"]][, "action"], pch=4)
			legend("topleft", legend=c("familiarity*environment", "familiarity*contact", "environment*contact", 
					paste(c("familiarity*environment", "familiarity*contact", "environment*contact"), "in all two-way", sep=" "), "action type"),
				pch=c(pchs.a2w, pchs.i2w, 4), col=c(rep(c(grey(level=0.3, alpha=0.5), "black"), each=3), "black"), bty="n", ncol=2)#, col=c("grey", rep("black", length(xnames))))
			abline(h=0.8, lty=3)
			savePlot(file="interactions.png", type="png")
			dev.copy2pdf(file="interactions.pdf")
	
	##extract results regarding the AICc:
		all.aic=lapply(all.res, function(m){
			iaic=lapply(m, "[[", "AIC")
			iw=lapply(m, "[[", "full.probl")
			iw=iw[unlist(lapply(iaic, length))==6]
			iaic=iaic[unlist(lapply(iaic, length))==6]
			iw=lapply(iw, lapply, paste, collapse="")
			sel=unlist(lapply(iaic, function(x){length(unlist(x))==6}))#browser(); 
			iaic=matrix(unlist(iaic[sel]), nrow=sum(sel), byrow=T)
			iw=matrix(unlist(iw[sel]), nrow=sum(sel), byrow=T)
			iw=gsub(iw, pattern="boundary (singular) fit: see ?isSingular", replacement="", fixed=T)
			iaic=iaic[apply(nchar(iw), 1, sum)==0, ]
			colnames(iaic)=c("main.only", "fam.cont", "fam.env", "env.cont", "all.2wi", "action.type")
			return(list(which.best=table(colnames(iaic)[apply(iaic, 1, which.min)]), 
				div=apply(iaic, 2, function(x){length(unique(x))})/nrow(iaic), 
				n=nrow(iaic)))
		})
		which.best=lapply(all.aic, "[[", "which.best")
		which.best=lapply(which.best, function(x){
			to.add=setdiff(c("main.only", "fam.cont", "fam.env", "env.cont", "all.2wi", "action.type"), names(x))
			ta=rep(0, length(to.add))
			names(ta)=to.add
			x=c(x, ta)
			return(x[sort(names(x))])
		})
		which.best=matrix(unlist(which.best), nrow=length(which.best), byrow=T)
		colnames(which.best)=sort(c("main.only", "fam.cont", "fam.env", "env.cont", "all.2wi", "action.type"))
		rownames(which.best)=imit.probs
		div=unlist(lapply(all.aic, function(x){mean(x[["div"]])}))
		which.best=which.best[, c("main.only", "fam.cont", "fam.env", "env.cont", "all.2wi", "action.type")]
		to.plot=t(apply(which.best, 1, cumsum))
		to.plot=cbind(0, to.plot)
		hbw=0.25
		cols=rainbow(ncol(to.plot)-1)
		###plotting:
		dev.off()
		X11(width=10.4, height=5)
		par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
		plot(1, 1, type="n", ylim=c(0, 1010), xlim=c(1, length(imit.probs))+c(-0.5, 0.5),
			xlab="maximum imitation probability", ylab="number data sets", xaxt="n", yaxs="i", xaxs="i")
		axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8, tcl=0)
		for(i in 2:ncol(to.plot)){
			rect(xleft=(1:length(imit.probs))-hbw, xright=(1:length(imit.probs))+hbw,
				ybottom=to.plot[, i-1], ytop=to.plot[, i], col=cols[i-1])
		}
		legend("topleft", pch=15, bty="n", col=rev(cols),
			legend=rev(c("main effects only", "familarity*contact", "familarity*environment", "environment*contact", "all two-way", "action type")))
		savePlot(file="best_models.png", type="png")
		dev.copy2pdf(file="best_models.pdf")
		
	##extract number positive responses (i.e., imitation events):
		all.pos=lapply(all.res, function(m){
			m=m[!unlist(lapply(m, is.null))]
			iw=lapply(m, "[[", "full.probl")
			iw=lapply(iw, lapply, paste, collapse="")
			iw=matrix(unlist(iw), nrow=length(iw), byrow=T)
			iw=gsub(iw, pattern="boundary (singular) fit: see ?isSingular", replacement="", fixed=T)
			m=m[apply(iw=="", 1, any)]
			isum=unlist(lapply(m, function(n){
				sum(n$rv[, "1"])
			}))
			return(isum)
		})
		y.range=range(unlist(all.pos))
		to.add=lapply(all.pos, table)
		max(unlist(to.add))
		##plotting:
		size.fac=0.3
		hbw=0.25
		dev.off()
		X11(width=10.4, height=5)
		par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
		plot(1, 1, type="n", ylim=y.range, xlim=c(1, length(imit.probs))+c(-0.5, 0.5),
			xlab="maximum imitation probability", ylab="number data sets", xaxt="n", xaxs="i")
		axis(side=1, at=1:length(imit.probs), labels=imit.probs, cex.axis=0.8, tcl=0)
		for(i in 1:length(to.add)){
			points(x=rep(i, length(to.add[[i]])), y=as.numeric(names(to.add[[i]])), cex=sqrt(to.add[[i]])*size.fac,
				col=grey(level=0.5, alpha=0.5), pch=19)
			qs=quantile(all.pos[[i]], probs=c(0.25, 0.5, 0.75))
			rect(xleft=i-hbw, xright=i+hbw, ybottom=qs[1], ytop=qs[3])
			segments(x0=i-hbw, x1=i+hbw, y0=qs[2], y1=qs[2], lwd=2)
		}
		abline(h=c(seq(10, 40, by=10), seq(60, 90, by=10), seq(110, 140, by=10), 160), lty=3, col="grey")
		abline(h=seq(50, 150, by=50), lty=2, col="grey")
		savePlot(file="nr_imit.png", type="png")
		dev.copy2pdf(file="nr_imit.pdf")
		unlist(lapply(all.pos, median))
		lapply(all.pos, range)
		lapply(all.pos, quantile, probs=c(0.25, 0.75))
	
