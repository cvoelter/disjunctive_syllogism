n.courses=20
range.n.part=10:30
slope.water.cons=0.05
sd.water.cons=0.3
icpt=0.5#intercept


set.seed(1)
#sample number participants:
n.part=sample(x=range.n.part, size=n.courses, replace=T)
#generate response:
resp=icpt+n.part*slope.water.cons+rnorm(n=n.courses, sd=sd.water.cons)
#fit model:
model.res=summary(lm(resp~n.part))$coefficients
model.res["n.part", "Pr(>|t|)"]

n.simus=10
res=rep(x=NA, times=n.simus)
res


n.courses=20#sample size
range.n.part=10:30#range number participants
slope.water.cons=0.05#slope cons. against number participants
sd.water.cons=0.3#residual sd
icpt=0.5#intercept
n.simus=1000#number simulations
res=rep(x=NA, time=n.simus)
set.seed(1)
for(i in 1:n.simus){
  #sample number participants:
  n.part=sample(x=range.n.part, size=n.courses, replace=T)
  #generate response:
  resp=icpt+n.part*slope.water.cons+rnorm(n=n.courses, sd=sd.water.cons)
  #fit model:
  model.res=summary(lm(resp~n.part))$coefficients
  #determine and store result:
  res[i]=model.res["n.part", "Pr(>|t|)"]<=0.05
}
mean(res)


set.seed(1)
round(rnorm(5), 3)


sample(x=5,size=5, replace=F)
library(gdata)
resample(x=1:10, size=10)




number.courses=seq(from=10, to=30, by=4)#sample sizes (this time a vector)
range.n.part=10:30#range number participants
slope.water.cons=0.05#slope cons. against number participants
sd.water.cons=0.3#residual sd
icpt=0.5#intercept
all.power=rep(NA, times=length(number.courses))#create vector to store results
names(all.power)=number.courses#and name it
n.simus=1000#number simulations
set.seed(1)

for(j in 1:length(number.courses)){#outer loop for number.courses:
  n.courses=number.courses[j]#set number courses
  res=rep(x=NA, time=n.simus)
  
  for(i in 1:n.simus){
    #sample number participants:
    n.part=sample(x=range.n.part, size=n.courses, replace=T)
    #generate response:
    resp=icpt+n.part*slope.water.cons+rnorm(n=n.courses, sd=sd.water.cons)
    #fit model:
    model.res=summary(lm(resp~n.part))$coefficients
    #determine and store result:
    res[i]=model.res["n.part", "Pr(>|t|)"]<=0.05
  }#end of inner loop for number simulations
  all.power[j]=mean(res)#store results
}#end of outer loop for number.courses
all.power

##define residual variance

number.courses=seq(from=10, to=30, by=4)#sample sizes (this time a vector)
res.sd=seq(from=0.2, to=0.8, by=0.1)#vector with residual standard deviations
range.n.part=10:30#range number participants
slope.water.cons=0.05#slope cons. against number participants
icpt=0.5#intercept
#create matrix storing the results:
all.power.values=matrix(NA, nrow=length(res.sd), ncol=length(number.courses))
rownames(all.power.values)=res.sd#and name it
colnames(all.power.values)=number.courses#and name it
set.seed(1)
n.simus=1000#number simulations

#run the simulations:
for(k in 1:length(res.sd)){#first loop for residual sd
  sd.water.cons=res.sd[k]#set residual sd
    
  for(j in 1:length(number.courses)){#outer loop for number.courses:
    n.courses=number.courses[j]#set number courses
    res=rep(x=NA, time=n.simus)
    
    for(i in 1:n.simus){
      #sample number participants:
      n.part=sample(x=range.n.part, size=n.courses, replace=T)
      #generate response:
      resp=icpt+n.part*slope.water.cons+rnorm(n=n.courses, sd=sd.water.cons)
      #fit model:
      model.res=summary(lm(resp~n.part))$coefficients
      #determine and store result:
      res[i]=model.res["n.part", "Pr(>|t|)"]<=0.05
    }#end of inner loop for number simulations
   all.power.values[k, j]=mean(res)#store results
  }#end of second loop for number.courses
}#end of end first loop for residual sd

round(all.power.values, digits=3)#show results


###simulate a factor

n.per.level=20
sex=c("F", "M")
sex=as.factor(rep(x=sex, each=n.per.level))
table(sex)


####unbalanced factors
set.seed(1)
n=200
levels=c("control", "social", "alone")
condition=as.factor(sample(levels, size=n, replace=T))
table(condition)

#####unbalanced probabilities
set.seed(1)
n=200
levels=c("F", "M")
probs=c(2, 1)
sex=sample(levels, size=n, replace=T, prob=probs)
sex=as.factor(sex)
table(sex)

###several balanced factors

n.per.combin.of.levels=10
sex=c("F", "M")
condition=c("control", "social", "alone")
xdata=data.frame(expand.grid(sex=sex, condition=condition))
xdata

xdata=xdata[rep(x=1:nrow(xdata), each=n.per.combin.of.levels), ]
table(xdata$sex, xdata$condition)


####several unbalanced factors

set.seed(1)
n=200
probs=c(2, 1)
sex=sample(x=c("F", "M"), size=n, replace=T, prob=probs)
condition=sample(x=c("control", "social", "alone"),
                 size=n, replace=T)
sex=as.factor(sex)
condition=as.factor(condition)
table(sex, condition)


set.seed(1)
n=20
sex=c("F", "M")
condition=c("control", "social", "alone")
#begin with fully balanced data set:
xdata=data.frame(expand.grid(sex=sex, condition=condition))
#append missing rows by sampling from the rows of xdata:
xdata=rbind(xdata,
            xdata[sample(1:nrow(xdata), size=n-nrow(xdata), replace=T), ])
table(xdata$sex, xdata$condition)


###loop until condition is met

set.seed(1)
min.req=5#required minimum number of entries per cell
n=30#total sample size
sex=c("F", "M")#define entries for factor sex
condition=c("control", "social", "alone")#... and for factor condition
xtab=0#generate object to be evaluated by the while loop
while(min(xtab)<min.req){#begin while loop
  #generate data:
  xdata=data.frame(
    sex=sample(x=sex, size=n, replace=T),
    condition=sample(x=condition, size=n, replace=T)
  )
  #cross tabulate the two factors:
  xtab=table(xdata$sex, xdata$condition)
}
xtab

###simulating a response
m.mat=model.matrix(object=~sex+condition, data=xdata)
head(m.mat)

coefs=rep(x=0, times=ncol(m.mat))#begin with vector filled with 0s
names(coefs)=colnames(m.mat)#name it
coefs["(Intercept)"]=0#set intercept to 4
coefs["conditionsocial"]=8#and effect of social condition to 2
resid.sd=1#give residual standard deviation a value
rv=m.mat[, names(coefs)]%*%coefs+rnorm(n=n, sd=resid.sd)
par(mar=c(2.7, 2.7, 0.2, 0.2), mgp=c(1.5, 0.3, 0),
    tcl=-0.15, las=1, cex.lab=0.7, cex.axis=0.5)
plot(xdata$condition, rv)


n=100
pv1=rnorm(n=n)
pv2=runif(n=100, min=0, max=5)
pv3=sample(x=1:10, size=n, replace=T)


library(MASS)
n=100
set.seed(1)
means=rep(x=1, times=3)
vcov.mat=matrix(0, ncol=3, nrow=3)
diag(vcov.mat)=c(1, 7, 2)^2#square because mvrnorm expects variances
vcov.mat

pv.mat=mvrnorm(n=n, mu=means, Sigma=vcov.mat)
colnames(pv.mat)=paste("pv", 1:3)

apply(X=pv.mat, MARGIN=2, FUN=mean)

apply(X=pv.mat, MARGIN=2, FUN=sd)

round(cor(pv.mat), 3)



n=1000
set.seed(1)
means=rep(x=1, times=3)
vcov.mat=matrix(0, ncol=3, nrow=3)
diag(vcov.mat)=c(1, 7, 2)^2
vcov.mat[2, 1]=0.7*1*7
vcov.mat[1, 2]=vcov.mat[2, 1]
pv.mat=mvrnorm(n=n, mu=means, Sigma=vcov.mat)
colnames(pv.mat)=paste("pv", 1:3, sep=".")

apply(X=pv.mat, MARGIN=2, FUN=mean)
apply(X=pv.mat, MARGIN=2, FUN=sd)
round(cor(pv.mat), 3)

m.mat=model.matrix(object=~pv.1+pv.2+pv.3,
                   data=as.data.frame(pv.mat))
head(m.mat)

coefs=c(2, 1, 0, 1.5)
names(coefs)=colnames(m.mat)

rv=t(t(m.mat[, names(coefs)])*coefs)
head(rv, n=3)

set.seed(1)
rv=apply(X=rv, MARGIN=1, sum) + rnorm(n, sd=0.5)
head(rv)

set.seed(1)
rv=m.mat[, names(coefs)]%*%coefs + rnorm(n, sd=0.5)
head(as.vector(rv))

set.seed(1)
n=100
noise.sd=2
latent.pv=runif(n=n, min=0, max=10)
pv1=latent.pv+rnorm(n=n, sd=noise.sd)
pv2=latent.pv+rnorm(n=n, sd=noise.sd)
cor(pv1, pv2)

par(mar=c(2, 2, 0.2, 0.2), mgp=c(0.7, 0.15, 0),
    tcl=-0.1, cex.lab=0.5, cex.axis=0.5, las=1)
plot(pv1, pv2, pch=19, cex=0.5,
     col=grey(level=0.5, alpha=0.5))


##
##interactions
n=500#sample size
resid.sd=3#residual standard deviation
#range of predictors:
min.rain=-30
max.rain=30
min.grazing=-7.5
max.grazing=7.5

#define coefficients:
c.i=mean(c(10, 2, 10, 10))#intercept
c.rain=mean(c(0, (10-2)/(30--30)))#effect of rain (at grazing=0)
c.grazing=mean(c(0, (2-10)/(7.5--7.5)))#effect of grazing (at rain=0)
c.rg=(0-(2-10)/(7.5--7.5))/(30-(-30))#effect of the interaction


#generate predictors:
rain=runif(n=n, min=min.rain, max=max.rain)
grazing=runif(n=n, min=min.grazing, max=max.grazing)
#create model matrix:
m.mat=model.matrix(object=~rain*grazing, data=data.frame(rain, grazing))
head(m.mat)

#create vector with coefficients to be simulated:
coefs=rep(x=0, times=ncol(m.mat))
names(coefs)=colnames(m.mat)
coefs["(Intercept)"]=c.i
coefs["rain"]=c.rain
coefs["grazing"]=c.grazing
coefs["rain:grazing"]=c.rg
round(coefs, 4)#checking

#create response:
biom=m.mat[, names(coefs)]%*%coefs+rnorm(n=n, sd=resid.sd)
#have a look at model results:
round(coefficients(lm(biom~rain*grazing)), 4)


make.data<-function(min.rn, max.rn, min.gr, max.gr, N, error.sd, coefs){
  rain=runif(n=n, min=min.rn, max=max.rn)
  grazing=runif(n=n, min=min.gr, max=max.gr)
  m.mat=model.matrix(object=~rain*grazing, data=data.frame(rain, grazing))
  biom=m.mat[, names(coefs)]%*%coefs+rnorm(n=N, sd=error.sd)
  return(data.frame(rain, grazing, biom))
}

  test.data=make.data(min.rn=-30, max.rn=30, min.gr=-7.5, max.gr=7.5, N=n,
                      error.sd=resid.sd, coefs=coefs)
head(test.data)


##factors

n.per.cell=20
xdata=data.frame(expand.grid(task=c("easy", "compl"), age=c(5, 10)))
xdata$task=relevel(xdata$task, ref="easy")
xdata$age=factor(xdata$age, levels=c("5", "10"))
xdata=xdata[rep(x=1:nrow(xdata), each=n.per.cell), ]
str(xdata)