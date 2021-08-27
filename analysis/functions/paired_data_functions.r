require(exactRankTests)
better.wilcox<-function(vector1, vector2){
  #function running an exact wilcoxon test
  #and revealing a nice output
  #load the library required
  to.keep=!is.na(vector1) & !is.na(vector2)
	vector1=vector1[to.keep]
	vector2=vector2[to.keep]
	N=sum(vector1!=vector2)#get sample size
  w.res=wilcox.exact(vector1, vector2,
    paired=T, exact=T)
  wT=w.res$statistic
  wT=max(c(wT, N*(N+1)/2-wT))
  return(list(T.plus=wT, N=N,
    P=w.res$p.value))
}

paired.data.plot<-function(vector1, vector2, xlab="", ylab="", xnames, size.fac=c(1, NA, -1), ylim=NULL, size.by=c("area", "circles"), 
	pt.col="black", l.col="black", edge.width=0.5, n1=NULL, n2=NULL, cex.lab=1, col.vec=NULL, lwd=1, bty="o", pch=NULL, lty=NULL, reset.par=F,
	add.y.axis=T, up.down.ltys=NULL, cex.axis=1, cex.axis.lab=1, main=NULL, cex.main=1, change.x.line = 0.3){
	#version from July 09 2015
	#plots paired data as connected points
	#size.fac can be...
		#a numeric > 0 (the default is 1) in which case points are sized according to the number of points having the same value
		#NA in which case all points are depicted with the same size
		#a numeric <0 in which case points will be grey and semi-transparent (i.e., sample size will be visible by point density)
	#size.by can be...
		#"area" in which case the number of cases per point is depicted by the size of filled dots
		#"circles" in which case the number of cases per point is depicted the number of concentric circles pre point
  if(length(ylim)==0){
     ylim=range(c(vector1, vector2), na.rm=T)
  }
  old.par = par(no.readonly = TRUE)
  size.fac=size.fac[1]
	size.by=size.by[1]
	if(add.y.axis){
		add.y.axis="s"
	}else{
		add.y.axis="n"
	}
  if(is.null(main)){
		par(mar=c(3, 3, 0.2, 1.7), mgp=c(1.7, 0.3, 0), tcl=-0.15)
		plot(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), las=1, xaxt="n", ylim=ylim, 
			xlab=xlab, ylab=ylab, xlim=c(1-edge.width, 2+edge.width), type="n", xaxs="i", cex.lab=cex.axis.lab, cex.axis=cex.axis, bty=bty,
			yaxt=add.y.axis)
	}else{
		par(mar=c(3, 3, 1.2*cex.main, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15)
		plot(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), las=1, xaxt="n", ylim=ylim, 
			xlab=xlab, ylab=ylab, xlim=c(1-edge.width, 2+edge.width), type="n", xaxs="i", cex.lab=cex.axis.lab, cex.axis=cex.axis, bty=bty,
			yaxt=add.y.axis)
		mtext(text=main, side=3, line=0.3, cex.main=cex.main)
	}
  #browser()
	sel=!is.na(vector1) & !is.na(vector2)
	for.segments=aggregate(1:length(vector1[sel]), list(vector1[sel], vector2[sel]), length)
	if(length(lty)<length(vector1)){
		if(length(lty)==0){
			lty=rep(2, length(vector1))
		}
		if(length(up.down.ltys)==3){
			lty=up.down.ltys
		}else{
			lty=rep(lty, 3)
		}
		lty[for.segments[, 1]<for.segments[, 2]]=lty[1]
		lty[for.segments[, 1]==for.segments[, 2]]=lty[2]
		lty[for.segments[, 1]>for.segments[, 2]]=lty[3]
		segments(x0=rep(1, nrow(for.segments)), x1=rep(2, nrow(for.segments)), y0=for.segments[, 1], y1=for.segments[, 2], lty=lty, col=l.col, lwd=lwd*for.segments[, 3])
	}else{
		segments(x0=rep(1, nrow(for.segments)), x1=rep(2, nrow(for.segments)), 
			y0=vector1, y1=vector2, lty=lty, col=l.col, lwd=1)
	}
	if(length(pch)==0){pch=19}
	if(size.by=="area"){
		if(length(n1)==length(vector1) & length(n2)==length(vector2)){
			if(is.na(size.fac)){size.fac=1}
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=19, col="white", cex=size.fac*sqrt(c(n1, n2)))
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=pch, col=pt.col, cex=size.fac*sqrt(c(n1, n2)))
		}else if(is.na(size.fac)){
			if(length(col.vec)>0){pt.col=col.vec}
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=19, col="white")
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=pch, col=pt.col)
		}else if(size.fac<0){
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=19, col="white")
			points(x=rep(c(1, 2), each=length(vector1)), y=c(vector1, vector2), pch=pch, col=grey(0.25, alpha=0.5), col=col)
		}else if(length(pch)>1){
			if(is.na(size.fac)){size.fac=1}
			points(x=rep(1, length(vector1)), y=vector1, pch=pch, cex=size.fac, col="white")
			points(x=rep(2, length(vector2)), y=vector2, pch=pch, cex=size.fac, col="white")
			points(x=rep(1, length(vector1)), y=vector1, pch=pch, cex=size.fac, col=pt.col)
			points(x=rep(2, length(vector2)), y=vector2, pch=pch, cex=size.fac, col=pt.col)
		}else if(size.fac>0){
			x=table(vector1)
			y=table(vector2)
			points(x=rep(1, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*sqrt(x), col="white")
			points(x=rep(2, length(y)), y=as.numeric(names(y)), pch=19, cex=size.fac*sqrt(y), col="white")
			points(x=rep(1, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*sqrt(x), col=pt.col)
			points(x=rep(2, length(y)), y=as.numeric(names(y)), pch=19, cex=size.fac*sqrt(y), col=pt.col)
		}else if(size.fac>0){
			x=table(vector1)
			y=table(vector2)
			points(x=rep(1, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*sqrt(x), col="white")
			points(x=rep(2, length(y)), y=as.numeric(names(y)), pch=19, cex=size.fac*sqrt(y), col="white")
			points(x=rep(1, length(x)), y=as.numeric(names(x)), pch=pch, cex=size.fac*sqrt(x), col=pt.col)
			points(x=rep(2, length(y)), y=as.numeric(names(y)), pch=pch, cex=size.fac*sqrt(y), col=pt.col)
		}
  }else if(size.by=="circles"){
		if(is.na(size.fac)){size.fac=1}
		if(size.fac<=0){size.fac=1}
		#browser()
		x=table(vector1)
		y=table(vector2)
		points(x=rep(1, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*x, col="white")
		points(x=rep(2, length(y)), y=as.numeric(names(y)), pch=19, cex=size.fac*y, col="white")
		to.do=min(c(x, y)):max(c(x, y))
		for(i in to.do){
			points(x=rep(1, sum(x>=i)), y=as.numeric(names(x)[x>=i]), pch=1, cex=size.fac*i, col=pt.col)
			points(x=rep(2, sum(y>=i)), y=as.numeric(names(y)[y>=i]), pch=1, cex=size.fac*i, col=pt.col)
		}
	}
  mtext(text=xnames, side=1, at=c(1, 2), line=change.x.line+cex.lab-1, cex=cex.lab)
  if(reset.par){par(old.par)}
}

related.data.plot<-function(data, xlab="", ylab="", xnames=NULL, size.fac=c(1, NA, -1), ylim=NULL, size.by=c("area", "circles"), reset.par=T,
	add.y.axis=T, pt.col=grey(0.25, alpha=0.5), line.col="black", pt.cex=1, add.n=T, lwd=1, pch=NULL, lty=2, cex.lab=1, cex.axis=1,
	my.par=list(mar=c(1.5, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
	#version from Dec 25 2015
	#plots paired data as connected points
	#size.fac can be...
		#a numeric > 0 (the default is 1) in which case points are sized according to the number of points having the same value
		#NA in which case all points are depicted with the same size
		#a numeric <0 in which case points will be grey and semi-transparent (i.e., sample size will be visible by point density)
	#size.by can be...
		#"area" in which case the number of cases per point is depicted by the size of filled dots
		#"circles" in which case the number of cases per point is depicted the number of concentric circles pre point
  if(length(ylim)==0){
     ylim=range(data, na.rm=T)
  }
  old.par = par(no.readonly = TRUE)
  size.fac=size.fac[1]
	size.by=size.by[1]
  par(my.par)
  if(add.y.axis){
		plot(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)),
			las=1, xaxt="n", ylim=ylim, 
			xlab=xlab, ylab=ylab, xlim=c(0.5, ncol(data)+0.5), type="n", cex.axis=cex.axis, cex.lab=cex.lab)
	}else{
		plot(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)),
			las=1, xaxt="n", ylim=ylim, yaxt="n",
			xlab=xlab, ylab=ylab, xlim=c(0.5, ncol(data)+0.5), type="n", cex.axis=cex.axis, cex.lab=cex.lab)
	}
	for(i in 1:(ncol(data)-1)){
		segments(x0=rep(i, nrow(data)),
			x1=rep(i+1, nrow(data)),
			y0=data[,i],
			y1=data[, i+1],
			col=line.col, lwd=lwd, lty=lty)
	}
	#browser()
	if(size.by=="area"){
		if(length(pch)==0){pch=19}
		if(is.na(size.fac)){
			points(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)), pch=19, col="white", cex=pt.cex)
			points(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)), pch=pch, col=pt.col, cex=pt.cex)
		}else if(size.fac<0){
			points(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)), pch=19, col="white", cex=pt.cex)
			points(x=rep(1:ncol(data), each=nrow(data)), y=unlist(c(data)), pch=pch, col=pt.col, cex=pt.cex)
		}else if(size.fac>0){####
			for(i in 1:ncol(data)){
				x=table(data[,i])
				points(x=rep(i, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*sqrt(x)*pt.cex, col="white")
				points(x=rep(i, length(x)), y=as.numeric(names(x)), pch=pch, cex=size.fac*sqrt(x)*pt.cex, col=pt.col)
			}
		}
  }else if(size.by=="circles"){
		if(is.na(size.fac)){size.fac=1}
		if(size.fac<=0){size.fac=1}
		#browser()
		for(j in 1:ncol(data)){
			x=table(data[, j])
			points(x=rep(j, length(x)), y=as.numeric(names(x)), pch=19, cex=size.fac*x, col="white")
			for(i in unique(x)){
				points(x=rep(j, sum(x>=i)), y=as.numeric(names(x)[x>=i]), pch=1, cex=size.fac*i*pt.cex)
			}
		}
	}
	if(length(xnames)==0){
		mtext(text=colnames(data), side=1, at=c(1:ncol(data)), line=0, cex=cex.lab)
	}else{
		mtext(text=xnames, side=1, at=c(1:ncol(data)), line=0, cex=cex.lab)
	}
  if(reset.par){par(old.par)}
}


##################################################################################################
paired.data.plot.2<-function(predictor, response, group, N=NULL, xlab="", ylab="", xnames=NULL, size.fac=1, ylim=NULL, 
	pt.col=grey(level=0.5, alpha=0.5), l.col="black", edge.width=0.5, cex.lab=1, cex.axis=1, bty="o", pch=19, lwd=1, lty=2, reset.par=F,
	add.y.axis=T, up.down.ltys=NULL, predictor.seq=NULL){
	#version from april 27 2017
	#plots paired/related data as connected points
  if(length(ylim)==0){
     ylim=range(response, na.rm=T)
  }
	if(!is.factor(predictor)){predictor=as.factor(predictor)}
	if(!is.null(predictor.seq)){
		predictor=factor(predictor, levels=predictor.seq)
	}
  if(is.null(xnames)){xnames=levels(predictor)}
  old.par = par(no.readonly = TRUE)
	if(add.y.axis){
		add.y.axis="s"
	}else{
		add.y.axis="n"
	}
  par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15)
  plot(x=as.numeric(predictor), y=response, las=1, xaxt="n", ylim=ylim, 
    xlab=xlab, ylab=ylab, xlim=c(1-edge.width, length(unique(predictor))+edge.width), type="n", xaxs="i", cex.lab=cex.lab, cex.axis=cex.axis, bty=bty,
    yaxt=add.y.axis)
  to.plot=tapply(response, list(group, predictor), mean, na.rm=T)
  for(i in 1:(ncol(to.plot)-1)){
		segments(x0=i, x1=i+1, y0=to.plot[, i], y1=to.plot[, i+1], lwd=lwd, lty=lty)
	}
	points(x=as.numeric(predictor), y=response, pch=19, col="white", cex=size.fac*sqrt(N))
	points(x=as.numeric(predictor), y=response, pch=pch, col=pt.col, cex=size.fac*sqrt(N))
	
  mtext(text=xnames, side=1, at=1:length(xnames), line=0.2+cex.lab-1, cex=cex.lab)
  if(reset.par){par(old.par)}
}
