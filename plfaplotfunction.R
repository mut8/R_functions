plfa.plot<-function(plfa_conc, factor1=1, factor2=1, plfa_names="", plfa_groups=F, xlab="", ylab="", main="", group_lines=F, col=1) {

samples<-data.frame(matrix(nrow=nrow(plfa_conc), ncol=2))
rownames(samples)<-rownames(plfa_conc)
colnames(samples)<-c("factor1","factor2")
  samples$factor1<-as.factor(factor1)
  samples$factor2<-as.factor(factor2)

if(plfa_groups[1]!=F & length(plfa_groups==ncol(plfa_conc))) {
  plfa_conc.ord<-plfa_conc[,order(plfa_groups)]
  plfa_groups.ord<-plfa_groups[order(plfa_groups)]  
  plfa_names.ord<-plfa_names[order(plfa_groups)]
  
  print(plfa_conc.ord)
} else{
  plfa_conc.ord <- plfa_conc
  plfa_groups.ord <- plfa_groups
  plfa_names.ord<-plfa_names
  print('plfas not ordered')
  #print(plfa_conc.ord)
}

mat<-data.frame(matrix(ncol=2, nrow=length(levels(samples$factor1))*length(levels(samples$factor2))))
colnames(mat)<-c("factor1", "factor2")
mat$factor1<-rep(levels(samples$factor1), length(levels(samples$factor2)))

max.f1<-length(levels(samples$factor1))
max.f2<-length(levels(samples$factor2))

for (i in 1:max.f2)
    for (j in 1:max.f1) {
      mat$factor2[(i-1)*max.f1+j]<-levels(samples$factor2)[i]
      print((i-1*max.f1+j))
      print(levels(samples$factor2)[i])
    }

means<-matrix(ncol=ncol(plfa_conc.ord), nrow=length(levels(samples$factor1))*length(levels(samples$factor2)))
colnames(means)<-colnames(plfa_conc.ord)
rownames(means)<-paste(mat$factor1, mat$factor2)
error<-means         
         

for (i in 1:ncol(means))
  for (j in 1:nrow(means)) {
    means[j,i]<-mean(plfa_conc.ord[samples$factor1==mat$factor1[j]&samples$factor2==mat$factor2[j],i])
    error[j,i]<-sd(plfa_conc.ord[samples$factor1==mat$factor1[j]&samples$factor2==mat$factor2[j],i])
  }

print(means)
print(error)

cond2<-is.na(means)==F & is.na(error)==F
ylim=c(0, 1.4*max(means[cond2] + error[cond2]))

barplot2(means, plot.ci=T, ci.l=means-error, ci.u=means+error, beside=T, col=col, xaxt="n", main=main, ylab=ylab, ylim=ylim, space=c(0,1), tck=0.01)  

at<-1:ncol(plfa_conc.ord)*(1+nrow(mat))-0.5*(nrow(mat))

axis(1, at=at, plfa_names.ord, las=2)

#legend("topleft", pt.bg=colscale[(unique(as.numeric(samples$Region[cond])))], pch=22,         levels(samples$Region[cond]))
  
print(plfa_groups.ord)
tmp3<-rep(F, ncol(plfa_conc.ord))
for (i in 1:length(tmp3)) {
  tmp3[i]<- (plfa_groups.ord[i]!=plfa_groups.ord[i+1])
}

y1<-ylim[1]
y2<-ylim[2]
y.u<-y2-(y2-y1)*.075
y.l<-y2-(y2-y1)*.125
y.t<-y2-(y2-y1)*.025
ys<-c(y.l, y.u, y.u, y.l)

tmp4<-c(0,which(tmp3==T), length(tmp3))

for (i in 1:(length(tmp4)-1)) {
  xs<-c(rep(tmp4[i],2), rep(tmp4[i+1],2))*(1+nrow(mat))+c(+.2, +.2, -.2, -.2)
  lines(xs, ys)
  text(xs[1]-1, y.t, plfa_groups.ord[tmp4[i+1]], cex=.7, pos=4)
  
  
}

}


hor.plot <- function(var, hor, horlev, fac, legpl="none", col=1, pt.bg=1, col.inv=F, pch=c(21,22), legsize=1, cex.sig=1, cex.pt=1, er.type="sd", ...) {
  #fac<-samples$Region
  #horlev<-c("L","F","H","B")
  #hor<-samples$horizon.ord
  cond1<-is.element(hor,horlev)
  hor1<-factor(hor[cond1], ordered=T, levels=horlev)
  fac1<-factor(fac[cond1])
  var1<-var[cond1]
  
  means<-tapply(var1, list(fac1,hor1), mean)
  if(er.type=="se") {
    error<-tapply(var1, list(fac1,hor1), stderr)    
  }
  if(er.type=="sd") {
    error<-tapply(var1, list(fac1,hor1), sd)
  }
  if(er.type=="ci") {
    error<-tapply(var1, list(fac1,hor1), CI)
  }
  col<-rep(col, ncol(means))

  if(col.inv==F) {
    plot(means[,T], rep(ncol(means):1,nrow(means)), yaxt="n", type="n", xlim=c(0, 1.2*max(means+error)), tck=0.01, ...)
    for(i in 1:nrow(means)) {
      plotCI(means[i,T], ncol(means):1, err="x", uiw=error[i,T], type="o", pch=pch[i], lty=1, col=col[i], pt.bg=pt.bg[i], add=T, gap=0, cex=cex.pt, ...)
    }
    
    if (legpl != "none")
      legend(legpl, pch=pch[1:ncol(means)],col=col[1:ncol(means)], pt.bg=col[1:ncol(means)], rownames(means), cex=legsize)
    
    axis(2, at=ncol(means):1, labels=colnames(means), tck=0.01, las=1)
    axis(1, tck=0.01)
    axis(3, tck=0.01, labels=F)
    axis(4, tck=0.01, labels=F,  at=ncol(means):1)
    
    for(i in 1:ncol(means)) {
      cond3<-hor1==colnames(means)[i]
      plev<-anova(lm(var1[cond3]~fac1[cond3]))[1,"Pr(>F)"]
      print(plev)
      if(plev!="NaN"){
        text(max(means[,i]+error[,i])+max(means+error)*.1, ncol(means)+1-i, labels=siglev(plev), cex=cex.sig)
        
      }
  }
    
  } else {
    par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white")
  plot(means[,T], rep(ncol(means):1,nrow(means)), yaxt="n", type="n", xlim=c(0, 1.2*max(means+error)), tck=0.01, ...)
  for(i in 1:nrow(means)) {
    plotCI(means[i,T], ncol(means):1, err="x", uiw=error[i,T], type="o", pch=pch[i], lty=1, col=col[i], pt.bg=col[i], add=T, gap=0, ...)
  }
  if (legpl != "none")
    legend(legpl, pch=pch[1:ncol(means)], col="black", pch[1:ncol(means)], pt.bg=col[1:ncol(means)], rownames(means), bty="n", cex=legsize)
  
  axis(2, at=ncol(means):1, labels=colnames(means), las=1, col="white")
  axis(1, col="white")
  axis(3, col="white", labels=F)
  axis(4, col="white", labels=F,  at=ncol(means):1)

  for(i in 1:ncol(means)) {
    cond3<-hor1==colnames(means)[i]
    plev<-anova(lm(var1[cond3]~fac1[cond3]))[1,"Pr(>F)"]
    print(plev)
    if(plev!="NaN"){
      text(max(means[,i]+error[,i])+max(means+error)*.1, ncol(means)+1-i, labels=siglev(plev), cex=cex.sig)
      
    }
  }
  
}
}

