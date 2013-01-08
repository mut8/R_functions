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



