
hor.plot <- function(var, hor, horlev, fac, legpl="none", nested=F, col=1, pt.bg=1, col.inv=F, pch=c(21,22), lty=1, legsize=1, cex.sig=1, cex.pt=1, er.type="sd", er.type.nested="se", ...) {
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
  cond2<-is.na(means)==F & is.na(error)==F
  
  if (nested[1]!=F) {
    nested1<-nested[cond1]
    means -> means.old
    error -> error.old
    means.old
    
    for (i in 1:nrow(means.old))
    {fac<-rownames(means.old)[i]
     tmp <- unique(nested1[which(fac1==fac)])
     nest[i]<-as.character(factor(tmp, levels=unique(nested1)))}
    
    nested1 <- factor(nested1)
    means.old[nest=="ER",T]
    means<-data.frame(matrix(nrow=length(levels(nest)), ncol=ncol(means.old)))
    rownames(means)<-levels(nest)
    colnames(means)<-colnames(means.old)
    error <- means
    
    for (i in 1:nrow(means))
    {
      means[i,T] <- colMeans(means.old[nest==rownames(means)[i],T])
      for (j in 1:ncol(means)) 
      {
        if(er.type.nested=="se") 
        {
          error[i,j]<-stderr(means.old[nest==rownames(means)[i],j])
        }
        if(er.type.nested=="sd") 
        {
          error[i,j]<-sd(means.old[nest==rownames(means)[i],j])
        }
        if(er.type.nested=="ci") 
        {
          error[i,j]<-CI(means.old[nest==rownames(means)[i],j])
        }
      }
    }
  }
  
  cond2<-is.na(means)==F & is.na(error)==F
  
  plot(t(means[,T]), rep(ncol(means):1,nrow(means)), yaxt="n", 
       xlim=c(0,1.2*max(means[cond2]+error[cond2])), tck=0.01, type="n")
#    plot(means[,T], rep(ncol(means):1,nrow(means)), yaxt="n", type="n", xlim=c(0, 1.2*max(means[cond2]+error[cond2])), tck=0.01, ...)
  for(i in 1:nrow(means)) {
    tmp.mean<-as.numeric(as.vector(means[i,T]))
    tmp.er<-as.numeric(as.vector(error[i,T]))
    plotCI(tmp.mean, ncol(means):1, uiw=tmp.er, err="x", pch=pch[i], lty=lty[i], col=col[i], pt.bg=pt.bg[i], add=T, gap=0, cex=cex.pt, ...)
    lines(tmp.mean, ncol(means):1, col=pt.bg[i], lty=lty[i], ...)
  }
  
#   for(i in 1:nrow(means)) {
#       plotCI(means[i,T], ncol(means):1, err="x", uiw=error[i,T], type="o", pch=pch[i], lty=1, col=col[i], pt.bg=pt.bg[i], add=T, gap=0, cex=cex.pt, ...)
#     }
    
    if (legpl != "none")
      legend(legpl, pch=pch[1:ncol(means)],col=col[1:ncol(means)], pt.bg=col[1:ncol(means)], rownames(means), cex=legsize)
    
    axis(2, at=ncol(means):1, labels=colnames(means), tck=0.01, las=1)
    axis(1, tck=0.01)
    axis(3, tck=0.01, labels=F)
    axis(4, tck=0.01, labels=F,  at=ncol(means):1)

  if (nested[1]==F) {    
    for(i in 1:ncol(means)) {
      cond3<-hor1==colnames(means)[i]
          plev<-anova(lm(var1[cond3]~fac1[cond3]))[1,"Pr(>F)"]
          plev<-
            anova(lm(means.old[cond3]~fac1[cond3]))[1,"Pr(>F)"]
      print(plev)
      if(plev!="NaN"){
        text(max(means[,i]+error[,i])+max(means+error)*.1, ncol(means)+1-i, labels=siglev(plev), cex=cex.sig)
      }
      }
  }
    else {
        for(i in 1:ncol(means)) {
          cond3<-colnames(means.old)==colnames(means)[i]
          plev<-anova(lm(means.old[,cond3]~nest))[1,"Pr(>F)"]
          print(plev)
          if(plev!="NaN"){
            text(max(means[,i]+error[,i])+max(means+error)*.1, ncol(means)+1-i, 
                 labels=siglev(plev), cex=cex.sig)
       }
      }
    } 
}
