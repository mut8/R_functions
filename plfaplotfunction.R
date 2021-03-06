
hor.plot <- function(var, hor, horlev, fac, legpl="none", nested=F, col=1, pt.bg=1, col.inv=F, pch=c(21,22), lty=1, legsize=1, cex.sig=1, lwd=1, cex.pt=1, er.type="sd", er.type.nested="se", oneway=F, xlim=c(0,F), sig=T, addlines=F, ax=T, offset=0, sfrac=0.01, col.bar=1, lwd.bar=1, ...) {
  #fac<-samples$Region
  #horlev<-c("L","F","H","B")
  #hor<-samples$horizon.ord
  cond1<-is.element(hor,horlev)& is.na(var)==F
  hor1<-factor(hor[cond1], ordered=T, levels=horlev)
  fac1<-factor(fac[cond1], levels=unique(fac[cond1]))
  var1<-var[cond1]
  

  
  
  means<-tapply(var1, list(fac1,hor1), function(x) mean(x,na.rm=T))
    if(er.type=="se") {
      error<-tapply(var1, list(fac1,hor1), stderr)    
    }
    if(er.type=="sd") {
      error<-tapply(var1, list(fac1,hor1), function(x) sd(x, na.rm=T))
    }
    if(er.type=="ci") {
      error<-tapply(var1, list(fac1,hor1), CI)
    }
  
  error[is.na(error)] <- 0
   
  col<-rep(col, nrow(means))
  pt.bg<-rep(pt.bg, nrow(means))
  lty<-rep(lty, nrow(means))
  lwd<-rep(lwd, nrow(means))
  col.bar<-rep(col.bar, nrow(means))
  
  
  cond2<-is.na(means)==F & is.na(error)==F
  
  if (nested[1]!=F) {
    nested1<-nested[cond1]
    means -> means.old
    error -> error.old
    nest<-1   
    
    for (i in 1:nrow(means.old))
    {
      fac<-rownames(means.old)[i]
     tmp <- unique(nested1[which(fac1==fac)])

     nest[i]<-as.character(factor(tmp, levels=unique(nested1)))
    }
    
    nest<-as.factor(nest)

    nested1 <- factor(nested1)
    
    means<-data.frame(
      matrix(nrow=length(levels(nest)), 
             ncol=ncol(means.old)))
      
    rownames(means)<-levels(nest)
    colnames(means)<-colnames(means.old)
    
    error <- list(means, error)
    
    for (i in 1:nrow(means))
    {
      for (j in 1:ncol(means)) 
      {
        means[i,j] <- mean(means.old[nest==rownames(means)[i],j], na.rm=T)
        if(er.type.nested=="se") 
        {
          error[i,j]<-stderr(means.old[nest==rownames(means)[i],j])
        }
        if(er.type.nested=="sd") 
        {
          error[i,j]<-sd(means.old[nest==rownames(means)[i],j], na.rm=T)
        }
        if(er.type.nested=="ci") 
        {
          error[i,j]<-CI(means.old[nest==rownames(means)[i],j])
        }
      }
    }
  }
  
  error[is.na(error)]<-0
  
  cond2<-is.na(means)==F & is.na(error)==F
  
  if (xlim[2]==F) {xlim=c(0,1.2*max(means[cond2]+error[cond2]))}

if (addlines==F) {
  plot(t(means[,T]), rep(ncol(means):1,nrow(means)), yaxt="n", xaxt="n", 
       xlim=xlim, tck=0.01, type="n", ...)
} 


  
#    plot(means[,T], rep(ncol(means):1,nrow(means)), yaxt="n", type="n", xlim=c(0, 1.2*max(means[cond2]+error[cond2])), tck=0.01, ...)
  for(i in 1:nrow(means)) {
    y <- ncol(means):1 + offset*(i-nrow(means)/2-.5)
    tmp.mean<-as.numeric(as.vector(means[i,T]))
    tmp.er<-as.numeric(as.vector(error[i,T]))
    plotCI(tmp.mean, y, uiw=tmp.er, err="x", pch=pch[i], lty=1, col=col[i], pt.bg=pt.bg[i], add=T, gap=0, cex=cex.pt, barcol=col.bar[i], sfrac=sfrac, lwd=lwd.bar, ...)
    lines(tmp.mean, y, col=pt.bg[i], lwd=lwd[i], lty=lty[i])
  }
  
#   for(i in 1:nrow(means)) {
#       plotCI(means[i,T], ncol(means):1, err="x", uiw=error[i,T], type="o", pch=pch[i], lty=1, col=col[i], pt.bg=pt.bg[i], add=T, gap=0, cex=cex.pt, ...)
#     }

#print legend unless legpl="none"
if (legpl != "none")
      legend(legpl, pch=pch[1:nrow(means)],col=col[1:nrow(means)], pt.bg=pt.bg[1:nrow(means)], rownames(means), cex=legsize, lty=lty, lwd=1, bty="n")
  
#plot axis
    if (ax==T) {
    axis(2, at=ncol(means):1, labels=colnames(means), tck=0.01, las=1)
    axis(1, tck=0.01)
    axis(3, tck=0.01, labels=F)
    axis(4, tck=0.01, labels=F,  at=ncol(means):1)
    }
  
#plot asterices when significant differences within horizons
  if(sig==T) {

#when not nested
  if (nested[1]==F) {    
    for(i in 1:ncol(means)) {
      cond3<-hor1==colnames(means)[i]
          if (length(unique(fac1[cond3 & is.na(var1)==F]))>1) {
            plev<-anova(lm(var1[cond3]~fac1[cond3]))[1,"Pr(>F)"]
          }
          
      if(plev!="NaN"){
        text(max(means[,i]+error[,i])+ (par("xaxp")[2] - par("xaxp")[1])*.00, ncol(means)+1-i, labels=siglev(plev), cex=cex.sig, pos=4)
      }
      }
  }
  
#when nested samples
    else {
        for(i in 1:ncol(means)) {
          cond3<-colnames(means.old)==colnames(means)[i]
          if (length(unique(nest[cond3 & is.na(means.old)==F]))>1) {
          plev<-anova(lm(means.old[,cond3]~nest))[1,"Pr(>F)"]
          }
          if(plev!="NaN"){
            text(max(means[,i]+error[,i])+ (par("xaxp")[2] - par("xaxp")[1])*.00, ncol(means)+1-i, 
                 labels=siglev(plev), cex=cex.sig, pos=4)
       }
      }
    } 
  if (oneway==T) {
    lm0<-lm(var1~hor1)
    hsd<-HSD.test(lm0, "hor1", group=TRUE)
    tmp<-hsd[[5]]
    tmp$trt<-factor(tmp$trt, ordered=T, levels=horlev)
    print(tmp)
    text(rep(xlim[1], nrow(tmp)), nrow(tmp):1, tmp[order(tmp$trt),"M"])
    
  }
  }
  return(list(means, error))
}
