library("vegan")
library("MASS")
library("Hmisc")
library("gplots")
library("TeachingDemos")
library("agricolae")
library("lawstat")
library("multcompView")
#library("RXKCD")
library("xtable")
library("lattice")
##################
##default colors##
##################

errcol<-"black"
lwd<-.3
cex<-2
lty<-1
pch<-c(1,16,2,17)



transcomp<-function(x)
{
  mat<-matrix(ncol=ncol(x)+1, nrow=nrow(x)+1)
  colnames<-colnames(x)
  names<-c(colnames[1], rownames(x))
  colnames(mat)<-names
  rownames(mat)<-names
  mat[2:nrow(mat), 1:ncol(mat)-1]<-x
  for (i in 1:nrow(mat)) {mat[i,i]<-1}
  return(mat)
}

                                        #funktion für standard error und CI
stderr <- function(x) sqrt(var(x[is.na(x)==F])/length(x[is.na(x)==F]))
CI <- function(x, alpha=0.05) qt(1-(alpha/2),df=length(x[is.na(x)==F])-1) * sqrt(var(x[is.na(x)==F])/length(x[is.na(x)==F]))
CI2<- function(sd, n, alpha=0.05) qt(1-(alpha/2),df=n-1) * sqrt(sd*sd/n)

sumif<-function(df,sep) {
  dfcol<-ncol(df)
  seplev<-levels(sep)
  sepnr<-length(seplev)
  sumsh<-as.data.frame(matrix(0, ncol=sepnr, nrow=length(df[,1])))
  colnames(sumsh)<-seplev
  sumsh

  for (j in 1:sepnr) {
    for (i in 1:dfcol) {

      if (sep[i]==seplev[j]) sumsh[,j]<-sumsh[,j]+df[,i]

    }
  }

  return(sumsh)
}

siglev<-function(x, no.ns=F)
{
  x<-as.numeric(x)
  if(is.na(x)) {return ("NA")} else
  if(x < 0.001)  {return("***")} else
  if(x < 0.01 ) {return("**")} else
  if (x < 0.05) {return ("*")} else
#  if (x < 0.1) {return (".")} else
  if(no.ns==F) {return("n.s.")} else {return("")}
}
                                        #rotate point around origin

########################
##barplot mit defaults##
########################

barplot.def<-function(height, error, ylab="", names.arg="", main="",
                      col=colscale, ylim=F, xlab="", ...)
{
  if (length(ylim)==1)
    {
      if(max(height+error)>0)
        ylim[2]<-max(height+error)
      if(min(height-error)<0)
        ylim[1]<-min(height-error)
    }

  barplot2(height, ylim=c(ylim[1], ylim[2]*1.2), plot.ci=TRUE, ci.u=height+error,ci.l=height, names.arg=names.arg,
           main=main, ylab=ylab, col=col, tck=0.01, xlab=xlab, ...)

  abline(h=0, col="black")
}

#############################
##barplots mit mean & error##
#############################

bplot<-function(x, y, col="black", err="stderr", main="", ylab="", ci=0.05, ylim=F, xlab="", order=levels(y), ...) 
{            

var<-x[is.na(x)!=T]
sep<-as.factor(y[is.na(x)!=T])
#if(length(order)>1)
#  order<-levels(y)

aov<-aov(lm(var ~ sep))
hsd<-HSD.test(aov, "sep")

mat<-data.frame(matrix(ncol=3, nrow=length(order)))
colnames(mat)<-c("ord","mean", "error")

for (i in 1:length(order))
{
mat$ord[i]<-order[i]
mat$mean[i]<-mean(var[sep==order[i]])
if(err=="stderr")
    mat$error[i]<-stderr(var[sep==order[i]])
if(err=="ci")
    {
      sds<-sd(var[sep==order[i]])
      mat$error[i]<-CI2(sds, 5, ci)
    }
}

barplot.def(mat$mean, mat$error, ylab=ylab, names.arg=mat$ord, main=main, ylim=ylim, xlab=xlab, ...)

for (i in 1:length(order))
text((i-.4)*1.2, max(mat$mean+mat$error)*1.15, hsd$M[which(mat$ord[i]==hsd$trt)])
  
}

#######################
##functions for pairs##
#######################
                                        #!!colors and pch hardcoded - change below!!


panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-cor(x, y)
  cor<-cor.test(x,y)
  sig<-siglev(cor$p.value)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, "R= ", txt, " ", sig, sep="")
  cex <- 1
  text(0.5, 0.5, txt, cex = cex)
}

l.panel<- function(x, y, ...) {
  points(x,y, pch=pch.all, cex=.3)
  cor<-cor.test(x,y)
  if(cor$p.value<0.05) {abline(lm(y~x))}
}

timeseries.panel<- function(x, ...) {
  plot<-timeseries(x,harvest, type, pch=pch, col=colscale, endsig=T, type="o", add=T)
  plot(plot$y,plot$massloss, type="n", add=T)
  timeseries(x,harvest, type, pch=pch, col=colscale, endsig=T, type="o", add=T)
}



#####################
##ord plot function##
#####################
##plots the means and x/y st errors by two factors of an ordination

ord.plot<-function(ord, site.sep1, site.sep2, spe.labels="o", spe.label.type="point", col="black", pt.bg="black", pch=1, name="", spe.mult=1, sep1.unit="", sep2.unit="", arrow=F, labname="PCA", choices=1:2, spe.cex=0.4, site.cex=1, cex.leg=1, leg.sep1=T, leg.sep2=T, xaxt="s", yaxt="s", xlim=c(-1,1),ylim=c(-1,1),...)
{
  sep1.lev<-unique(as.factor(site.sep1))
  sep2.lev<-unique(as.factor(site.sep2))

  if(length(col) < length(sep1.lev))
    col <- rep(col, length(sep1.lev))
  if(length(pch) < length(sep2.lev))
    pch <- rep(pch, length(sep2.lev))
  if(length(pt.bg) < length(sep2.lev))
    pt.bg <- rep(pt.bg, length(sep2.lev))
  
  
  xvar<-eigenvals(ord)/sum(eigenvals(ord))
  print(xvar)
  
  if(is.na(xvar[1])) {
    xlab<-paste(labname, "1 ", sep="")
    ylab<-paste(labname, "2 ", sep="")
  } else {
    xlab<-paste(labname, choices[1], " ", formatC(xvar[choices[1]]*100, digits=3), "% variance", sep="")
    ylab<-paste(labname, choices[2], " ", formatC(xvar[choices[2]]*100, digits=3), "% variance", sep="")
    
  }
  
  
  
  plot(ord, choices=choices, type="n", tck=.01,
       xlab=xlab, ylab=ylab, xaxt=xaxt, yaxt=yaxt, ...)

  
  scores<-(scores(ord, display="species", choices=1:2))
  if (spe.label.type=="point")
    points(scores[,1]*spe.mult, scores[,2]*spe.mult, pch=spe.labels, cex=spe.cex)
  if (spe.label.type=="text")
    text(scores[,1]*spe.mult, scores[,2]*spe.mult, labels=spe.labels, cex=spe.cex)
  
  scores.sites<-(scores(ord, display="sites", choices=choices))
                                        #        for (i in 1:length(typlev))
                                        #        for (j in 1:length(harlev))
                                        #        points(ord, choices=c(1,2),  display="sites", select=harvest==harlev[j] & type==typlev[i], col=colscale[j], pch=pch[i], cex=.6)
                                        #     mat<-matrix(nrow=length(typlev)*length(harlev), ncol=6)
                                        #      colnames(mat)<-c("type", "harvest", "pca1", "pca1.error", "pca2", "pca2.error")
  

  
  
  for (i in 1:length(sep1.lev))
    for (j in 1:length(sep2.lev))
      {
        x<-mean(scores.sites[site.sep1==sep1.lev[i] & site.sep2==sep2.lev[j],1])
        y<-mean(scores.sites[site.sep1==sep1.lev[i] & site.sep2==sep2.lev[j],2])
        x.err<-stderr(scores.sites[site.sep1==sep1.lev[i] & site.sep2==sep2.lev[j],1])
        y.err<-stderr(scores.sites[site.sep1==sep1.lev[i] & site.sep2==sep2.lev[j],2])
        plotCI(x, y, uiw=y.err, liw=y.err, col=col[i], pch=pch[j], cex=site.cex, add=T, gap=0, pt.bg=pt.bg[i])
        plotCI(x, y, uiw=x.err, liw=x.err, err="x", col=col[i], pch=pch[j], cex=site.cex, add=T, gap=0, pt.bg=pt.bg[i])
                                        #      print(paste(x, x.err,y, y.err))
                                        #      mat[length(harlev)*(i-1)+j, T]<-c(typlev[i], harlev[j], x,x.err,y,y.err)
      
      if(i!=1 & arrow==T)  
      #?arrow
      #?abline
      {  
      lastx<-mean(scores.sites[site.sep1==sep1.lev[i-1] & site.sep2==sep2.lev[j],1])
      lasty<-mean(scores.sites[site.sep1==sep1.lev[i-1] & site.sep2==sep2.lev[j],2])
      line(c(lastx,lasty), c(x,y), arrow=arrow(length=unit(0.25, "cm"), type="closed", angle=10))
      }
    }
                                        #      mat
                                        #      write.csv(mat, "export/dif.sites.csv")
                                        #      print(spe.labels)
  #      write.csv(data.frame(scores,peaks$orig), "export/dif.species.csv")
  
  title(name)
if(leg.sep1==T)  legend("bottomright", pch=pch, col=col[1], pt.bg=pt.bg[1], paste(sep2.lev, sep2.unit), bty="n", cex=cex.leg)
if(leg.sep2==T)  legend("bottomleft", pch=pch[1], col=col, pt.bg=pt.bg, paste(sep1.lev, sep1.unit), bty="n", cex=cex.leg)

}


###################
##dif by function##
###################
##samples are grouped by factor "by". for each col of data and for each group, the mean of all rows that match cond and are part of the group is substracted from each value of the group.
##application: for n different treatments, substract the mean of all initial measurements in a given treatment from all measurements from the treatment.

dif.by<-function (data, cond, by)
{
  dif<-data
  mea<-data
  for (i in 1:nrow(dif))
    mea[i,T]<-colMeans(data[cond & by==by[i], T])
  dif<-data-mea
  return(dif)
}


rot<-function(x,del)
{
  r<-sqrt(x[,1]*x[,1]+x[,2]*x[,2])
  alpha<-atan2(x[,2], x[,1])

  alphaneu<-(alpha+del)
  x.neu<-r*cos(alphaneu)
  y.neu<-r*sin(alphaneu)
  neu<-data.frame(x.neu,y.neu)
  colnames(neu)<-paste(colnames(x), ".rot", sep="")
  return(neu)
}




#########################################
#correation matrix between two dataframes#
#########################################

corr.ab<-function(a,b, pomit=F, tex=F, alpha=0.05, digits=3)
{
  if (pomit==T) {
    var1<-matrix(nrow=ncol(a), ncol=2*ncol(b))
    } else if (tex==T) {
      var1<-matrix(nrow=ncol(a), ncol=ncol(b))
      } else {
        var1<-matrix(nrow=ncol(a), ncol=3*ncol(b))
        }

  rownames(var1)<-colnames(a)
  cnames<-vector(length=length(colnames(var1)))
  prenames<-colnames(b)

  if (pomit==F & tex==F) {
    for (i in 1:length(colnames(b)))
      {
        cnames[i*3-2]<-paste(prenames[i], "R")
        cnames[i*3-1]<-paste(prenames[i], "p")
        cnames[i*3]<-paste(prenames[i], "sig")
      }
  } else if (pomit==T) {
    for (i in 1:length(colnames(b)))
      {
        cnames[i*2-1]<-paste(prenames[i], "R")
        cnames[i*2]<-paste(prenames[i], "sig")
      }
  } else {cnames<-prenames}


  colnames(var1)<-cnames

  for (i in 1:ncol(a))
    for (j in 1:ncol(b))
      {
        cond<-is.na(a[,i]) == FALSE & is.na (b[,j]) == FALSE
        ctest<-cor.test(as.numeric(a[cond,i]),as.numeric(b[cond,j]))
        if (pomit==F & tex==F) {
          var1[i,j*3-2]<-formatC(ctest$estimate, digits=3)
          var1[i,j*3-1]<-formatC(ctest$p.value, digits=4)
          var1[i,j*3]<-siglev(var1[i,j*3-1])
        } else if (pomit==T) {

          var1[i,j*2-1]<-ctest$estimate
          tmp<-ctest$p.value
          var1[i,j*2]<-siglev(tmp)
        } else { 
          if (ctest$p.value < alpha) { 
            var1[i,j] <- paste("\\textbf{", formatC(ctest$estimate, digits=digits, format = "f"), "}" )
             } else { 
               var1[i,j]<-formatC(ctest$estimate, digits=digits, format = "f")
               }
        }
      }
  return(var1)
}

######################
##timseries function##
######################



                                        #timeseries funktion
                                        #mat = datenmatrix
                                        #xfac = factor x axis
                                        #sep fac = factor seperating plots
                                        #nam = diagram title names


timeseries <- function(y, xfac, sepfac, nam="", xlab="", ylab="",
                       massloss=0, masslossSE=0, ylim=c(F,F), xlim=c(F,F),
                       ci=F, allpoints=FALSE, legend=F, legsig=T, endsig=F, pt.bg=1, topsig=T,
                       col=1, lwd=1, lty=1, pch=20, normalize=0, add=F, errcol="black", type="o",text.cex=1, letters=F, ...) {
                                        #xfac<-as.factor<-as.numeric(xfac)

                                        #  nam<-""
                                        #  xlab<-""
                                        #  ylab<-""
                                        #  add<-F
                                        #  y<-orig_rsim[,i]
                                        #  xfac<-days
                                        #  sepfac<-type
                                        #  massloss<-0
                                        #  masslossSE<-0
                                        #  ymin<-F
                                        #  ymax<-F
                                        #   xmin<-F
                                        #   xmax<-F
                                        #  ci<-F
                                        #   points<-T
                                        #   allpoints<-FALSE
                                        #  legende<-T
                                        #   legsig<-T
                                        #   endsig<-F
                                        #
                                        #   col<-1
                                        #   lwd<-1
                                        #   lty<-1
                                        #   lines<-T
                                        #   pch<-20
                                        #   normalize<-0
                                        #  add<-T
                                        #   errcol<-"darkgrey"

  xlev<-sort(as.numeric(levels(factor(xfac))))
  xf<-as.numeric(xfac[is.na(y)==F])
  sepfac<-as.factor(sepfac[is.na(y)==F])
  seplev<-levels(sepfac)
  y<-as.numeric(y[is.na(y)==F])
  if(length(col)<length(seplev)) col<-rep(col, length(seplev))
  if(length(lwd)<length(seplev)) lwd<-rep(lwd, length(seplev))
  if(length(lty)<length(seplev)) lty<-rep(lty, length(seplev))
  if(length(pch)<length(seplev)) pch<-rep(pch, length(seplev))
  if(length(pt.bg)<length(seplev)) bg<-rep(pt.bg, length(seplev))


  mat<-data.frame(matrix(nrow=length(xlev)*length(seplev), ncol=6))
  colnames(mat)<-c("sepfac", "xfac", "y", "y.errbar","massloss", "massloss.errbar" )

  mat$xfac<-rep(xlev, length(seplev))
  mat$sepfac<-sort(rep(seplev,length(xlev)))



  for (i in 1:nrow(mat))
    {
      mat$y[i]<-mean(y[sepfac==mat$sepfac[i] & xfac==mat$xfac[i]], na.rm=T)
      if(ci==F) {mat$y.errbar[i]<-stderr(y[sepfac==mat$sepfac[i] & xfac==mat$xfac[i]])} else
      {mat$y.errbar[i]<-CI(y[sepfac==mat$sepfac[i] & xfac==mat$xfac[i]],ci)}
      if(length(massloss)>1) {mat$massloss[i]<-mean(massloss[sepfac==mat$sepfac[i] & xfac==mat$xfac[i]])} else {mat$massloss[i]<-mat$xfac[i]}
      if(length(masslossSE)>1) {mat$massloss.errbar[i]<-mean(masslossSE[sepfac==mat$sepfac[i] & xfac==mat$xfac[i]])} else {mat$massloss.errbar[i] <- 0}
    }

  if (normalize>0)
    for (i in 1:length(seplev))
      {
        mat$y.errbar[mat$sepfac==seplev[i]]<-100*mat$y.errbar[mat$sepfac==seplev[i]]/mat$y[mat$sepfac==seplev[i]&mat$xfac==xlev[normalize]]
        mat$y[mat$sepfac==seplev[i]]<-100*mat$y[mat$sepfac==seplev[i]]/mat$y[mat$sepfac==seplev[i]&mat$xfac==xlev[normalize]]-100
        mat$massloss[mat$sepfac==seplev[i]]<-mat$massloss[mat$sepfac==seplev[i]]-mat$massloss[mat$sepfac==seplev[i]&mat$xfac==xlev[normalize]]
      }

  if (normalize<0)
    for (i in 1:length(seplev))
      {
        mat$y.errbar[mat$sepfac==seplev[i]]<-mat$y.errbar[mat$sepfac==seplev[i]]/mean(mat$y[mat$sepfac==seplev[i]])
        mat$y[mat$sepfac==seplev[i]]<-mat$y[mat$sepfac==seplev[i]]/mean(mat$y[mat$sepfac==seplev[i]])
      }


  if (ylim[1]==F){ymin<-min(mat$y)} else {ymin=ylim[1]}
  
  if (ylim[2]==F) {ymax<-max(mat$y)+(max(mat$y)-min(mat$y))*0.1} else {ymax=ylim[2]}
  
  if (xlim[1]==F) {xmin<-min(mat$massloss)} else {xmin=xlim[1]}
                                        #if (ymax==F)
  if (xlim[2]==F) {
    if(endsig==T) {xmax<-max(mat$massloss)+(max(mat$massloss)-min(mat$massloss))*0.1} else {xmax<-max(mat$massloss)}
  } else {xmax=xlim[2]}
                                        #if (length(massloss)==0) {xlim<-c(xmin, xmax)} else {c(xmax, xmin)}
                                        #ylim<-c(ymin, ymax)
  xlim<-c(xmin, xmax)
  ylim<-c(ymin, ymax)

  if (normalize==0)
    imin<-1
  if (normalize>0)
    imin<-normalize

  if (add==F)
    {
      plot(mat$massloss, mat$y, xlim=xlim, ylim=ylim,  xlab=xlab, ylab=ylab, tck=.01, type="n", ...)
      title(nam)
    }
                                        #i sind die unterschiedlichen treatments, j die unterschiedlichen zeitpunkte
  for (i in 1:length(seplev)) {

    select<-mat$sepfac==seplev[i]
    if (ci==F) {plotCI(mat$massloss[select], mat$y[select], barcol=errcol, uiw=mat$y.errbar[select], liw=mat$y.errbar[select], add=TRUE, gap=0,
          type=type, pch=pch[i], col=col[i], lwd=lwd, lty=1, pt.bg=pt.bg[i], ...)} else
    {plotCI(mat$massloss[select]+(xmax-xmin)*0.004*(i-imax/2), mat$y[select],  uiw=mat$y.erbar[select], liw=mat$y.errbar[select], add=TRUE, gap=0, barcol=errcol, type=type, pt.bg=pt.bg[i], pch=pch[i], col=col[i], lwd=lwd, lty=lty[i], ...)}

    if (length(masslossSE)>1)
      plotCI(mat$massloss[select], mat$y[select], err="x", uiw=mat$massloss.errbar[select], liw=mat$massloss.errbar[select], add=TRUE, pch=pch[i], barcol=errcol, gap=0, type=type, col=col[i], lwd=lwd, lty=lty[i], , pt.bg=pt.bg[i], ...)

                                        #1-way anovas nach für einzelne littertypen

    sig<-vector(length=length(seplev))
    for (i in 1:length(seplev))
      {
        aov<-oneway.test(y[sepfac==seplev[i]] ~ xf[sepfac==seplev[i]])
        comp<-pairwise.t.test(y[sepfac==seplev[i]], xf[sepfac==seplev[i]], p.adjust="bonferroni", pool.sd=F, var.eq=F)
        trcomp<-transcomp(comp$p.value)
        tmp<-multcompLetters(trcomp, Letters=LETTERS)
        mat$xcomp[mat$sepfac==seplev[i]]<-tmp$Letters
        sig[i]<-siglev(aov$p.value)

      }

  }
  print(paste(seplev, "~ harvest" ,sig))

  if (legend!=F) {
    if (legsig==T) {leg <- paste(seplev, sig)} else {leg <- seplev}
                                        #sternchen in legende für signifikanzniveaus für 1.way anovas für einzelne littertypen
    legend(legend, leg, col=col[1:length(seplev)],
           lty=lty[1:length(seplev)], lwd=lwd[1:length(seplev)])
  }

  if (endsig == T) {
    select<-mat$xfac==max(xfac)
    text(rep(xlim[2], length(seplev)), mat$y[select],
         labels=sig,cex=text.cex)
  }

  if (allpoints==TRUE)
    for (j in 1:length(xlev))
      points (rep (mat$massloss[mat$sepfac==seplev[i] &
                                mat$xf==xlev[j]], length(y[xfac==xlev[j]&sepfac==seplev[i]])),
              y[xfac==xlev[j]&sepfac==seplev[i]], col=col[i],cex=.08,
              pch=pch[i], ...)


                                        #1-way anovas für einzelne zeitpunkte mit sternchen über dem zeitpunkt für signifikanzniveaus
  if (length(massloss)==1 & topsig==T)
    {
      sig<-vector(length=length(xlev))
      for (j in 1:length(xlev))
        {
          aov<-oneway.test(y[xf==xlev[j]] ~ sepfac[xf==xlev[j]])
          comp<-pairwise.t.test(y[xf==xlev[j]], sepfac[xf==xlev[j]], p.adjust="bonferroni", pool.sd=F, var.eq=F)


          trcomp<-transcomp(comp$p.value)
          tmp<-multcompLetters(trcomp)
                                        #         tmp<-multcompLetters(trcomp, Letters=letters)

          mat$sepcomp[mat$xf==xlev[j]]<-tmp$Letters
          sig[j] <- siglev(aov$p.value)
          text(xlev[j], ylim[2], sig[j], cex=text.cex, adj=c(0,1))
        }
    }

                                        #twoway<-anova(lm(mat ~ sepfac*xf))
                                        #text(6, lim*0.3, twoway[5])
                                        #twoway[5]

  if(letters[1]!=F & length(massloss)==1) {text(mat$massloss+(xmax-xmin)*letters[1], mat$y+(ymax-ymin)*letters[2], paste(mat$xcomp, mat$sepcomp), cex=text.cex)}
  if(letters[1]!=F & length(massloss)>1) {text(mat$massloss+(xmax-xmin)*letters[1], mat$y+(ymax-ymin)*letters[2], mat$xcomp, cex=text.cex)}


  print(paste(xlev, "~ type", sig))
  return(mat)
}

##########################
##plots for correlations##
##########################

corplot<-function(v1,v2,v1lab="", v2lab="", alpha=0.05, xlim=F, ylim=F, line=T, textpos="right", textcex=0.8, ...)
{
if (xlim[1]==F) {xlim <- c(min(v1, na.rm=T), max(v1, na.rm=T))}
if (ylim[1]==F) {ylim<-c(min(v2, na.rm=T), max(v2, na.rm=T)+(max(v2, na.rm=T)-min(v2, na.rm=T))*0.1)}
plot(v1, v2, ylim=ylim, xlim=xlim, ...)
cor<-cor.test(v1,v2)
if (cor$p.value < alpha & line==T) abline(lm(v2~v1))     
if (textpos =="left") {text(xlim[1], ylim[2], labels=paste("R =", formatC(cor$estimate, digits=3), siglev(cor$p.value)), cex=textcex, pos=4,adj=c(0,1))}
else if (textpos == "right") {text(xlim[2], ylim[2], labels=paste("R =", formatC(cor$estimate, digits=3), siglev(cor$p.value)), cex=textcex,pos=2,adj=c(1,1))
}

}
?text