library(psych)
library(corrplot)
library(Hmisc)
dummify <- function(D) { as.data.frame(lapply(data.frame(D),dummy.code)) }

cpplus <- function(V,reorder=TRUE,alpha=0.005,...) {

  i <- unlist(lapply(data.frame(V),is.numeric))
  if(sum(i)==0) { 
    catdat <- dummify(cbind(V)[,!i])
    alldat <- catdat
  } else {
    numdat <- cbind(V[,i])
      if(sum(i)==ncol(cbind(V))) { 
        alldat <- numdat
      } else {
        catdat <- dummify(cbind(V)[,!i])
        alldat <- data.frame(numdat,catdat)
      }
  }
  alldat <- as.data.frame(alldat)
  allcor <- cor(alldat,use="pairwise.complete.obs")
  cat("\n",sum(is.na(allcor[lower.tri(allcor)])),"missing correlations set to zero\n\n")
  flush.console()
  allcor[is.na(allcor)] <- 0
  diag(allcor) <- 1

  pmat <- rcorr(as.matrix(alldat),type="pearson")$P ; diag(pmat) <- 1
  pmat[is.na(pmat)] <- 1

  allcor2 <- ifelse(pmat>alpha,0,allcor)
  allcor2col <-  ifelse(pmat>alpha,adjustcolor("white"),"black")
  colpal <- colorRampPalette(c("burlywood","white","darkcyan"))

  if(reorder==FALSE) {
    corrplot(allcor2,method="color",tl.col="black",p.mat=pmat, sig.level=alpha,insig="pch",pch=4,pch.cex=0.6,pch.col="grey50",
      addCoefasPercent=F,addgrid.col="grey",col=colpal(100),addCoef.col=allcor2col)
  }
  if(reorder==TRUE) {
    horder <- corrplot(allcor2,method="color",col="white",order="hclust",hclust.method="ward.D")
    horder <- rownames(horder$corr)
    corrplot(allcor2,method="color",tl.col="black",p.mat=pmat, sig.level=alpha,insig="pch",pch=4,pch.cex=0.6,pch.col="grey50",
      addCoefasPercent=F,addgrid.col="grey",col=colpal(100),addCoef.col=allcor2col[horder,horder], order="hclust",hclust.method="ward.D")
  }
  invisible(alldat)
}