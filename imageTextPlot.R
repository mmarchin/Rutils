#Custom heatmap functions
imageTextPlot <- function(x,col.grad,mn){
  par(mar=c(1,7,9,1))
  image(t(x[nrow(x):1,]),axes=F,col=col.grad)

  x.row <- seq(from=0, to=1, length=ncol(x)) 
  y.col <- seq(from=1, to=0, length=nrow(x))

  lbl <- round(x,digits=2)

  for( i in 1:length(y.col) ) 
  { 
     text(x.row,y.col[i],labels=as.character(lbl[i,])) 
  }

  axis(3, at=seq(0,1,length.out=ncol(x)),
        labels=colnames(x), tick=F,las=2,cex.axis=.8)
  axis(2, at=seq(0,1,length.out=nrow(x)),
        labels=rownames(x[nrow(x):1,]), tick=F,las=2,cex.axis=.8)
}

colorBar <- function(n,col.grad){
  ColorLevels <- seq(floor(min(n)), ceiling(max(n)), length.out=length(col.grad))
  par(mar=c(1,1,1,1),oma=c(1,1,1,1))
  image(matrix(ColorLevels,ncol=1),axes=F,col=col.grad)
  axis(1,at=seq(0,1,length.out=length(col.grad)),labels=round(ColorLevels,digits=1),cex.axis=.7)
  mtext(mn,side=3,outer=TRUE)
}

#nf <- layout(matrix(c(1,2),nrow=2,ncol=1), heights=c(5,1), TRUE)
#layout.show(nf)

#new.colors <- colorRampPalette(brewer.pal(10, "RdBu"))(20)

#x<-as.matrix(s3.rearrange[nona.iv,c(3:4,6:13)])
#CustomHeatmap(x,new.colors)
#colorBar(x,new.colors)
