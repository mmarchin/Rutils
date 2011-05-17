
library(RColorBrewer)

myheatmap<-function(data,columns,colors)
{
	par(mar=c(5,5,5,10))
	image(t(data[nrow(data):1,columns]),axes=F,col=new.colors)
	axis(1, at=seq(0,1,length.out=ncol(data)),labels=colnames(data), tick=F,las=2)
	axis(4, at=seq(0,1,length.out=nrow(data)), labels=data[nrow(data):1,1], tick=F,las=2,cex.axis=.5)

	x11()
	colorBar <- function(n,col.grad){
	  ColorLevels <- seq(floor(min(n)), ceiling(max(n)), length.out=length(col.grad))
	  labels <- seq(floor(min(n)), ceiling(max(n)), length.out=5)
	  par(mar=c(3,1,1,1))
	  image(matrix(ColorLevels,ncol=1),axes=F,col=col.grad)
	  axis(1,at=seq(0,1,length.out=5),tick=F,labels=round(labels,digits=0),cex.axis=.8)
	}
	colorBar(data[,columns],colors);

}
