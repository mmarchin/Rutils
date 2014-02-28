# Draw heatmaps with row side colors for different sets.
library(RColorBrewer)

mcm_image<-function(data,colors,title="",zlim=c(floor(min(data,na.rm=T)),ceiling(max(data,na.rm=T))),RowSideLengths=rowsidelengths,RowSideLabels=rowsidelabels,RowSideValues=rowsidevalues,RowSideColors=rowsidecolors,addgrid=F,cexCol=.8,cexRow=.1,lab=6,colAxis=T,rowAxis=T,mymar=c(7,2,4,7),colmar=c(1,4,4,5),addvert=F,vert=NA,vertcol="gray",...)
{
	library(RColorBrewer)
	par(mar=mymar)
	image(t(matrix(rev(RowSideValues),ncol=1)),axes=F,col=RowSideColors)
	axis(4,at=cumsum(rev(RowSideLengths))/sum(RowSideLengths),tick=F,labels=rev(RowSideLabels),cex.axis=.9,las=2,line=-.5)
	image(t(data[nrow(data):1,]),axes=F,col=colors,zlim=zlim)

	#adding the grid.
	if(addgrid)
	{
		abline(v=seq(-1*seq(0,1,length.out=ncol(data))[2]/2,1+seq(0,1,length.out=ncol(data))[2]/2,length.out=ncol(data)+1),lwd=.1,col='white')
		abline(h=seq(-1*seq(0,1,length.out=nrow(data))[2]/2,1+seq(0,1,length.out=nrow(data))[2]/2,length.out=nrow(data)+1),lwd=.1,col='white')
	}
	if(addvert)
	{
		abline(v=vert[1],col=vertcol)
		abline(v=vert[2],col=vertcol)
	}
	if(colAxis)
	{
		axis(1, at=seq(0,1,length.out=ncol(data)),labels=colnames(data),tick=F,las=2,cex.axis=cexCol,line=-.5)
	}
	if(rowAxis)
	{
		axis(4, at=seq(0,1,length.out=nrow(data)), labels=rev(rownames(data)), tick=F,las=2,cex.axis=cexRow,line=-.5)
	}
	mtext(title,side=3,outer=F,cex=1,line=1)

	colorBar <- function(n,col.grad,zlim,lab=3,colmar){
		ColorLevels <- seq(zlim[1],zlim[2],length.out=length(col.grad))
		labels <- seq(zlim[1],zlim[2],length.out=lab)
		par(mar=colmar)
		image(t(matrix(ColorLevels,ncol=1)),axes=F,col=col.grad)
		axis(4,at=seq(0,1,length.out=lab),tick=F,labels=round(labels,digits=0),cex.axis=.9,las=2,line=-.5)
		mtext("Scale",side=3,outer=F,cex=.6,line=.4)
	}
	colorBar(data,colors,zlim,lab,colmar);
}
