# Draw heatmaps
library(RColorBrewer)

#mouse data
#download from http://plugins.biogps.org/download/geneatlas_MOE430_20090327.raw.avg.csv.zip and unzip.
exp <- read.csv("H:/introR/data/mouse_ge/geneatlas_MOE430.csv",as.is=T)
names <- read.table("H:/introR/data/mouse_ge/moe4302.txt",as.is=T,header=T,sep="\t")

#My own image plot function, give it the data matrix, colors, title, fixed limits, add a white grid or not, adjust text sizes...
mcm_image<-function(data,colors,title="",zlim=c(floor(min(data,na.rm=T)),ceiling(max(data,na.rm=T))),addgrid=F,cexCol=.8,cexRow=.1,...)
{
	library(RColorBrewer)
	par(mar=c(7,2,4,7))
	image(t(data[nrow(data):1,]),axes=F,col=colors,zlim=zlim)

	#adding the grid.
	if(addgrid)
	{
		abline(v=seq(-1*seq(0,1,length.out=ncol(data))[2]/2,1+seq(0,1,length.out=ncol(data))[2]/2,length.out=ncol(data)+1),lwd=.1,col='white')
		abline(h=seq(-1*seq(0,1,length.out=nrow(data))[2]/2,1+seq(0,1,length.out=nrow(data))[2]/2,length.out=nrow(data)+1),lwd=.1,col='white')
	}
	axis(1, at=seq(0,1,length.out=ncol(data)),labels=colnames(data),tick=F,las=2,cex.axis=cexCol,line=-.5)
	axis(4, at=seq(0,1,length.out=nrow(data)), labels=rev(rownames(data)), tick=F,las=2,cex.axis=cexRow,line=-.5)
	mtext(title,side=3,outer=F,cex=1,line=1)

	colorBar <- function(n,col.grad,zlim){
		ColorLevels <- seq(zlim[1],zlim[2],length.out=length(col.grad))
		labels <- seq(zlim[1],zlim[2],length.out=6)
		par(mar=c(1,4,4,5))
		image(t(matrix(ColorLevels,ncol=1)),axes=F,col=col.grad)
		axis(4,at=seq(0,1,length.out=6),tick=F,labels=round(labels,digits=0),cex.axis=.9,las=2,line=-.5)
		mtext("Scale",side=3,outer=F,cex=.6,line=.4)
	}
	colorBar(data,colors,zlim);

}

selected <- exp[,c("MEF", "adipose_brown","adipose_white", "amygdala", "bladder", "bone",
"bone_marrow", "cerebellum", "cerebral_cortex", "cornea", "heart", "iris", "kidney", "lens", "liver", "lung", "olfactory_bulb", "ovary", "pancreas", "pituitary", "placenta", "prostate", "retina", "salivary_gland", "skeletal_muscle", "spinal_cord", "spleen", "stomach", "testis", "umbilical_cord", "uterus")]

#modify the looooong gene names that are like:
#> names[49,]
#   Probe.Set.ID             Gene.Symbol
#49   1415718_at LOC100044471 /// Sap30l
#
#to be like 1415718_at (First symbol ...)

names[,2]<-gsub(pattern=" /// .*",replacement=" ...",names[,2])

#setting the rownames to be probeset_id (gene_name)
rownames(selected)<-paste(exp[,1],"(",names[match(names[,1],exp[,1]),2],")")

highest20.iv <- order(-rowMeans(selected))[1:20]

h<-heatmap(as.matrix(selected[highest20.iv,]))
nf<-layout(matrix(data=c(1,1,1,2,0,0),nrow=3,ncol=2),widths=c(13,3),TRUE)
layout.show(nf)
mcm_image(data=as.matrix(selected[highest500.iv,])[h$rowInd,h$colInd],colors=brewer.pal(9,"Blues"),title="Gene expression in Mouse Tissues",zlim=c(0,130000),addgrid=F)


#try with another data set...
weather <- read.csv("H:/introR/data/weather/us_weather.csv",as.is=T,strip.white=T)
fweather <- weather
fweather[,7:18] <- weather[,7:18] * 9/5 + 32

boston.iv <- fweather[,1]=="BOSTON"
boston <- fweather[boston.iv,]
rownames(boston)<-boston$Period

nf<-layout(matrix(data=c(1,1,1,2,0,0),nrow=3,ncol=2),widths=c(13,3),TRUE)
layout.show(nf)
mcm_image(data=boston[,7:18],colors=colorRampPalette(rev(brewer.pal(9,"RdBu")))(100),zlim=c(0,80),addgrid=F)

nf<-layout(matrix(data=c(1,1,1,2,0,0),nrow=3,ncol=2),widths=c(13,3),TRUE)
mcm_image(data=head(boston[,7:18],n=10),colors=colorRampPalette(rev(brewer.pal(9,"RdBu")))(100),zlim=c(10,80),addgrid=F,cexRow=.8)

x11()
nf<-layout(matrix(data=c(1,1,1,2,0,0),nrow=3,ncol=2),widths=c(13,3),TRUE)
mcm_image(data=tail(boston[,7:18],n=10),colors=colorRampPalette(rev(brewer.pal(9,"RdBu")))(100),zlim=c(10,80),addgrid=F,cexRow=.8)

