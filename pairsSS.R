#library(geneplotter)
library(RColorBrewer)

pairsSS <-function(df,...)
{
	n<-length(df[1,])
	yl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	xl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
				}
			}
		}
	}
}

#x<-rnorm(100);
#y<-rnorm(100);
#z<-rnorm(100);
#df<-data.frame(x,y,z);
#pairsSS(df);

pairsplain <-function(df,...)
{
	n<-length(df[1,])
	yl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	xl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					plot(df[,j],df[,i],pch='.',ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					abline(0,1,col='blue',lwd=.2)
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					plot(df[,j],df[,i],pch='.',ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					abline(0,1,col='blue',lwd=.2)
					if(j==n)
					{
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					plot(df[,j],df[,i],pch='.',ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					abline(0,1,col='blue',lwd=.2)
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					plot(df[,j],df[,i],pch='.',ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					abline(0,1,col='blue',lwd=.2)
				}
			}
		}
	}
}

#x<-rnorm(100);
#y<-rnorm(100);
#z<-rnorm(100);
#df<-data.frame(x,y,z);
#pairsplain(df);

pairsSSpoints <-function(df,pts,...)
{
	df[df==-Inf]<-NA
	n<-length(df[1,])
	yl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	xl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					points(pts[,j],pts[,i],pch=20,col='red');
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					points(pts[,j],pts[,i],pch=20,col='red');
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					points(pts[,j],pts[,i],pch=20,col='red');
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					points(pts[,j],pts[,i],pch=20,col='red');
				}
			}
		}
	}
}

#x<-rnorm(100);
#y<-rnorm(100);
#z<-rnorm(100);
#df<-data.frame(x,y,z);
#pts<-df[x<.5,]
#pairsSSpoints(df,pts);


pairsSSna <-function(df,...)
{
	df[df==-Inf]<-NA
	n<-length(df[1,])
	yl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	xl<-c(quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[2],quantile(unlist(df),seq(from=0,to=1,by=.001),na.rm=T)[1000]);
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F);
				}
			}
		}
	}
}

pairsSSna_lim <-function(df,xl,yl,...)
{
	df[df==-Inf]<-NA
	n<-length(df[1,])
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,...);
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,...);
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,...);
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,...);
				}
			}
		}
	}
}

pairsSSna_lim_nopoints <-function(df,xl,yl,...)
{
	df[df==-Inf]<-NA
	n<-length(df[1,])
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
				}
			}
		}
	}
}

#

#cuffna.iv<-is.na(cuff_rearrange[,i])
#countna.iv<-is.na(count_rearrange[,i])
#plot(log2(count_rearrange[,i]),log2(cuff_rearrange[,i]),ylim=c(-20,25),xlim=c(-1,24))
#points(log2(count_rearrange[cuffna.iv,i]),rep(0,times=length(which(cuffna.iv))),col='blue')
#points(rep(0,times=length(which(countna.iv))),log2(cuff_rearrange[countna.iv,i]),col='red')

pairsSSna_lim_nopoints_showempties <-function(df,xl,yl,...)
{
	df[df==-Inf]<-NA
	n<-length(df[1,])
	par(mfrow=c(n,n),oma=c(4,4,4,4),mar=c(0,0,0,0),bty='o')
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == 1)
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					missing1.iv<-is.na(df[,i])
					missing2.iv<-is.na(df[,j])
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					points(df[missing1.iv,j],rep(0,times=length(which(missing1.iv))),col='red',pch='.')
					points(rep(0,times=length(which(missing2.iv))),df[missing2.iv,i],col='red',pch='.')
					mtext(colnames(df)[j],line=1,side=3,...)
					mtext(colnames(df)[i],line=1,side=2,...)
				}
				else
				{
					#i is 1, j is 2/3.
					par(mar=c(0,0,0,0));
					missing1.iv<-is.na(df[,i])
					missing2.iv<-is.na(df[,j])
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					points(df[missing1.iv,j],rep(0,times=length(which(missing1.iv))),col='red',pch='.')
					points(rep(0,times=length(which(missing2.iv))),df[missing2.iv,i],col='red',pch='.')
					if(j==n)
					{
						#axis(4,at=seq(from=floor(yl[1]),to=ceiling(yl[2]),by=(ceiling(yl[2])-floor(yl[1]))/5))
						axis(4,at=pretty(yl))
					}
					mtext(colnames(df)[j],line=1,side=3,...)
				}
			}
			else
			{
				if(j==1)
				{
					par(mar=c(0,0,0,0));
					missing1.iv<-is.na(df[,i])
					missing2.iv<-is.na(df[,j])
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					points(df[missing1.iv,j],rep(0,times=length(which(missing1.iv))),col='red',pch='.')
					points(rep(0,times=length(which(missing2.iv))),df[missing2.iv,i],col='red',pch='.')
					mtext(colnames(df)[i],line=1,side=2,...)
					if(i==n)
					{
						#axis(1,at=seq(from=floor(xl[1]),to=ceiling(xl[2]),by=(ceiling(xl[2])-floor(yl[1]))/5))
						axis(1,at=pretty(xl))
					}
				}
				else
				{
					par(mar=c(0,0,0,0));
					missing1.iv<-is.na(df[,i])
					missing2.iv<-is.na(df[,j])
					smoothScatter(df[,j],df[,i],pch='.',colramp=colorRampPalette(brewer.pal(7,"YlGnBu")),nbin=100,ylim=yl,xlim=xl,xaxt='n',yaxt='n',ann=F,nrpoints=0);
					points(df[missing1.iv,j],rep(0,times=length(which(missing1.iv))),col='red',pch='.')
					points(rep(0,times=length(which(missing2.iv))),df[missing2.iv,i],col='red',pch='.')
				}
			}
		}
	}
}

