# make a venn diagram

library(limma)
#this one takes two sets and the names of the sets.
Venn2<-function(set1,set2,names)
{
	stopifnot(length(names) == 2)
	#Form universe as union of sets
	universe<-sort(unique(c(set1,set2)))
	Counts<-matrix(0,nrow=length(universe),ncol=2)
	colnames(Counts)<-names
	for(i in 1:length(universe))
	{
		Counts[i,1]<- universe[i] %in% set1
		Counts[i,2]<- universe[i] %in% set2
	}
	vennDiagram(vennCounts(Counts));
	tab<-data.frame(universe,Counts,stringsAsFactors=FALSE)
	colnames(tab)<-c("genes",names);
	return(tab);
}

#same as above, except with a title.
Venn2t<-function(set1,set2,names,title)
{
	stopifnot(length(names) == 2)
	#Form universe as union of sets
	universe<-sort(unique(c(set1,set2)))
	Counts<-matrix(0,nrow=length(universe),ncol=2)
	colnames(Counts)<-names
	for(i in 1:length(universe))
	{
		Counts[i,1]<- universe[i] %in% set1
		Counts[i,2]<- universe[i] %in% set2
	}
	par(mar=c(1,1,2,1),oma=c(1,1,2,1))
	vennDiagram(vennCounts(Counts));
	mtext(title,outer=T,line=1);
	tab<-data.frame(universe,Counts,stringsAsFactors=FALSE)
	colnames(tab)<-c("genes",names);
	return(tab);
}

#this one takes 3 sets
require(limma)
Venn3 <- function(set1, set2, set3, names)
{
	stopifnot( length(names) == 3)
	# Form universe as union of all three sets
	universe <- sort( unique( c(set1, set2, set3) ) )
	Counts <- matrix(0, nrow=length(universe), ncol=3)
	colnames(Counts) <- names

	for (i in 1:length(universe))
	{
		Counts[i,1] <- universe[i] %in% set1
		Counts[i,2] <- universe[i] %in% set2
		Counts[i,3] <- universe[i] %in% set3
	}
	par(mar=c(1,1,2,1),oma=c(1,1,2,1))
	vennDiagram(vennCounts(Counts));
	tab<-data.frame(universe,Counts,stringsAsFactors=FALSE)
	colnames(tab)<-c("genes",names);
	return(tab);
}

#3 sets, with a title.
Venn3t <- function(set1, set2, set3, names, title)
{
	stopifnot( length(names) == 3)
	# Form universe as union of all three sets
	universe <- sort( unique( c(set1, set2, set3) ) )
	Counts <- matrix(0, nrow=length(universe), ncol=3)
	colnames(Counts) <- names

	for (i in 1:length(universe))
	{
		Counts[i,1] <- universe[i] %in% set1
		Counts[i,2] <- universe[i] %in% set2
		Counts[i,3] <- universe[i] %in% set3
	}

	par(mar=c(1,1,2,1),oma=c(1,1,2,1))
	vennDiagram( vennCounts(Counts) )
	mtext(title,outer=T,line=1);
	tab<-data.frame(universe,Counts,stringsAsFactors=FALSE)
	colnames(tab)<-c("genes",names);
	return(tab);
}


#this one is empty, just give it names a,b,c and values a b c ab ac bc abc
circle <- function(x, y, r, ...)
{
	ang <- seq(0, 2*pi, length = 100)
	xx <- x + r * cos(ang)
	yy <- y + r * sin(ang)
	polygon(xx, yy, ...)
}

fillvenn3<-function(names,values)
{
	par(mar=c(3, 3, 3, 3))
	plot(-12,-12, ylim=c(0, 9), xlim=c(0, 9),axes=FALSE,type='n')
	circle(x=3, y=6, r=3, col=rgb(1,0,0,.5))
	circle(x=6, y=6, r=3, col=rgb(0,.5,.1,.5))
	circle(x=4.5, y=3, r=3, col=rgb(0,0,1,.5))
	text(0.2,7.8,names[1], cex=1.2, col="black", srt=55)
	text(8.7,7.8,names[2], cex=1.2, col="black", srt=-55)
	text(4.5,-0.2,names[3], cex=1.2, col="black", srt=0)

	text(1.6,6.5,values[1], cex=2, col="black", srt=0) #1
	text(7.3,6.5,values[2], cex=2, col="black", srt=0) #2
	text(4.5,1.8,values[3], cex=2, col="black", srt=0) #3

	text(4.5,7.0,values[4], cex=2, col="black", srt=0) #12
	text(2.7,4.0,values[5], cex=2, col="black", srt=0) #13
	text(6.2,4.0,values[6], cex=2, col="black", srt=0) #23

	text(4.5,5.0,values[7], cex=2, col="black", srt=0) #123
}

fillvenn2<-function(names,values)
{
	par(mar=c(3, 3, 0, 0))
	plot(-10,-10, ylim=c(0, 9), xlim=c(0, 9),axes=FALSE,type='n')
	circle(x=3, y=6, r=3, col=rgb(1,0,0,.5))
	circle(x=6, y=6, r=3, col=rgb(0,.5,.1,.5))
	text(0.2,7.8,names[1], cex=1.5, col="black", srt=55)
	text(8.7,7.8,names[2], cex=1.5, col="black", srt=-55)

	text(1.6,6,values[1], cex=2, col="black", srt=0) #1
	text(7.3,6,values[2], cex=2, col="black", srt=0) #2

	text(4.5,6,values[3], cex=2, col="black", srt=0) #12
}


fillvenn3calc<-function(names,values) #where values = A,B,C,AB,AC,BC,ABC total (not calculated yet)
{
	ABC<-values[7]
	AB<-values[4] - ABC
	AC<-values[5] - ABC
	BC<-values[6] - ABC
	A<-values[1] - AB - AC - ABC
	B<-values[2] - AB - BC - ABC
	C<-values[3] - AC - BC - ABC
	val2<-c(A,B,C,AB,AC,BC,ABC);
	fillvenn3(names,val2);
}

fillvenn2calc<-function(names,values) #where values = A,B,AB total
{
	AB<-values[3]
	A<-values[1] - AB
	B<-values[2] - AB
	fillvenn2(names,c(A,B,AB))
}

fillvenn2<-function(names,values)
{
	par(mar=c(3, 3, 0, 0))
	plot(-10,-10, ylim=c(0, 9), xlim=c(0, 9),axes=FALSE,type='n')
	circle(x=3, y=6, r=3, col=rgb(1,0,0,.5))
	circle(x=6, y=6, r=3, col=rgb(0,.5,.1,.5))
	text(0.2,7.8,names[1], cex=1.5, col="black", srt=55)
	text(8.7,7.8,names[2], cex=1.5, col="black", srt=-55)

	text(1.6,6,values[1], cex=2, col="black", srt=0) #1
	text(7.3,6,values[2], cex=2, col="black", srt=0) #2

	text(4.5,6,values[3], cex=2, col="black", srt=0) #12
}


venny<-function(set1,set2,set3,set4)
{
	# Form universe as union of all four sets
	universe <- sort( unique( c(set1, set2, set3, set4) ) )
	Counts <- matrix(0, nrow=length(universe), ncol=4)

	a<- c(rep(0,times=8),rep(1,times=8))
	b<-c(rep(0,times=4),rep(1,times=4),rep(0,times=4),rep(1,times=4))
	c<-rep(c(0,0,1,1),times=4)
	d<-rep(c(0,1),times=8)

	section_list <- vector("list", 16)
	sections<-paste(a,b,c,d,sep='')

	names(section_list)<-paste("c",sections,sep='')

	s<-vector(length=16)

	for (i in 1:length(universe))
	{
		Counts[i,1] <- universe[i] %in% set1
		Counts[i,2] <- universe[i] %in% set2
		Counts[i,3] <- universe[i] %in% set3
		Counts[i,4] <- universe[i] %in% set4
		
		num<-match(paste(Counts[i,],collapse=''),sections)
		s[num]<-s[num]+1;
		section_list[[paste("c",paste(Counts[i,],collapse=''),sep='')]]<-c(section_list[[paste("c",paste(Counts[i,],collapse=''),sep='')]],universe[i])
	}

	return(section_list);
}

venny3<-function(set1,set2,set3)
{
	# Form universe as union of all four sets
	universe <- sort( unique( c(set1, set2, set3) ) )
	Counts <- matrix(0, nrow=length(universe), ncol=3)

	a<- c(rep(0,times=4),rep(1,times=4))
	b<-c(rep(0,times=2),rep(1,times=2),rep(0,times=2),rep(1,times=2))
	c<-rep(c(0,1),times=4)

	section_list <- vector("list", 8)
	sections<-paste(a,b,c,sep='')

	names(section_list)<-paste("c",sections,sep='')

	s<-vector(length=8)

	for (i in 1:length(universe))
	{
		Counts[i,1] <- universe[i] %in% set1
		Counts[i,2] <- universe[i] %in% set2
		Counts[i,3] <- universe[i] %in% set3
		
		num<-match(paste(Counts[i,],collapse=''),sections)
		s[num]<-s[num]+1;
		section_list[[paste("c",paste(Counts[i,],collapse=''),sep='')]]<-c(section_list[[paste("c",paste(Counts[i,],collapse=''),sep='')]],universe[i])
	}

	return(section_list);
}


venntable<-function(mylist)
{
	# Form universe as union of all sets
	universe<-unique(sort(unlist(mylist)))
	Counts <- matrix(0, nrow=length(universe), ncol=length(mylist))
	rownames(Counts)<-universe
	colnames(Counts) <- names(mylist)

	for (i in 1:length(universe))
	{
		for(j in 1:length(mylist))
		{
			Counts[i,j] <- universe[i] %in% mylist[[j]]
		}
	}
	tab<-data.frame(universe,Counts,stringsAsFactors=FALSE)
	return(tab);
}

