library(VennDiagram)
library(RColorBrewer)

#wrapper functions for VennDiagram functions.

myvenn2<-function(mylist,filename,mn,cols=c("red","dodgerblue"),lwd=2,alpha=.75,labelcol="black",fontfamily="sans",cat.dist=c(.04,.04),cat.pos=c(-21,15),main.pos=c(.5,.9),ext.text=F)
{
	venn.diagram(
		x=mylist,
		filename = filename,
		lwd = lwd,
		cat.dist=cat.dist,
		cat.pos=cat.pos,
		fill = cols,
		alpha = alpha,
		label.col=labelcol,
		fontfamily=fontfamily,
		cat.fontfamily=fontfamily,
		cat.col = cols,
		main=mn,
		main.pos=main.pos,
		main.fontfamily=fontfamily,
		ext.text=ext.text
		);
	sectionlist <- venntable(mylist)
	return(sectionlist);
}


myvenn3<-function(mylist,filename,mn,cols=brewer.pal(3,"Set1"))
{
	lwd<-2
	alpha<-.5
	labelcol<-"white"
	fontfamily="sans"
	venn.diagram(
		x=mylist,
		filename = filename,
		lwd = lwd,
		fill = cols,
		alpha = alpha,
		label.col=labelcol,
		fontfamily=fontfamily,
		cat.fontfamily=fontfamily,
		cat.col = cols,
		main=mn,
		main.pos=c(.5,1.05),
		main.fontfamily=fontfamily,
		);
	sectionlist <- venntable(mylist)
	return(sectionlist);
}

myvenn4<-function(mylist,filename,mn,cols=brewer.pal(4,"Set1"))
{
	lwd<-2
	alpha<-.6
	labelcol<-"black"
	fontfamily="sans"
	venn.diagram(
		x=mylist,
		filename = filename,
		lwd = lwd,
		fill = cols,
		alpha = alpha,
		label.col=labelcol,
		fontfamily=fontfamily,
		cat.fontfamily=fontfamily,
		cat.col = cols,
		main=mn,
		main.pos=c(.5,1.05),
		main.fontfamily=fontfamily,
		);
	templist<-list(mylist[[1]],mylist[[3]],mylist[[4]],mylist[[2]])
	names(templist)<-names(list)[c(1,3,4,2)]
	sectionlist <- venntable(templist)
	return(sectionlist);
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
