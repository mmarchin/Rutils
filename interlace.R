interlace<-function(a1,a2)
{
	new<-vector();
	if(length(a1) == length(a2))
	{
		oddnumbers<-seq(from=1,to=length(a1)*2,by=2);
		for(i in seq(from=1,to=length(a1),by=1))
		{
			j<-oddnumbers[i]
			new[j]<-a1[i]
			new[j+1]<-a2[i]
		}
		return(new);
	}
	else
	{
		return(NA);
	}
}

#x<-c('a','b','c')
#y<-c('z','y','x')
#interlace(x,y)
##[1] "a" "z" "b" "y" "c" "x"

