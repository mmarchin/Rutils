showcolors <- function(col)
{
  image(z=matrix(1:length(col), ncol=1), col=col, xaxt="n", yaxt="n" )
}

colortohex <- function(colorlist)
{
	hex<-vector()
	for(i in 1:length(colorlist))
	{
		c <- col2rgb(colorlist[i])
		temp<-sprintf("#%02X%02X%02X", c[1], c[2], c[3])
		hex<-c(hex,temp);
	}
	return(hex);
}

