#'
#'
#'

disagree<-function(data,m=2,pow=1,full=FALSE)
{
deem=dim(data)
if(m>deem[2]) stop('Not enough raters.\n')
dout=matrix(NA,nrow=deem[1]*(deem[1]-1)/2,ncol=3)
i=1
for (a in 1:(m-1))
{
  for (b in (a+1):m)
  {
    dout[i,]=c(a,b,1-mean(abs(data[,a]-data[,b])^pow))
  }
}
if(full) return(dout)
return(mean(dout[,3]))
}
