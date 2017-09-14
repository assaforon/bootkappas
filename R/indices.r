#'
#'
#'

dissim2<-function(datmat,pow=2,full=FALSE,...)
{
deem=dim(datmat)
cat(deem,'\n')
dout=data.frame(t(combn(deem[2],2)))
names(dout)=c('rater1','rater2')
dout$dis=NA
for (a in 1:dim(dout)[1]) dout$dis[a]=mean(abs(datmat[,dout$rater2[a]]-datmat[,dout$rater1[a]])^pow,...)

if(full) return(dout)
return(mean(dout$dis,...))
}

###

zero1<-function(datmat,m=2,full=FALSE,...)
{
  deem=dim(datmat)
  if(m>deem[2]) stop ("Not enough raters.\n")
  tuples=combn(deem[2],m)
  dout=data.frame(tuple=apply(tuples,2,paste,collapse=','))
  dout$dis=NA
  for (a in 1:dim(dout)[1]) 
  {
    v=apply(datmat[,tuples[,a]],1,var,...)
    dout$dis[a]=sum(is.finite(v) & v>0)/sum(is.finite(v))
  }
  if(full) return(dout)
  return(mean(dout$dis,...))
}

###

dissenters<-function(datmat,...)
{
counts=rowSums(is.finite(datmat))
mean(counts[counts>0]-apply(datmat[counts>0,],1,function(x) max(table(x))),na.rm=TRUE)
}

