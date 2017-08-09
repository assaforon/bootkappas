#'
#'
#'

dissim2<-function(datmat,pow=1,full=FALSE)
{
deem=dim(datmat)
dout=datmat.frame(t(combn(deem[2],2)))
names(dout)=c('rater1','rater2')
dout$dis=NA
for (a in 1:dim(dout)[1]) dout$dis[a]=mean(abs(datmat[,dout$rater2[a]]-datmat[,dout$rater1[a]])^pow)

if(full) return(dout)
return(mean(dout$dis))
}

###

zero1<-function(datmat,m=2,full=FALSE)
{
  deem=dim(datmat)
  if(m>deem[2]) stop ("Not enough raters.\n")
  tuples=combn(deem[2],m)
  dout=datmat.frame(tuple=apply(tuples,2,paste,collapse=','))
  dout$dis=NA
  for (a in 1:dim(dout)[1]) dout$dis[a]=mean(apply(datmat[,tuples[,a]],1,var)>0)
  if(full) return(dout)
  return(mean(dout$dis))
}

###

dissenters<-function(datmat)
{
dim(datmat)[2]-mean(apply(datmat,1,function(x) max(table(x))))
}

