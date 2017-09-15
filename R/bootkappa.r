


bootkappa<-function(data,B,varnames,rater='rater',id='study_id',boots=TRUE,disfun=dissim2,...)
{
require(reshape2)

prep=melt(data[,c(id,rater,varnames)],id.var=c(id,rater))
truemat=acast(formula=paste(id,'+variable~',rater),data=prep,fun.aggregate=mean)
#return(list(prep,truemat))
observed=disfun(truemat,...)

stratID=paste(prep$rater,prep$variable)
perms=genperms(prep[,id],strata=stratID,B=B,boots=boots)

nulls=rep(NA,B)
for (a in 1:B)
{
  mixprep=cbind(prep[,id],prep[match(perms[,a],paste(stratID,prep[,id])),-1])
  names(mixprep)[1]=id
  permat=acast(formula=paste(id,'+variable~',rater),data=mixprep,fun.aggregate=mean)
#return(list(perms,permat,stratID)) 
#  cat(dim(permat))
  nulls[a]=disfun(permat,...)
}
return(list(true=observed,ref0=nulls,truemat=truemat,perms=perms))


# list(true=observed,boot=1-observed/bootraw)  
}

# example using multiagree dataset
# data(FEES)
# la=FEES[-c(20,23),-3] %>% melt(id.var=1:2) %>% dcast (formula=subject+variable~swallow)
# names(la)[-1]=c('rater','s1','s4')
# ha=bootkappa(la,B=10,id='subject',varnames=c('s1','s4'),na.rm=TRUE)

