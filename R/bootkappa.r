


bootkappa<-function(data,B,varnames,rater='rater',id='study_id',boots=TRUE,disfun=dissim2,...)
{
require(reshape2)

prep=melt(data[,c(id,rater,varnames)],id.var=c(id,rater))
truemat=acast(formula=paste(id,'+variable~',rater),data=prep,fun.aggregate=mean)
#return(truemat)
observed=disfun(truemat,...)

perms=genperms(data[,id],strata=data[,rater],B=B,boots=boots)

nulls=rep(NA,B)
for (a in 1:B)
{
  permat=acast(formula=paste(id,'+variable~',rater),data=prep[match(perms[,a],paste(data[,rater],data[,id])),],fun.aggregate=mean)
  print(permat)
  nulls[a]=disfun(permat,...)
}
return(list(true=observed,ref0=nulls,truemat=truemat,perms=perms))


# list(true=observed,boot=1-observed/bootraw)  
}

# example using multiagree dataset
# data(FEES)
# la=multiagree::FEES[-c(20,23),-3] %>% melt(id.var=1:2) %>% dcast (formula=subject+variable~swallow)
# names(la)[-1]=c('rater','s1','s4')

