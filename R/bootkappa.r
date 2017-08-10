


bootkappa<-function(data,B,varnames,rater='rater',id='study_id',boots=TRUE,disfun=dissim2,...)
{
require(reshape2)

prep=melt(data[,c(id,rater,varnames)],id.var=c(id,rater))
truemat=acast(formula=paste(id,'+variable~',rater),data=prep)
#return(truemat)
observed=disfun(truemat,...)

perms=genperms(data[,id],strata=data[,rater],B=B,boots=boots)

return(observed)


# 
# deem=dim(datmat)
#   
# bootraw=rep(NA,B)
# for (a in 1:B)
# {
#     bootmat=apply(datmat,2,sample,replace=boots) 
#     #  print(bootmat)
#     bootraw[a]=disfun(bootmat,...)
# }
# list(true=observed,boot=1-observed/bootraw)  
}

# la=FEES[-c(20,23),-3] %>% melt(id.var=1:2) %>% dcast (formula=subject+variable~swallow)
# names(la)[-1]=c('rater','s1','s4')

