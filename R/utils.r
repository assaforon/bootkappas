#'

#
#
genperms<-function(orig,B,strata=NULL,boots=TRUE)
{
  if(!is.null(strata))
  {
    if(length(strata)!=length(orig)) stop("Length mismatch.\\n")
    orig=paste(strata,orig) ### appends strata ID to individual ID to avoid ID aliasing
    resort=unlist(lapply(split(orig,strata),identity)) ### rearranges obs in blocks by strata
    return(replicate(B,sapply(split(orig,strata),sample,replace=boots)[match(orig,resort)]))
  }
  return(replicate(B,sample(orig,replace=boots)))
}