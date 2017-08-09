#'
#'
#'

#
#
genperms<-function(orig,B,strata=NULL,boots=TRUE)
{
  if(!is.null(strata))
  {
    if(length(strata)!=length(orig)) stop("Length mismatch.\\n")
    resort=unlist(lapply(split(orig,strata),identity))
    return(replicate(B,sapply(split(orig,strata),sample,replace=boots)[match(orig,resort)]))
  }
  return(replicate(B,sample(orig,replace=boots)))
}