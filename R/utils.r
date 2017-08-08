#'
#'
#'

genperms<-function(n,B,strata=NULL,boots=TRUE)
{
if(!is.null(strata))
{
    if(length(strata)!=n) stop("Length mismatch.\n")
    resort=unlist(lapply(split(1:n,strata),identity))
    return(replicate(B,sapply(split(1:n,strata),sample,replace=boots)[match(1:n,resort)]))
}
return(replicate(B,sample(n,replace=boots)))
}
