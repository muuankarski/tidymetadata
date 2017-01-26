strip_attributes <- function(data){
  d <- data
  for (i in 1:ncol(d)) {
    z<-class(d[[i]])
    if (z[[1]]=='labelled'){
      class(d[[i]]) <- z[-1]
      attr(d[[i]],'labels')<-NULL
    }
    attr(d[[i]],'names')<-NULL
    attr(d[[i]],'label')<-NULL
    attr(d[[i]],'format.stata')<-NULL
    attr(d[[i]],'format.spss')<-NULL
  }
  return(d)
}


