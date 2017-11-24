#' @title Strip attributes from labelled data.frame using as.vector() function
#' @description Strip attributes from labelled data.frame using as.vector() function
#' @param data Labelled data.frame
#' @author Markus Kainu <markuskainu@gmail.com>
#' @return data.frame
#' @examples
#'  \dontrun{
#'  strip_attributes(data = spss_data)
#'  }
#'
#' @rdname strip_attributes
#' @import dplyr labelled
#' @export

strip_attributes <- function(data){
  library(dplyr)
  library(labelled)
  d <- data
  for (i in 1:ncol(d)) {

    d[[i]] <- as.vector(d[[i]])

  }
  return(d)
}


