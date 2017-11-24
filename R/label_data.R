#' @title Create a string or factor vector based on value labels in matching metadata
#' @description Create a string or factor vector based on value labels in matching metadata
#' @param data A data.frame including the source variable to be labeled
#' @param variable.data Code of the source variable in data.frame
#' @param variable.meta Code of the source variable in the matching metadata
#' @param metadata A tidy metadata
#' @param into.factor Whether the output variable should be converted into a factor or not
#' @author Markus Kainu <markuskainu@gmail.com>
#' @return vector
#' @examples
#'  \dontrun{
#'  label_data(data=d, variable.data="sex", variable.meta=variable.data, metadata=meta, into.factor=TRUE)
#'  }
#'
#' @rdname label_data
#' @import dplyr labelled
#' @export

label_data <- function(data=d, variable.data=varcode, variable.meta=variable.data, metadata=meta, into.factor=TRUE){

  library(dplyr)
  library(labelled)

  if (metadata[metadata$code %in% variable.meta,"class"] %in% c("numeric","character")) stop("Variable is either character or numeric and has no labels")
  vardata <- as_data_frame(metadata[metadata$code %in% variable.meta,], stringAsFactors=FALSE)
  new_values <- vardata$label[match(data[[variable.data]], vardata$value)]
  if (into.factor) new_values <- factor(new_values, levels=vardata$label)
  return(new_values)
}
