label_data <- function(data=d, variable.data=varcode, variable.meta=variable.data, metadata=meta, into.factor=TRUE){
  if (metadata[metadata$code %in% variable.meta,"class"] %in% c("numeric","character")) stop("Variable is either character or numeric and has no labels")
  vardata <- as_data_frame(metadata[metadata$code %in% variable.meta,], stringAsFactors=FALSE)
  new_values <- vardata$label[match(data[[variable.data]], vardata$value)]
  if (into.factor) new_values <- factor(new_values, levels=vardata$label)
  return(new_values)
}
