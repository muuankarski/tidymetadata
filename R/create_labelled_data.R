#' @import dplyr labelled

create_labelled_data <- function(data,metadata){
  library(dplyr)
  library(labelled)
  d <- data
  meta <- metadata

  for (n in names(d)){
    var_label(d[[n]]) <- meta[meta$code %in% n, ]$name[1]
  }

  for (n in names(d)){
    # if numeric, no need for value label
    if (meta[meta$code %in% n,]$class[1] %in% c("numeric","character")) next()
    vec <- as.integer(meta[meta$code %in% n, ]$value)
    names(vec) <- meta[meta$code %in% n, ]$label
    if (n == "esimies") next()
    d[[n]] <- labelled(d[[n]], label=vec)
  }
  return(d)
}
