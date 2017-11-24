#' @title Create a labeled data.frame for SPSS, Stata or SAS export
#' @description Create a labeled data.frame from plain data.frame and matching metadata
#' @param data a data.frame stripped from any attributes
#' @param metadata a data.frame with matching meta in tidy format ie. each value of each variable on its own row
#' @author Markus Kainu <markuskainu@gmail.com>
#' @return data.frame
#' @examples
#'  \dontrun{
#'  create_labelled_data(data = stripped_spss_data, metadata = meta_spss)
#'  }
#'
#' @rdname create_labelled_data
#' @import dplyr labelled
#' @export

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
