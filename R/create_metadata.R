#' @title Create a metadata from labeled data.frame SPSS, Stata or SAS export
#' @description Create a metadata from labeled data.frame imported from SPSS, Stata or SAS file
#' @param data a labeled data.frame imported from Stata, SPSS or SAS using haven
#' @author Markus Kainu <markuskainu@gmail.com>
#' @return data.frame
#' @examples
#'  \dontrun{
#'  create_metadata(data = imported_spss_data)
#'  }
#'
#' @rdname create_metadata
#' @import dplyr labelled
#' @export

create_metadata <- function(data){
  library(dplyr)
  library(labelled)
  d <- data
  meta_df <- data_frame()
  for (i in 1:ncol(d)){
    code  <- names(d[i])
    name <- attributes(d[[i]])$label
    label <- names(attributes(d[[i]])$labels)
    if (is.null(label)){
      value = NA
      label=NA
    } else {
      value = attributes(d[[i]])$labels
      names(value) <- NULL
    }
    if (is.null(name)) name="not applicaple"
    class <- ifelse(is.na(value), "numeric", "factor")
    if (class == "numeric") class <- ifelse(class(d[[i]]) %in% "numeric", "numeric", "character")
    new_row <- data_frame(code=code,
                          name=name,
                          label=label,
                          value=value,
                          class=class)
    meta_df <- rbind(meta_df,new_row) # korjaa tää!!!
  }
  return(meta_df)
}
