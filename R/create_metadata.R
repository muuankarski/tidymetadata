create_metadata <- function(data){

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
