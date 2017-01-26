
add_to_metadata <- function(metadata=meta, code, name, label=NA, value=NA, class="numeric"){

  metadata <- bind_rows(metadata,
                        data_frame(code=code,
                                   name=name,
                                   label=label,
                                   value=value,
                                   class=class))
  return(metadata)
}
