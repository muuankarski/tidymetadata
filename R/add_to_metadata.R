#' @title Add new items to metadata
#' @description Add new items to metadata, effectively a dplyr::bind_rows on top of current metadata
#' @param metadata metadata data.frame
#' @param code variable code of the new var in data
#' @param name a more expressive variable name
#' @param label vector of labels for each value
#' @param value each value as a vector
#' @param class class of the new vector as \code{numeric}, \code{factor} or \code{character}
#' @author Markus Kainu <markuskainu@gmail.com>
#' @return data.frame
#' @examples
#'  \dontrun{
#'  add_to_metadata(metadata=meta, code = "sex", name = "Repondents sex", label=c("male","female"), value=c(1:2), class="factor")
#'  }
#'
#' @rdname add_to_metadata
#' @import dplyr labelled
#' @export

add_to_metadata <- function(metadata=meta, code, name, label=NA, value=NA, class="numeric"){

  library(dplyr)
  library(labelled)

  metadata <- bind_rows(metadata,
                        data_frame(code=code,
                                   name=name,
                                   label=label,
                                   value=value,
                                   class=class))
  return(metadata)
}
