#' @title Export The Metadata from Labelled Data Frame
#' @description Create a metadata from labelled data.frame
#' imported from SPSS, Stata or SAS file
#' @param data a labelled data.frame imported from Stata, SPSS
#'  or SAS using haven
#' @author Daniel Antal <daniel.antal@ceemid.eu>
#' @return Returns a metadata data frame with five columns.
#' Each row corresponds to a column in the \code{'data'} data frame.
#' @examples
#'  \dontrun{
#'  create_metadata2(data = imported_spss_data)
#'  }
#'
#' @rdname create_metadata2
#' @importFrom dplyr distinct bind_cols
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom purrr set_names map
#' @importFrom haven is.labelled
#' @export

create_metadata2 <- function(data){

  d <- data
  fn_label  <- function(a) {
    if( is.null(attributes(a)$label) ) { return(NA_character_ )
    } else {as.character(attributes(a)$label) }
  }

  fn_labels_old  <- function(a) {

    if ("labels" %in% names ( attributes(a)) ) {
      list ( labels = attributes(a)$labels,
               label = names(attributes(a)$labels),
               value = attributes(a)$label)
    } else {
      list ( labels = attributes(a)$labels,
               label = NA_character_,
               value = NA_character_ )

    }
  }

  fn_labels <- function(a) {
   if ( 'labels' %in% names (a) ) {
     list(names = names(a$labels),
          values = unname(a$labels))
   }  else {
     list(names = NA_character_,
          values = NA_character_)
   }
  }

  fn_labels(a)

  meta_df <- tibble(
    code  = names(d),
    name  =  vapply ( d, fn_label,  character(1) ),
    class = ifelse ( test = vapply( d, is.character, logical(1) ),
                     yes = "character",
                     no  = ifelse( test = vapply(d, is.labelled, logical(1)),
                                   yes =  "labelled",
                                   no = "numeric")))



  attribs <- map ( data, attributes)
  my_attribs <- map( attribs, fn_labels)
  value_list <- sapply(my_attribs, "[", -(1))

  tested <- do.call(rbind.data.frame, value_list)
  tested$code <- meta_df$code
  new_names <- c(paste0("var", 1:(ncol(tested)-1)),
                 "code")
  tested <- purrr::set_names ( tested, nm = new_names )

  wide_cols <- c(paste0("var", 1:(ncol(tested)-1)))
  long_form <-  tidyr::pivot_longer (
                       data = tested,
                       cols = wide_cols,
                       values_to = 'value',
                       names_to = 'var') %>%
    distinct ( code, value , .keep_all = T)

  name_list <- unlist(sapply(my_attribs, "[[", 1))
  attrib_df <- setNames(stack(setNames(name_list, value_list))[2:1],
           c('value2', 'label'))

  metadata <- bind_cols ( long_form, attrib_df ) %>%
    select ( -all_of(c("value2", "var")))

  return_metadata <- meta_df %>%         ## assing for code checking only
    left_join ( metadata, by = 'code' )

  return_metadata
}
