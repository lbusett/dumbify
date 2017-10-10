#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_data PARAM_DESCRIPTION
#' @param in_names PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname undumbify
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
undumbify <- function(in_data, in_names) {
  call = match.call()
  str_in_names <- as.character(call[[3]])
  lgt_in_names <- length(str_in_names)
  if (lgt_in_names == 1) {
    dumb_names <- str_in_names
  } else {
    dumb_names <- str_in_names[2:lgt_in_names]
  }
  cols_to_rename <- as.numeric(na.omit(match(dumb_names, names(in_data))))
  if (length(cols_to_rename != 0)) {
    existing_dumbs <- names(in_data)[cols_to_rename]
    names(in_data)[cols_to_rename]  <- in_names[which(dumb_names %in% existing_dumbs)]
  }
  which_modified <- grep("!!_!!_", names(in_data))
  if (length(which_modified) != 0) {
    names(in_data)[which_modified] <- sub("!!_!!_", "", names(in_data)[which_modified])
  }
  in_data
}
