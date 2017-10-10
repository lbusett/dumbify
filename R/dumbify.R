#' @title dumbify
#' @description function to automatically rename columns in an input data frame
#'  or data table *within a function* so that columns needed for the analysis
#'  can be easily accessed.
#' @param in_data `data.frame` or `data.table`
#' @param in_names `character array` containing names of the variables containing
#'   the names of the columns of `in_data` needed for the analysis
#' @param out_names `character array` containing names to which the columns on
#'   selected in `in_names` should be changed to needed for the analysis. If NULL,
#'   out_names are reset to the names of the names of the variables passed as `in_names`
#'   (see examples),  Default: NULL
#' @return a copy of `in_data` with replaced column names
#' @details To avoid loss of data, the function checks beforehand to see if any
#'   columns in `in_data` are named as one of `out_names`. In that case, the names
#'   of those columns are replaced as "!!_!!_"
#' @examples
#' \dontrun{
#' in_data = iris
#' dumb_addcol <- function(in_data, ADD_1_VAR, ADD_2_VAR) {
#'   out_data <- in_data %>%
#'      dumbify(in_names = c(ADD_1_VAR, ADD_2_VAR)) %>%
#'      dplyr::mutate(sum = ADD_1_VAR + ADD_2_VAR) %>%
#'      undumbify(c(ADD_1_VAR, ADD_2_VAR))
#'  out_data
#'  }
#'  ADD_1_VAR   <- "Petal.Length"
#'  ADD_2_VAR   <- "Petal.Width"
#'  out <- dumb_addcol(in_data, ADD_1_VAR, ADD_2_VAR)
#'  head(out)
#'  ADD_2_VAR   <- "Sepal.Length"
#'  out <- dumb_addcol(in_data, ADD_1_VAR, ADD_2_VAR)
#'  head(out)
#' }
#' @rdname dumbify
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
dumbify <- function(in_data,
                    in_names,
                    out_names = NULL) {
  # browser()
  call <- match.call()
  if (missing(in_data)) {
    stop("dumbify --> argument `in_data` is missing. Aborting!")
  }

  if (missing(in_names)) {
    stop("dumbify --> argument `in_names` is missing. Aborting!")
  }

  if (!is.null(out_names)) {
    assertthat::assert_that(length(in_names) == length(out_names),
                            msg = "dumbify --> length(in_names) must be equal
                                to length(out_names). Aborting!")
  } else {
    str_in_names <- as.character(call[[3]])
    lgt_in_names <- length(str_in_names)
    if (lgt_in_names == 1) {
      out_names <- str_in_names
    } else {
      out_names <- str_in_names[2:lgt_in_names]
    }
  }

  #   ____________________________________________________________________________
  #   check if any columns in `in_data` are named as one of `out_names`       ####

  if (any(names(in_data) %in% out_names)) {
    which_dup <- which(names(in_data) %in% out_names)
    for (dup in which_dup) {
      names(in_data)[dup] <- paste("!!_!!_", names(in_data)[dup])
    }
  }

  #   ____________________________________________________________________________
  #   replace column names       ####

  col_indexes  <- which(names(in_data) %in% in_names)
  subs_indexes <- as.numeric(na.omit(match(names(in_data), in_names)))
  names(in_data)[col_indexes] <- out_names[subs_indexes]


  return(in_data)
}
