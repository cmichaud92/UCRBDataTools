#' Title Check hierarchical data for orphaned records
#'
#' @param .parent The parent table
#' @param .child The child table
#' @param .key The primary key on the parent tabled referenced as a foreign key
#' on the child table
#' @param .write2csv Write orphaned records to root/orphan.csv?
#'
#' @return A warning and .csv (if requested) or a success message
#' @export
#'
#' @examples
#'parent <- data.frame(key = 1:5,
#'                     varA = letters[1:5])
#'child <- data.frame(key = c(1,2,5,7,10),
#'                    varB = LETTERS[6:10])
#'orphan(.parent = parent, .child = child, .key = "key")


orphan <- function(.parent, .child, .key, .write2csv = FALSE) {

  stopifnot(is.data.frame(.parent), is.data.frame(.child),
            .key %in% names(.parent), .key %in% names(.child))

  dat <- dplyr::anti_join(.child,
                          .parent,
                          by = c(.key))

  if (nrow(dat) > 0) {
    warning(paste("FAIL:",
                  nrow(dat),
                  "orphaned records found in",
                  deparse(substitute(.child)),
                  "dataset."))
    return(utils::head(dat))

    if (.write2csv == TRUE){
    readr::write_csv(dat, "./orphan.csv", na = "")
    }
  } else {
    message(paste("PASS: No orphaned records in",
                  deparse(substitute(.child)),
                  "dataset."))
  }
}
