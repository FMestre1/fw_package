#' Check which matrices are square matrices (same number of columns and rows)
#'
#' @param list1 List with the interaction matrices
#' @return Information on whether each matrix is square (same number of columns and rows).
#' @export
#' @examples
#' data(mg1)
#' is.sq.matrix(mg1)

is.sq.matrix <- function(list1){

  #Check the list1 structure
  if(class(list1[[1]])=="list") {

    list1 <- list1[[1]]

  }

  for(i in 1:length(list1)){

    m1 <- list1[[i]]

    rnames <- rownames(m1)
    cnames <- colnames(m1)
    all_names <- c(rnames,cnames)

    if (ncol(m1)==nrow(m1) && setequal(rnames,cnames)) {
      message(paste0("Matrix ",i, " already is square!"))
    }
    if(ncol(m1)!=nrow(m1) | !setequal(rnames,cnames)){
      message(paste0("Matrix ",i, " NOT square!"))
    }

  }

}#END is.sq.matrix
