#' Converts all matrices to adjacency matrices (0 and 1 values for interaction)
#'
#' @param list1 List with the interaction matrices
#' @return A list of adjacency matrices (0: no interaction; 1: interaction).
#' @export
#' @examples
#' data(mg1)
#' mg2 <- convert2adjacency(mg1)


convert2adjacency <- function(list1){
  #which_i
  list3 <- list1#save list1 structure

  list_names <- names(list1[[1]])

  #if(any((names(list1))=="int_matrix"))
  if(any((names(list3))=="spatial_info")) list_S <- list3$spatial_info
  if(any((names(list3))=="ecosystem")) list_E <- list3$ecosystem
  if(any((names(list3))=="references")) list_R <- list3$references
  if(any((names(list3))=="code")) list_C <- list3$code
  

  #Check the list1 structure
  if(class(list1[[1]])=="list") {
    #nameslist <- names(list1)
    list1 <- list1[[1]]
  }

  list2 <- list()
  #which_i <- c()

  for(i in 1:length(list1)){

    matrix1 <- as.matrix(list1[[i]])

    matrix1[matrix1>0] <- 1
    #matrix1[matrix1<0] <- 0

    list2[[i]] <- as.data.frame(matrix1)

  }#end for loop

  #list2[sapply(list2, is.null)] <- NULL#remove null elements
  master.list <-list()

  #Check the list1 structure
  if(class(list3[[1]])=="list"){
    names(list2) <- list_names
    master.list[["int_matrix"]] <- list2
    if(any((names(list3))=="ecosystem")) master.list[["ecosystem"]] <- list_E
    if(any((names(list3))=="references")) master.list[["references"]] <- list_R
    if(any((names(list3))=="spatial_info")) master.list[["spatial_info"]] <- list_S
    if(any((names(list3))=="code")) master.list[["code"]] <- list_C
    }

  if(class(list3[[1]])!="list") names(list2) <- list_names

  if(class(list3[[1]])=="list") return(master.list)
  if(class(list3[[1]])!="list")return(list2)


}
