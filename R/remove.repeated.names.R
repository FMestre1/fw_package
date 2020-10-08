#' Remove food web matrices with repeated names, whether in columns ot rows
#'
#' @param list1 List with the interaction matrices
#' @return A list of matrices.
#' @export
#' @examples
#' data(mg1)
#' mg5 <- remove.repeated.names(mg1)

remove.repeated.names <- function(list1){

  list3 <- list1

  list_names <- names(list1[[1]])

  #if(any((names(list1))=="int_matrix"))
  if(any((names(list1))=="spatial_info")) list_S <- list3$spatial_info
  if(any((names(list1))=="ecosystem")) list_E <- list3$ecosystem
  if(any((names(list1))=="references")) list_R <- list3$references
  if(any((names(list1))=="code")) list_C <- list3$code

  #Check the list1 structure
  if(class(list1[[1]])=="list") {
    #nameslist <- names(list1)
    list1 <- list1[[1]]
  }

  list2 <- list()
  which_i <- c()

  for(i in 1:length(list1)){

    m1 <- list1[[i]]

    names_gw_r <- rownames(m1)
    names_gw_c <- colnames(m1)

    names_gw_r <- sapply(strsplit(names_gw_r, split='sp_', fixed=TRUE), function(x) (x[2]))
    names_gw_r <- sapply(strsplit(names_gw_r, split='_', fixed=TRUE), function(x) (x[2]))
    names_gw_c <- sapply(strsplit(names_gw_c, split='sp_', fixed=TRUE), function(x) (x[2]))
    names_gw_c <- sapply(strsplit(names_gw_c, split='_', fixed=TRUE), function(x) (x[2]))

    #Same names in rows or columns?
    if(length(unique(names_gw_r)) < length(names_gw_r) | length(unique(names_gw_c)) < length(names_gw_c)){
      message(paste0("The food web ", i, " has repeated names in either columns or rows and will be removed!!"))
    }
    if(length(unique(names_gw_r)) == length(names_gw_r) && length(unique(names_gw_c)) == length(names_gw_c)){
      #list2 <- append(list2, m1)
      rownames(m1) <- names_gw_r
      colnames(m1) <- names_gw_c
      list2[[ (length(list2) + 1) ]] <- as.data.frame(m1)
      #list2 <- c(list2, m1)
      which_i <- c(which_i,i)
    }

  }#end of for loop

  master.list <-list()

  #Check the list1 structure
  if(class(list3[[1]])=="list"){
    names(list2) <- list_names[which_i]
    master.list[["int_matrix"]] <- list2
    #if(any((names(list3))=="int_matrix"))
    if(any((names(list3))=="spatial_info")) master.list[["spatial_info"]] <- list_S[which_i]
    if(any((names(list3))=="ecosystem")) master.list[["ecosystem"]] <- list_E[which_i,]
    if(any((names(list3))=="references")) master.list[["references"]] <- list_R[which_i,]
    if(any((names(list3))=="code")) master.list[["code"]] <- list_C[which_i]
    }

  names(list2) <- list_names[which_i]


  if(class(list3[[1]])=="list") return(master.list)
  if(class(list3[[1]])!="list")return(list2)

}#end remove repeated.names
