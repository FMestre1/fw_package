#' Remove matrices with non-numeric values
#'
#' @param list1 List with the interaction matrices
#' @return A list of matrices.
#' @export
#' @examples
#' data(mg1)
#' mg4 <- remove.non.numeric(mg1)

remove.non.numeric <- function(list1){
  

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
  which_i <- c()
  
  for(i in 1:length(list1)){
    m0 <- list1[[i]]
    m1 <- list1[[i]]
    m1 <-unique(as.vector(as.matrix(m1)))
    m3 <- c()
    for(a in 1:length(m1)){
      car <- m1[a]
      m3[a] <- all.is.numeric(car)
    }
    m3 <- m3[m3==TRUE]
    #m2 <- Numextract(m1)
    m2<-unlist(regmatches(m1,gregexpr("\\-*[[:digit:]]+\\.*[[:digit:]]*",m1)))
    m2 <- as.numeric(m2)
    m2 <- m2[m2>=0]
    #if(length(m2)!=length(m3)){
    if(!setequal(as.numeric(m1),m2) || !all(m3)){
      message(paste0("Food web ",i, " has non-numeric values and was removed!"))
    }
    #if(length(m2)==length(m3)){
    if(setequal(as.numeric(m1),m2) && all(m3)){
      #list2[[i]] <- m0
      list2[[ (length(list2) + 1) ]] <- as.data.frame(m0)
      #list2 <- c(list2, m0)
      which_i <- c(which_i,i)
    }
  }#end for loop
  
  #list2[sapply(list2, is.null)] <- NULL#remove null elements
  master.list <-list()
  
  #Check the list1 structure
  if(class(list3[[1]])=="list"){
    names(list2) <- list_names[which_i]
    master.list[["int_matrix"]] <- list2
    if(any((names(list3))=="ecosystem")) master.list[["ecosystem"]] <- list_E[which_i,]
    if(any((names(list3))=="references")) master.list[["references"]] <- list_R[which_i,]    
    if(exists("list_S")){
      if(any((names(list3))=="spatial_info" & class(list_S)!="data.frame")) master.list[["spatial_info"]] <- list_S[which_i]
      if(any((names(list3))=="spatial_info" & class(list_S)=="data.frame")) master.list[["spatial_info"]] <- list_S[which_i,]
    }
    if(any((names(list3))=="code")) master.list[["code"]] <- list_C[which_i]
    
  }
  
  names(list2) <- list_names[which_i]
  
  if(class(list3[[1]])=="list") return(master.list)
  if(class(list3[[1]])!="list")return(list2)
  
  
}#end remove.n.numeric
