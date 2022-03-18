#' Convert rectanguar matrices to square matrices (same number of columns and rows)
#'
#' @param list1 List with the interaction matrices
#' @param is.gw Was the dataset downloaded from Globalweb? (TRUE/FALSE)
#' @return A list of square matrices (not simetrical).
#' @export
#' @examples
#' data(mg1)
#' mg3 <- rect2square(mg1)

rect2square <- function(list1, is.gw=FALSE){

  
  list3 <- list1#save list1 structure
  
  #list_names <- names(list1[[1]])
  
  #if(any((names(list1))=="int_matrix"))
  if(any((names(list3))=="spatial_info")) list_S <- list3$spatial_info
  if(any((names(list3))=="ecosystem")) list_E <- list3$ecosystem
  if(any((names(list3))=="references")) list_R <- list3$references
  if(any((names(list3))=="code")) list_C <- list3$code
  
  
  #Check the list1 structure
  if(class(list1[[1]])=="list") {
    #nameslist <- names(list1)
    list1 <- list1[[1]]
    list_names <- names(list1)
  }
  
  if(class(list1[[1]])!="list") list_names <- names(list1)
  
  list2 <- list()
  
  #length(list2)
  
  suppressWarnings(
    {
      
      for(i in 1:length(list1)){
        
        m1 <- list1[[i]]
        
        rnames <- rownames(m1)
        cnames <- colnames(m1)
        all_names <- c(rnames,cnames)
        
        if (ncol(m1)==nrow(m1) && setequal(rnames,cnames)) {
          message(paste0("Matrix ",i, " already is square!"))
          m2 <- m1
          list2[[i]] <- as.data.frame(m2)
        }#end of 'if'
        
        if (ncol(m1)!=nrow(m1) | !setequal(rnames,cnames)){
          message(paste0("Matrix ", i, " needs to be 'squared'!"))
          col_names0 <- colnames(m1)
          row_names0 <- rownames(m1)
          #col_names0 <- gsub("[.](?!\\d+$)", " ", col_names0, perl=TRUE)#remove dots
          #row_names0 <- gsub("[.](?!\\d+$)", " ", row_names0, perl=TRUE)#remove dots
          col_names0 <- stringr::str_replace_all(col_names0,"\\."," ")
          row_names0 <- stringr::str_replace_all(row_names0,"\\."," ")
          col_names0 <- stringr::str_replace_all(col_names0,"-"," ")#remove dashes
          row_names0 <- stringr::str_replace_all(row_names0,"-"," ")#remove dashes
          col_names0 <- trimws(col_names0, which = "both")#remove starting and endinf whitspaces
          row_names0 <- trimws(row_names0, which = "both")#remove starting and endinf whitspaces
          all_names <- unique(c(col_names0,row_names0))
          size_M <- length(all_names)
          #
          colnames(m1) <- col_names0
          rownames(m1) <- row_names0
          #
          m2 <- as.data.frame(matrix(ncol=size_M, nrow=size_M))
          colnames(m2) <- all_names
          rownames(m2) <- all_names
          #col_names0 <- unique(col_names0)
          #row_names0 <- unique(row_names0)
          for(a in 1:size_M){
            for(b in 1:size_M){
              row1 <- all_names[a]
              col1 <- all_names[b]
              row1 <- which(rownames(m2)==row1)
              col1 <- which(colnames(m2)==col1)
              el1 <- as.numeric(as.character(m1[all_names[a],all_names[b]]))
              #el2 <- as.numeric(as.character(m1[all_names[b],all_names[a]]))
              #el3 <- c(el1,el2)
              ##el3 <- el3[el3!=0]
              #el3 <- unique(el3)
              if(identical(el1,numeric(0))) el1 <- 0
              if(is.na(el1)) el1 <- 0
              #if(el3[1]==el3[2]) el3 <- el3[1]
              #if(length(el3)==0) el2 <- 0
              m2[a,b] <- el1
            }
          }
          
          list2[[i]] <- as.data.frame(m2)
          #return(list2)
          
        }#end of 'if'
        
        #list2[[i]] <- as.data.frame(m2)
        
      }#end for loop
      
    })
  
  master.list <-list()
  
  #Check the list1 structure
  if(class(list3[[1]])=="list"){
    names(list2) <- list_names
    master.list[["int_matrix"]] <- list2
    #if(any((names(list3))=="int_matrix"))
    if(any((names(list3))=="spatial_info")) master.list[["spatial_info"]] <- list_S
    if(any((names(list3))=="ecosystem")) master.list[["ecosystem"]] <- list_E
    if(any((names(list3))=="references")) master.list[["references"]] <- list_R
    if(any((names(list3))=="code")) master.list[["code"]] <- list_C
    
  }
  
  #if(class(list3[[1]])!="list") names(list2) <- list_names
  
  if(class(list3[[1]])=="list") return(master.list)
  if(class(list3[[1]])!="list")return(list2)
  
  

}#END rect2square
