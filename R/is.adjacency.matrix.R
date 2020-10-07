#Are matrix bollean?
is.adjacency.matrix <- function(list1){

  #Check the list1 structure
  if(class(list1[[1]])=="list") {

    list1 <- list1[[1]]

  }

  for(i in 1:length(list1)){

    m1 <- list1[[i]]

    if(length(unique(as.vector(as.matrix(m1))))==2 && as.numeric(max(as.matrix(m1)))==1 && as.numeric(min(as.matrix(m1)))==0) message(paste0("Matrix ", i, " is an adjacency matrix!"))

    if(length(unique(as.vector(as.matrix(m1))))>2) message(paste0("Matrix ", i, " is NOT bollean!"))

  }

}#end is.bollean.int
