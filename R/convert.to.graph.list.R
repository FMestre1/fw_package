#' Convert to a list of igraph objects 
#'
#' @param list1 List produced by the function create.fw.list
#' @return Returns a list of igraph objects.
#' @export
#' @examples
#' data(mg1)
#' graph_list1 <- convert.to.graph.list(mg1)

convert.to.graph.list <- function(list1){
  #
  list1 <- list1$int_matrix
  #
  output.list <- list()
  list_n <- names(list1)
  
  for (i in 1:length(list1)){    
    
    s1 <- list1[[i]] #get matrix
    
    g1 <- igraph::graph_from_adjacency_matrix(
      adjmatrix = as.matrix(s1),
      mode = "undirected",
      diag = TRUE) # convert to graph
    
    output.list[[i]] <- g1
    
    message(paste0("Just converted to graph the matrix ", i, " of ", length(list1)))
    
  }
  
  names(output.list) <- list_n
  return(output.list)
  
}