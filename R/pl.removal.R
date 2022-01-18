#' Computes de probability of removal of each node in the food web base on a power-law function
#'
#' @param fw Food web matrix to be attacked, a data frame.
#' @param i_index Parameter, between 0 and 1 defining the probability of attacking hubs.
#' @return
#' @export
#' @examples
#' data(mg1)
#' graph_list1 <- convert.to.graph.list(mg1)
#' #Create a vector with the values for the Intentionality Index (I)
#' i_index <- seq(from = 0, to = 1, by =0.01)
#' i_index <- head(i_index,-1)
#' fw1 <- graph_list1[[1]]
#' prob_pl <- pl.removal(fw1, i_index)

pl.removal <- function(fw, i_index){
  
  #fw - a igraph object with the fw
  #i_index - a vector of I (intentionality) to remove hubs (from 0 to 1)
  #plot - should the function plot the results?  
  
  #require(igraph)
  d1 <- as.data.frame(degree(fw))
  
  #Compute the species with tha maximum and minimum k in the FW
  kmax <- max(d1$`degree(fw)`)
  kmin <- min(d1$`degree(fw)`)
  k_vector <- sort(unique(d1$`degree(fw)`))
  
  #Create matrix for the prob results 
  colnames_Ppl <- paste0("Ppl_", i_index)
  Ppl_results <- as.data.frame(matrix(ncol = length(colnames_Ppl), nrow = nrow(d1)))
  Ppl_results <- cbind(d1,Ppl_results)
  colnames(Ppl_results)[-1] <- colnames_Ppl
  
  #Determine Pe - the probability of removal
  for (j in 1:nrow(d1)){
    
    current_species_k <- d1[j,]
    Ntot <- nrow(d1)
    
    for (i in 1:length(i_index)){
      
      current_i_value <- i_index[i]
      #
      sum_denominator <- as.data.frame(matrix(nrow = nrow(d1)))
      sum_denominator <- cbind(d1$`degree(fw)`, sum_denominator)
      for (f in 1:nrow(sum_denominator)) sum_denominator[f,2] <- sum_denominator[f,1]^current_i_value
      sum_denominator <- sum(sum_denominator$V1)
      # 
      #compute the probability of extinction for the species using this I
      Ppl <- (current_species_k^current_i_value)/sum_denominator
      
      Ppl_results[j,i+1] <- Ppl
      
    }#end of loop on I index
    
  }#end of loop across the species k
  
  return(Ppl_results[,-1])
  
}