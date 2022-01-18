#' Computes de probability of removal of each node in the food web base on a exponential function
#'
#' @param fw xxx
#' @param i_index xxx
#' @return
#' @export
#' @examples
#' data(mg1)
#' graph_list1 <- convert.to.graph.list(mg1)
#' 
#' #Create a vector with the values for the Intentionality Index (I)
#' i_index <- seq(from = 0, to = 1, by =0.01)
#' i_index <- head(i_index,-1)
#' fw1 <- graph_list_1[[1]]
#' prob_exp <- exp.removal(fw1, i_index)

exp.removal <- function(fw, i_index){
  
  #fw - a igraph object with the fw
  #i_index - a vector of I (intentionality) to remove hubs (from 0 to 1)
  #plot - should the function plot the results?  
  
  require(igraph)
  d1 <- as.data.frame(degree(fw))
  
  #Compute the species with tha maximum and minimum k in the FW
  kmax <- max(d1$`degree(fw)`)
  kmin <- min(d1$`degree(fw)`)
  k_vector <- sort(unique(d1$`degree(fw)`))
  
  #Create matrix for the prob results 
  colnames_Pe <- paste0("Pe_", i_index)
  Pe_results <- as.data.frame(matrix(ncol = length(colnames_Pe), nrow = nrow(d1)))
  Pe_results <- cbind(d1,Pe_results)
  colnames(Pe_results)[-1] <- colnames_Pe
  
  #Determine Pe - the probability of removal
  for (j in 1:nrow(d1)){
    
    current_species_k <- d1[j,]
    Nk <- sum(d1$`degree(fw)`==current_species_k)
    
    for (i in 1:length(i_index)){
      
      current_i_value <- i_index[i]
      
      sum_denominator <- as.data.frame(matrix(nrow = length(k_vector)))
      sum_denominator <- cbind(k_vector, sum_denominator)
      for (f in 1:nrow(sum_denominator)){
        Ni <- sum(d1$`degree(fw)`==sum_denominator[f,1])
        sum_denominator[f,2] <- ((1-current_i_value)^(kmax-sum_denominator[f,1]))*Ni
      }
      # 
      sum_denominator <- sum(sum_denominator$V1)
      #
      #compute the probability of extinction for the species using this I
      Pe <- (((1-current_i_value)^(kmax-current_species_k))*Nk)/sum_denominator
      
      Pe_results[j,i+1] <- Pe
      
    }#end of loop on I index
    
  }#end of loop across the species k
  
  return(Pe_results[,-1])
  
}