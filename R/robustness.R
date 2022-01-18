#' Conducts species-removal robustness assessments on food webs
#'
#' @param fw_to_attack Food web matrix to be attacked, a data frame.
#' @param probs_of_fw Vector with the probability of attack to each node.
#' @param alpha1 Percentage of secondary extinctions after a primary extinction.
#' @return
#' @importFrom igraph V
#' @importFrom igraph gsize
#' @importFrom igraph delete_vertices
#' @importFrom igraph gorder
#' @export
#' @examples
#' data(mg1)
#' graph_list1 <- convert.to.graph.list(mg1)
#' #Create a vector with the values for the Intentionality Index (I)
#' i_index <- seq(from = 0, to = 1, by =0.01)
#' i_index <- head(i_index,-1)
#' fw1 <- graph_list1[[1]]
#' prob_exp <- exponent.removal(fw1, i_index)
#' r1 <- robustness(fw1, prob_exp, alpha1=50)

robustness <- function(fw_to_attack, probs_of_fw, alpha1){
  #fw - List of food webs
  #probs - list of probs
  #to derive R_alpha11
  #require(igraph)
  
  ##FUNCTION TO ID ISOLATES (non-connected nodes)  
  isolates <- function(g){
    return(which(degree(g)==0)-1)
  }
  
  fw_nodes <- V(fw_to_attack)$name
  n_species <- length(fw_nodes)
  i_output_list <- list()
  
  for(j in 1:ncol(probs_of_fw)){
    output_nodes_and_links <- data.frame(matrix(ncol=n_species,nrow=5))
    colnames(output_nodes_and_links) <- paste0("del_species_", 1:(n_species-1))
    rownames(output_nodes_and_links) <- c("nodes","links","secondary_extinctions","%_extinctions", "n_primary_extinctions")
    probs_i <- probs_of_fw[,j] 
    n_links_original <- gsize(fw_to_attack)
    perc_ext <- 0
    
    for (z in 1:length(fw_nodes)){
      #across the species ro delete
      node_to_kill <- sample(x = fw_nodes, size = z, replace = FALSE, prob = probs_i)#Species that gets extinguished
      fw_resulting_from_attack <- delete_vertices(fw_to_attack, node_to_kill)#Delete nodes to kill from FW
      secondary_extinctions <- isolates(fw_resulting_from_attack) #Which are isolates?
      fw_resulting_from_attack <- igraph::delete.vertices(fw_resulting_from_attack, names(secondary_extinctions))#delete isolates
      if(gsize(fw_resulting_from_attack)==0 && gorder(fw_resulting_from_attack)==0)
      {
        output_nodes_and_links[1,z] <- NA
        output_nodes_and_links[2,z] <- NA
        output_nodes_and_links[3,z] <- NA 
        output_nodes_and_links[4,z] <- NA
        output_nodes_and_links[5,z] <- NA
      }
      #NODES
      if(gorder(fw_resulting_from_attack)!=0)
      {
        nodes_in_original_fw <- n_species #How many species in the starting FW?
        nodes_in_fw_resulting_from_attack <- gorder(fw_resulting_from_attack)#count vertices in the attacked FW
        lost_nodes <- nodes_in_original_fw - nodes_in_fw_resulting_from_attack #LOST NODES FROM PRIMARY EXTINCTION
        number_of_secondary_extinctions <- length(secondary_extinctions)#How many secondary extinctions?
        #LINKS
        links_in_original_fw <- n_links_original#count links in starting fw
        links_in_fw_resulting_from_attack <- gsize(fw_resulting_from_attack)#count links in the attacked FW
        lost_links <- links_in_original_fw - links_in_fw_resulting_from_attack #LOST LINKS FROM PRIMARY EXTINCTION
        #
        perc_ext <- ((n_species - nodes_in_fw_resulting_from_attack)*100)/n_species #Percentage of extinctions
        #
        output_nodes_and_links[1,z] <- nodes_in_fw_resulting_from_attack
        output_nodes_and_links[2,z] <- links_in_fw_resulting_from_attack
        output_nodes_and_links[3,z] <- number_of_secondary_extinctions 
        #if (z==1)output_nodes_and_links[3,z] <- number_of_secondary_extinctions 
        #if (z>1)output_nodes_and_links[3,z] <- number_of_secondary_extinctions +  output_nodes_and_links[3,z-1] 
        output_nodes_and_links[4,z] <- perc_ext
        output_nodes_and_links[5,z] <- z
        #
        #z <- z+1
        #idx <- which(fw_nodes1 %in% V(fw_resulting_from_attack)$name) # Which nodes are in the fw_nodes1 names vector
        #fw_nodes1 <- fw_nodes1[idx]#Select those      
        #probs_i <- probs_i[idx]#Select their probabilities of getting extinguished
      }
      #if(gorder(fw_resulting_from_attack)==0){
      #  output_nodes_and_links[1,z] <- NA
      #  output_nodes_and_links[2,z] <- NA
      #  output_nodes_and_links[3,z] <- NA 
      #  output_nodes_and_links[4,z] <- NA
      #  output_nodes_and_links[5,z] <- NA
      #} 
    }#END OF SPECIES
    
    i_output_list[[j]] <- output_nodes_and_links
    
  }#END OF GOING ACROSS I
  
  #Get the robustness
  R_alpha1 <- NA
  #total_species <- gorder(fw_to_attack)
  if (alpha1==50) prop_species <- n_species/2
  if (alpha1==100) prop_species <- n_species
  #
  #START of computing R
  for(x in 1:length(i_output_list)){
    
    df1 <- i_output_list[[x]]
    df1[4,] <- round(as.numeric(df1[4,]))
    #View(df1)
    #R_alpha1[x] <- min(which(df1[1,] < prop_species))/n_species
    #
    if(any(as.numeric(df1[4,]) <= alpha1, na.rm = TRUE))
    {
      col1 <- max(which(df1[4,] <= alpha1))
      R_alpha1[x] <- df1[5,col1]/n_species
    }else{
      #col1 <- 0
      R_alpha1[x] <- 0
    }
    
  }#END of computing R
  
  #SAVE OUTPUT
  #names(i_output_list) <- colnames(probs_of_fw)
  return(list(Simulation_results = i_output_list, Ralpha=R_alpha1))
  
}
