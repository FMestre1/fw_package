#' Derive and plot the degree distribution
#'
#' @param list1 List with the interaction matrices
#' @param log Log-log scale
#' @param cumulative Show cumulative degree distribution
#' @return A plot with the degree distribution for the full dataset
#' @export
#' @examples
#' data(mg1)
#' dd.fw(mg1, log=TRUE, cumulative=TRUE)

dd.fw <- function(list1, log=TRUE, cumulative=TRUE){
  #Arguments:
  #list1 - food web list;

  #Check the list1 structure
  if(class(list1[[1]])=="list") {
    list2 <- list1[[1]]
  }else list2 <- list1

  df_final <- data.frame()

  for(i in 1:length(list2)){

    m1 <- list2[[i]]
    m1 <- as.matrix(m1)
    g1 <- igraph::graph_from_adjacency_matrix(m1, weighted=NULL,mode="undirected")
    g2 <- igraph::degree_distribution(g1, cumulative)

    d <- igraph::degree(g1, mode = "all")
    degree1 <- 1:max(d)
    probability <- g2[-1]

    # delete blank values
    nonzero.position <- which(probability != 0)

    #if(log==TRUE){probability <- log(probability[nonzero.position])
    #degree1 <- log(degree1[nonzero.position])}

    #if(log==FALSE){probability <- (probability[nonzero.position])
    #degree1 <- (degree1[nonzero.position])}

    probability <- (probability[nonzero.position])#NEW
    degree1 <- (degree1[nonzero.position])#NEW

    iter <- rep(paste0("iter",i), length(degree1))
    colour1 <- rep(randomColor(),length(iter))
    #class1 <- rep(classify[i],length(iter))
    #if(!is.null(classify)) df0 <- data.frame(iter, class1,degree1,probability,colour1)
    #if(is.null(classify)) df0 <- data.frame(iter,degree1,probability,colour1)
    df0 <- data.frame(iter,degree1,probability,colour1)#NEW (08-10-2020)
    df_final <- rbind(df_final, df0)
  }

#  if(is.null(classify)){

    if(log==TRUE){

      print(ggplot2::ggplot(df_final,aes(x = degree1, y = probability, group = iter)) +  # plot data
              geom_line(aes(col=factor(iter))) +
              theme(legend.position="none") +
              labs(title="Degree distribution (log-log)", x ="degree (log)", y = "Probability (log)")+
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_y_log10 () +
              scale_x_log10() +
              annotation_logticks()
              #scale_x_continuous(trans='log10') +
              #scale_y_continuous(trans='log10')
            )

    }
    if(log==FALSE){

      print(ggplot2::ggplot(df_final,aes(x = degree1, y = probability, group = iter)) +  # plot data
              geom_line(aes(col=factor(iter))) +
              theme(legend.position="none") +
              labs(title="Degree distribution", x ="degree", y = "Probability")+
              theme(plot.title = element_text(hjust = 0.5)))
    }

# }


#  if(is.null(classify)) return(df_final[,-4])

return(df_final[,-4])

}#end of dd.fw
