#' Iterate through species-removal robustness assessments in food webs 
#'
#' @param fw_to_attack Food web matrix to be attacked, a matrix.
#' @param probs_of_fw Vector with the probability of attack to each node.
#' @param alpha1 Percentage of secondary extinctions after a primary extinction.
#' @param iter Number of iterations to simulate.
#' @param i_index Parameter, between 0 and 1 defining the probability of attacking hubs.
#' @param plot Plot the results of the simulation (TRUE/FALSE).
#' @param export_plot Should plots be exported to the working directory (TRUEFALSE)
#' @param plot_name Plot file naming.
#' @return A list of vectors with each metric.
#' @importFrom stats sd
#' @importFrom stats qnorm
#' @importFrom utils download.file
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_ribbon
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @export
#' @examples
#' data(mg1)
#' graph_list1 <- convert.to.graph.list(mg1)
#' #Create a vector with the values for the Intentionality Index (I)
#' i_index <- seq(from = 0, to = 1, by =0.01)
#' i_index <- head(i_index,-1)
#' fw1 <- graph_list1[[1]]
#' prob_exp <- exponent.removal(fw1, i_index)
#' it1 <- iterate(fw_to_attack=fw1, prob_exp, alpha1=50, iter=10, i_index, plot = TRUE)

iterate <- function(fw_to_attack, 
                    probs_of_fw, 
                    alpha1, 
                    iter, 
                    i_index, 
                    plot = FALSE, 
                    export_plot = FALSE, 
                    plot_name = NULL)
{
  
  #require(ggplot2)
  
  result_iterate <- data.frame(matrix(nrow = ncol(probs_of_fw)))
  
  for(i in 1:iter){
    r1 <- robustness(fw_to_attack, probs_of_fw, alpha1=50)
    R1 <- r1$Ralpha
    result_iterate <- cbind(result_iterate, R1)
    message(paste0("Iteration ", i))
  }
  
  result_iterate <- result_iterate[,-1]
  
  meanR <- apply(result_iterate, 1, FUN = mean)
  sdR <- apply(result_iterate, 1, FUN = sd)
  output <- as.data.frame(cbind(i_index, meanR, sdR))
  
  #Built confidence interval
  output.se <- output$sdR/sqrt(nrow(output))
  margin.error <- qnorm(0.975)*output.se
  
  #degrees.freedom <- nrow(output) - 1
  #t.score = qt(p=0.05/2, df=degrees.freedom,lower.tail=FALSE)
  
  #margin.error <- t.score * output.se
  #The confidence interval is the mean +/- margin of error
  
  lower.bound <- output$meanR - margin.error
  upper.bound <- output$meanR + margin.error
  
  output <- data.frame(output, lower.bound, upper.bound)
  if(any(output$lower.bound<0))output[output$lower.bound<0,]$lower.bound <- 0
  
  if (plot == TRUE){
    print(
      ggplot(output, aes(x=i_index, y=meanR), xlab = "label") +
        xlab("Intentionality (I)") +
        ylab(paste0("R", alpha1)) +
        ylim(0, (alpha1/100)+0.1) + 
        geom_line(color="steelblue4", lwd=1) +
        geom_ribbon(alpha=0.5, aes(ymin=lower.bound, ymax=upper.bound), fill="steelblue2", color="steelblue2")
    )
  }
  
  if (export_plot == TRUE){
    png(paste0(plot_name, ".png"), width = 500, height = 400)
    print(
      ggplot(output, aes(x=i_index, y=meanR), xlab = "label") +
        xlab("Intentionality (I)") +
        ylab(paste0("R", alpha1)) +
        ylim(0, (alpha1/100)+0.1) + 
        geom_line(color="steelblue4", lwd=1) +
        geom_ribbon(alpha=0.5, aes(ymin=lower.bound, ymax=upper.bound), fill="steelblue2", color="steelblue2")
    )
    dev.off()
  }
  
  return(output)
  
}