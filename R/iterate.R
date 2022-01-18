#' Iterate through species-removal robustness assessments in food webs 
#'
#' @param fw_to_attack xxx
#' @param probs_of_fw xxx
#' @param alpha1 xxx
#' @param iter xxx
#' @param i_index xxx
#' @param export_plot xxx
#' @param plot_name xxx 
#' @return A list of vectors with each metric.
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
#' r1 <- robustness(fw1, prob_exp, alpha1=50)
#' it1 <- iterate(fw_to_attack=fw1, prob_exp, alpha1=50, iter=10, i_index, plot = TRUE)

iterate <- function(fw_to_attack, 
                    probs_of_fw, 
                    alpha1, 
                    iter, 
                    i_index, plot = FALSE, 
                    export_plot = FALSE, 
                    plot_name = NULL)
{
  
  require(ggplot2)
  
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
        ylim(0, 0.9) + 
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
        ylim(0, 0.9) + 
        geom_line(color="steelblue4", lwd=1) +
        geom_ribbon(alpha=0.5, aes(ymin=lower.bound, ymax=upper.bound), fill="steelblue2", color="steelblue2")
    )
    dev.off()
  }
  
  return(output)
  
}