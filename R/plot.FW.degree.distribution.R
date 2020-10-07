plot.FW.degree.distribution <- function (FW_L, random_L, xlab = NULL, ylab = NULL, sig_info =NULL,log.scale = TRUE, iteration_plot){

  #The argument 'sig_info' is to be able to pass info about significance
  
    iter_plot <- paste0("iter", iteration_plot)
    fw_plot <- FW_L[ which(FW_L$iter==iter_plot),]
    random_plot <- random_L[[iteration_plot]]  
    #unique(random_plot$iter)

    if (log.scale == TRUE){
      #plot FW DD
      plot(fw_plot[,2:3],type="l", col="red", lwd=2, log = "xy", xlab, ylab)
      if(!is.null(sig_info)) text(x = min(fw_plot[,2])+((max(fw_plot[,2])-min(fw_plot[,2]))/10), y = min(fw_plot[,3])+((max(fw_plot[,3])-min(fw_plot[,3]))/10), labels = sig_info[iteration_plot])
      text(x = max(fw_plot[,2])-((max(fw_plot[,2])-min(fw_plot[,2]))/10), y = max(fw_plot[,3])-((max(fw_plot[,3])-min(fw_plot[,3]))/10), labels = as.character(iteration_plot))
      for(b in 1:100){
        iter_plot_2 <- paste0("iter", b)
        random_plot_1 <- random_plot[ which(random_plot$iter==iter_plot_2),]
        lines(random_plot_1[,2:3], col="grey")
      }
    }
    
    if (log.scale == FALSE){
      plot(fw_plot[,2:3],type="l", col="red", lwd=2, xlab, ylab)
      if(!is.null(sig_info)) text(x = min(fw_plot[,2])+((max(fw_plot[,2])-min(fw_plot[,2]))/10), y = min(fw_plot[,3])+((max(fw_plot[,3])-min(fw_plot[,3]))/10), labels = sig_info[iteration_plot])
      text(x = max(fw_plot[,2])-((max(fw_plot[,2])-min(fw_plot[,2]))/10), y = max(fw_plot[,3])-((max(fw_plot[,3])-min(fw_plot[,3]))/10), labels = as.character(iteration_plot))
      for(b in 1:100){
        iter_plot_2 <- paste0("iter", b)
        random_plot_1 <- random_plot[ which(random_plot$iter==iter_plot_2),]
        lines(random_plot_1[,2:3], col="grey")
      }
      
      
    }
    
}#end plot.FW.degree.distribution




