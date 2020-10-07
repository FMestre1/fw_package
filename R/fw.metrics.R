########## Function to derive metrics ########## 
fw.metrics <- function(list1){
  #all_models_01
  
  
  #Check the list1 structure
  if(class(list1[[1]])=="list") {
    list1 <- list1[[1]]
    }
  
  #dir.create(folder_name)
  #message(paste0("Just created a folder named ", folder_name, " to save all the intermediate files..."))
  #redefine wd
  #previous_WD <- getwd()
  #setwd(paste0(previous_WD,paste0("/", folder_name)))
  #mnames1 <- names(FWlist)
  nnodes <- c()
  nlinks <- c()
  linkdens <- c()
  connectance <- c()
  compartmentalization <- c()
  max_tlevel <- c()
  
  #nrFW <- length(FWlist)
  #if (plotFW==TRUE) par(mfrow3d(nr=2, nc=round(nrFW/2)))
  
  for(i in 1:length(list1)){
    message(paste0("Computing network metrics for food web ", i, " of ", length(list1), "..."))
    #fw1 <- all_models_01[[i]]
    tl <- list1[[i]]
    if(!any(apply(tl, 2, function(x) is.numeric(x)))) tl <- apply(tl, 2, function(x) as.numeric(x)) 
    test1 <- GenInd(tl)
    nnodes <- c(nnodes,test1$N)#number of nodes
    nlinks <- c(nlinks,test1$Ltot)#number of links
    linkdens <- c(linkdens,test1$LD)#link density (average # of links per node)
    connectance <- c(connectance,test1$C)#the connectance of the graph
    compartmentalization <- c(compartmentalization,test1$Cbar)#compartmentalization [0,1]
    test2 <- TrophInd(tl)
    max_tlevel <- c(max_tlevel,max(test2$TL))    
    
    #nnodes[i] <- test1$N#number of nodes
    #nlinks[i] <- test1$Ltot#number of links
    #linkdens[i] <- test1$LD#link density (average # of links per node)
    #connectance[i] <- test1$C#the connectance of the graph
    #compartmentalization[i] <- test1$Cbar#compartmentalization [0,1]
    #test2 <- TrophInd(tl)
    #max_tlevel[i] <- max(test2$TL)
    
    #write.table(tl, file = paste0("fw_",i ,".csv"), append=FALSE, sep=",", col.names=FALSE, row.names=FALSE)
    #analyse.single(filename=paste0("fw_",i ,".csv"))
    #tl_table <- read.csv(file=paste0("Results-", "fw_",i ,".csv"), header=TRUE, sep=",")
    #tl_tlevels <- tl_table$Total...trophic.positions
    #tlevels[i] <- tl_tlevels
    #connectance[i] <- tl_table$Connectance
    #if (plotFW==TRUE){
    #color <- heat.colors(tl_tlevels)
    #radii <- rep(5,tl_tlevels)
    #plotweb(cols=color, radii=radii)
    #title3d(main=mnames1[i])
    #}
  }  
  

  result <- list(number_nodes=nnodes,
                 number_links=nlinks,
                 link_density=linkdens,
                 connectance=connectance,
                 compartmentalization=compartmentalization,
                 maximum_trophic_level=max_tlevel
                 )
  
  #setwd(previous_WD) #restore wd
  
  #rgl.snapshot(filename="FW.png", fmt = "png", top = TRUE)
  
  return(result)
  
}