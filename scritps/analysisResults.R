##################################################################################################
# HPML-J                                                                                         #
##################################################################################################

##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
##################################################################################################

##################################################################################################
# Script 13 - Results Analysis
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()

# hclust


##################################################################################################
#
##################################################################################################
graphics <- function(Folder, namesLabels, number_folds){
  
  setwd(Folder)
  Graphics = paste(Folder, "/Graphics", sep="")
  if(dir.exists(Graphics) == FALSE){
    dir.create(Graphics)
  }
  setwd(Graphics)
  
  i = 4
  while(i<=ds$Labels){
    cat("\n\nROTULO: ", i)
    setwd(Folder)
    rotulos = data.frame(read.csv(paste("rotulos-", i, ".csv", sep=""), stringsAsFactors = FALSE))
    rotulos2 = rotulos[,-1]
    coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
    
    setwd(Graphics)
    pdf(paste(i, "-levelplot.pdf", sep=""), width = 6, height = 6)
    print(levelplot(as.matrix(rotulos2), col.regions = coul))
    dev.off()
    
    
    i = i + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE ANALYSE PARTITIONS                                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}


