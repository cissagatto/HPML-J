##################################################################################################
# HPML-J                                                                                         #
# Hybrid Partitions for Multi-label Classification version Jaccard                               #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #     
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################

##################################################################################################
# Script 4 - Miscellaneuous                                                                      #
##################################################################################################

#################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
shm = 0
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  shm = 1
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  shm = 0
}
shm = shm
setwd(FolderRoot)

# folder SCRIPTS
FolderScripts = paste(FolderRoot, "/scripts/", sep="")

# folder shm
FolderSHM = "/dev/shm/"



##################################################################################################
# FUNCTION EVALUATE GENERAL                                                                      #
#   Objective:                                                                                   #
#       Evaluate Multilabel                                                                      #  
#   Parameters:                                                                                  #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds to be created                                              #
#       Folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Confusion Matrix                                                                         #
##################################################################################################
evaluateGlobal <- function(ds, dataset_name, number_folds, Folder){  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
    f = 1
    avaliaParalel <- foreach (f = 1:number_folds) %dopar%{    
      library("utiml")
      library("mldr")    
      cat("\n\nSplit: ", f)    
      setwd(Folder)
      FolderSplit = paste(Folder, "/Split-", f, sep="")
      
      setwd(FolderSplit)
      y_pred = data.frame(read.csv("y_predict.csv"))
      y_true = data.frame(read.csv("y_true.csv"))
      
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
      
      salva3 = paste("ConfMatFold-", f, ".txt", sep="")
      setwd(FolderSplit)
      sink(file=salva3, type="output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()
      
      resConfMat = multilabel_evaluate(confmat)
      resConfMat = data.frame(resConfMat)
      names(resConfMat) = paste("Fold-", f, sep="")
      setwd(FolderSplit)
      write.csv(resConfMat, "ResConfMat.csv")    
      
      gc()
    }


  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE EVALUATION MISCELLANEOUS FUNCTION                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION RESUME RESULTS HYBRID PARTITIONS                                                      #
#   Objective                                                                                    #
#       Summarizes the results                                                                   #  
#   Parameters                                                                                   #
#       FolderHClust: path of files generated by HClust and Cutree                               #
#       FolderHybPart: file path of the generated hybrid partitions                              #
#       FolderD: specific dataset path                                                           #
#   Return                                                                                       #
#       data frame with the results                                                              #
##################################################################################################
resumeHP <- function(dataset_name, FolderHClust, FolderHybPart, FolderReports){
  
  retorno = list()
  
  # best hclust coeficients
  setwd(FolderHClust)
  coeficientes = data.frame(read.csv("BestFoldsCoef.csv"))
  names(coeficientes) = c("fold", "method", "coefficient")
  
  # best hybrid partitions
  setwd(FolderHybPart)
  bestPerformance = data.frame(read.csv("BestF1Macro.csv"))
  
  # put the best results in one dataframe
  resumo = cbind(coeficientes, bestPerformance)
  resumo = resumo[,-4]
  
  # save a csv file
  setwd(FolderReports)
  write.csv(resumo, paste(dataset_name, "-summaryHybPart.csv", sep=""), row.names = FALSE)
  
  # return values
  retorno$Resumo = resumo 
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE RESUME RESULTS FUNCTION                                                             #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION COMPARE METHODS                                                                       #
#   Objective                                                                                    #
#       compares the results between the methods                                                 #
#   Parameters                                                                                   #
#       FolderLocal: path of local partition results                                             #
#       FolderGlobal: global partition results path                                              #
#       FolderHybrid: path of hybrid partition results                                           #
#       FolderRandom1: path of random partitions 1 results                                       #
#       FolderRandom2: path of random partitions 2 results                                       #
#       FolderRandom3: path of random partitions 3 results                                       #
#       FolderDataset: specific dataset path                                                     #
#       FolderReports: specific dataset reports path                                             #
#   Return                                                                                       #
#       returns a dataframe with the results of all type of partitions                           #
##################################################################################################
compareMethods <- function(dataset_name, FolderRandom1, FolderRandom2, FolderRandom3, 
                           FolderLocal, FolderGlobal, FolderHybrid, FolderDataset, 
                           FolderReports){
  
  retorno = list()
  
  # Open Evaluation Measures for Hybrid Partitions
  setwd(FolderHybrid)
  hybrid = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(hybrid) = c("measure", "hybrid")
  measures = c(hybrid[,1])
  
  # Open Evaluation Measures for Random Partitions V1
  FolderR1 = paste(FolderRandom1, "/Test", sep="")
  setwd(FolderR1)
  randomPart1 = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(randomPart1) = c("measure", "random1")
  
  # Open Evaluation Measures for Random Partitions V2
  setwd(FolderRandom2)
  randomPart2 = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(randomPart2) = c("measure", "random2")
  
  # Open Evaluation Measures for Random Partitions V3
  FolderR3 = paste(FolderRandom3, "/Test", sep="")
  setwd(FolderR3)
  randomPart3 = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(randomPart3) = c("measure", "random3")
  
  # Open Evaluation Measures for Local Partitions
  setwd(FolderLocal)
  localPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(localPart) = c("measure", "local")
  
  # Open Evaluation Measures for Global Partitions
  setwd(FolderGlobal)
  globalPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(globalPart) = c("measure", "global")
  
  # Put all in a data frame
  Final = cbind(measures, hybrid = hybrid[, 2, drop = FALSE], random1 = randomPart1[, 2, drop = FALSE], 
                random2 = randomPart2[, 2, drop = FALSE], random3 = randomPart3[, 2, drop = FALSE], 
                local = localPart[, 2, drop = FALSE], global = globalPart[, 2, drop = FALSE])
  
  # save in a csv file
  setwd(FolderReports)
  write.csv(Final, paste(dataset_name, " - ResultsFinal.csv", sep=""), row.names = FALSE)
  
  # return values
  retorno$Compare = Final
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE COMPARE METHODS FUNCTION                                                            #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION DELETE ALL                                                                            #
#   Objective                                                                                    #
#       delete all files and folders                                                             #
#   Parameters                                                                                   #
#       number_folds: number of folds                                                            #
#       FolderLocal: path of local partition results                                             #
#       FolderGlobal: global partition results path                                              #
#       FolderHybrid: path of test hybrid partition results                                      #
#       FolderHClust: path of hclust results                                                     #
#       FolderHybPart: path of validation hybrid partition results                               #
#       FolderRandom1: path of random partitions version 1                                       #
#       FolderRandom2: path of random partitions version 2                                       #
#       FolderRandom3: path of random partitions version 3                                       #
#       FolderDataset: specific dataset path                                                     #
#   Return                                                                                       #
#      none                                                                                      #
##################################################################################################
deleteAll <- function(number_folds, FolderHybrid, FolderHybPart, FolderHClust, FolderLocal, 
                      FolderGlobal, FolderRandom1, FolderRandom2, FolderRandom3){
  
  f = 1
  daParalel <- foreach (f = 1:number_folds) %dopar%{    
    
    # Tested Hybrid Partitions
    Folder1 = paste(FolderHybrid, "/Split-", f, sep="")
    #str1 = paste("rm -r ", Folder1, sep="")
    print(system(str1))
    
    # Validated Hybrid Partitions
    Folder2 = paste(FolderHybPart, "/Split-", f, sep="")
    str2 = paste("rm -r ", Folder2, sep="")
    print(system(str2))
    
    # Local Partitions
    Folder4 = paste(FolderLocal, "/Split-", f, sep="")
    str4 = paste("rm -r ", Folder4, sep="")
    print(system(str4))
    
    # Global Partitions
    Folder5 = paste(FolderGlobal, "/Split-", f, sep="")
    str5 = paste("rm -r ", Folder5, sep="")
    print(system(str5))
    
    # Validated Random Partitions V1
    Folder6 = paste(FolderRandom1, "/Split-", f, "/Validation", sep="")
    str6 = paste("rm -r ", Folder6, sep="")
    print(system(str6))
    
    # Tested Random Partitions V2
    Folder7 = paste(FolderRandom2, "/Split-", f, "/Test", sep="")
    str7 = paste("rm -r ", Folder7, sep="")
    print(system(str7))
    gc()
    
    # Tested Random Partitions V3
    Folder8 = paste(FolderRandom3, "/Split-", f, "/Test", sep="")
    str8 = paste("rm -r ", Folder8, sep="")
    print(system(str8))
    gc()
    
  }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE DELETE ALL FUNCTION                                                                #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}



##################################################################################################
# FUNCTION RESUME PARTITIONS                                                                     #
#   Objective                                                                                    #
#       Count partitions                                                                         #
#   Parameters                                                                                   #
#       FolderHybPart: path of validation hybrid partition results                               #
#       FolderRandom1: path of random partitions version 1                                       #
#       FolderRandom2: path of random partitions version 2                                       #
#       FolderRandom3: path of random partitions version 3                                       #
#       FolderDataset: specific dataset path                                                     #
#       FolderReports: reports specific dataset path                                             #
#       ds: information about the dataset                                                        #
#       dataset_name: name of the dataset                                                        #
#   Return                                                                                       #
#       returns a dataframe with the number of times each partition was chosen by the method     #
##################################################################################################
resumePartitions <- function(ds, dataset_name, FolderRandom1, FolderRandom2, FolderRandom3,
                             FolderHybPart, FolderDataset, FolderReports){
  
  #cat("\nDataset: ", dataset_name)
  
  # get the number of partitions
  num.partitions = ds$Labels - 2
  cat("\nNumber of partitions: ", num.partitions)
  
  #cat("\nRandom1\n")
  setwd(FolderRandom1)
  random1 = data.frame(read.csv("BestF1Macro.csv"))
  names(random1) = c("fold","name.part", "num.part","MaF1")
  R1 = random1[,3, drop = FALSE]
  contaR1 = count(R1, vars=R1$num.part)
  names(contaR1) = c("partition", "R1")
  names(R1) = "Random1"
  
  #cat("\nRandom2\n")
  setwd(FolderRandom2)
  random2 = data.frame(read.csv("summary_partitions.csv"))
  R2 = random2[,3, drop = FALSE]
  contaR2 = count(R2, vars=R2$number_groups)
  names(contaR2) = c("partition", "R2")
  names(R2) = "Random2"
  
  #cat("\nRandom3\n")
  setwd(FolderRandom3)
  random3 = data.frame(read.csv("BestF1Macro.csv"))
  names(random3) = c("fold","name.part", "num.part","MaF1")
  R3 = random3[,3, drop = FALSE]
  contaR3 = count(R3, vars=R3$num.part)
  names(contaR3) = c("partition", "R3")
  names(R3) = "Random3"

  #cat("\nHYBRID\n")
  setwd(FolderHybPart)
  hybrid = data.frame(read.csv("BestF1Macro.csv"))
  names(hybrid) = c("fold","name.part", "num.part","MaF1")
  H = hybrid[,3, drop = FALSE]
  contaH = count(H, vars=H$num.part)
  names(contaH) = c("partition", "H")
  names(H) = "Hybrid"
  
  # save results
  setwd(FolderReports)
  write.csv(contaH, paste(dataset_name, "-hp-count-p.csv", sep=""))
  write.csv(contaR1, paste(dataset_name, "-r1-count-p.csv", sep=""))
  write.csv(contaR2, paste(dataset_name, "-r2-count-p.csv", sep=""))
  write.csv(contaR3, paste(dataset_name, "-r3-count-p.csv", sep=""))
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE RESUME PARTITIONS                                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}

creatingSFile <- function(ds, inicio, fim, TP, sFileName, trainFileName, testFileName){
  
  if(inicio == fim){
    
    cat("\nSINGLE LABEL")
    
    sink(sFileName, type = "output")
    
    cat("[General]")    
    cat("\nCompatibility = MLJ08")
    
    cat("\n\n[Data]")
    cat(paste("\nFile = ", trainFileName, sep=""))
    cat(paste("\nTestSet = ", testFileName, sep=""))
    
    cat("\n\n[Attributes]")
    cat("\nReduceMemoryNominalAttrs = yes")
    
    cat("\n\n[Attributes]")
    cat(paste("\nTarget = ", fim, sep=""))
    cat("\nWeights = 1")
    
    cat("\n")
    cat("\n[Tree]")
    cat("\nHeuristic = VarianceReduction")
    cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
    
    cat("\n\n[Model]")
    cat("\nMinimalWeight = 5.0")
    
    cat("\n\n[Output]")
    cat("\nWritePredictions = {Test}")
    cat("\n")
    sink()
  
  } else {
    
    cat("\nMULTI LABEL")
    
    sink(sFileName, type = "output")
    
    cat("[General]")    
    cat("\nCompatibility = MLJ08")
    
    cat("\n\n[Data]")
    cat(paste("\nFile = ", trainFileName, sep=""))
    cat(paste("\nTestSet = ", testFileName, sep=""))
    
    cat("\n\n[Attributes]")
    cat("\nReduceMemoryNominalAttrs = yes")
    
    cat("\n\n[Attributes]")
    cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
    cat("\nWeights = 1")
    
    cat("\n")
    cat("\n[Tree]")
    cat("\nHeuristic = VarianceReduction")
    cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
    
    cat("\n\n[Model]")
    cat("\nMinimalWeight = 5.0")
    
    cat("\n\n[Output]")
    cat("\nWritePredictions = {Test}")
    cat("\n")
    sink()
  }
  
  
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################