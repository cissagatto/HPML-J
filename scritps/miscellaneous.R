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
# Script 4 - Miscellaneuous                                                                      #
##################################################################################################

##################################################################################################
# SET WORKING DIRECTORY                                                                          #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()

##################################################################################################
# FUNCTION EVALUATE GENERAL                                                                      #
#   Objective:                                                                                   #
#       Evaluate Global Partitions                                                               #  
#   Parameters:                                                                                  #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds to be created                                              #
#       Folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Confusion Matrix                                                                         #
##################################################################################################
evaluateGeneral <- function(ds, dataset_name, Folder, number_folds){  
  
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
      
      #f = f + 1
      gc()
    }


  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE EVALUATION MISCELLANEOUS FUNCTION                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION RESUME RESULTS                                                                        #
#   Objective                                                                                    #
#       Summarizes the results                                                                   #  
#   Parameters                                                                                   #
#       FolderHClust: path of files generated by HClust and Cutree                               #
#       FolderHybPart: file path of the generated hybrid partitions                              #
#       FolderD: specific dataset path                                                           #
#   Return                                                                                       #
#       data frame with the results                                                              #
##################################################################################################
resumeResults <- function(FolderHClust, FolderHybPart, FolderD){
  retorno = list()
  
  setwd(FolderHClust)
  coeficientes = data.frame(read.csv("BestFoldsCoef.csv"))
  names(coeficientes) = c("fold", "method", "coefficient")
  
  setwd(FolderHybPart)
  bestPerformance = data.frame(read.csv("BestF1Macro.csv"))
  
  resumo = cbind(coeficientes, bestPerformance)
  resumo = resumo[,-4]
  
  setwd(FolderD)
  write.csv(resumo, "summaryHybPart.csv", row.names = FALSE)
  
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
#       FolderDataset: specific dataset path                                                     #
#   Return                                                                                       #
#       data frame with the results                                                              #
##################################################################################################
compareMethods <- function(FolderRandom, FolderLocal, FolderGlobal, FolderHybrid, FolderDataset){
  retorno = list()
  
  #FolderRandom = folders$folderRandom
  #FolderLocal = folders$folderLocal
  #FolderGlobal = folders$folderGlobal
  # FolderHybrid = folders$folderHybrid
  #FolderDataset = folders$folderResDataset
  
  setwd(FolderHybrid)
  hybrid = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(hybrid) = c("measure", "hybrid")
  
  # C:\Users\elain\HPML-J\results\flags\ClusRandom\Test
  FolderRandomT = paste(FolderRandom, "/Test", sep="")
  
  setwd(FolderRandomT)
  randomPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(randomPart) = c("measure", "random")
  
  setwd(FolderLocal)
  localPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(localPart) = c("measure", "local")
  
  setwd(FolderGlobal)
  globalPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(globalPart) = c("measure", "global")
  
  Final = cbind(hybrid, random = randomPart$random, local = localPart$local, global = globalPart$global)
  setwd(FolderDataset)
  write.csv(Final, "ResultsFinal.csv", row.names = FALSE)
  
  retorno$Compare = Final
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE COMPARE METHODS FUNCTION                                                            #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}




##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################