##################################################################################################
# In-between Global and Local Partitions for Multi-label Classification                          #
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
# Script 3 - Miscellaneuous                                                                      #
# LAST UPDATE: 2020-06-28                                                                        #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()


##################################################################################################
#
##################################################################################################
avalia <- function(ds, dataset_name, Folder){  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:10) %dopar%{    
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
    
    f = f + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO AVALIA MISCELLANEOUS                                                             #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
resume <- function(FolderHClust, FolderHybPart, FolderD){
  retorno = list()
  
  setwd(FolderHClust)
  coeficientes = data.frame(read.csv("BestFoldsCoef.csv"))
  
  setwd(FolderHybPart)
  bestPerformance = data.frame(read.csv("BestF1Macro.csv"))
  
  resumo = cbind(coeficientes, bestPerformance)
  resumo = resumo[,-4]
  
  setwd(FolderD)
  write.csv(resumo, "resumoDataset.csv", row.names = FALSE)
  
  retorno$Resumo = resumo 
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO RESUME                                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
compara <- function(FolderLocal, FolderGlobal, FolderHybrid, FolderBR, FolderDataset, ds, dataset_name){
  retorno = list()
  
  setwd(FolderHybrid)
  hybrid = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(hybrid) = c("measure", "hybrid")
  
  setwd(FolderLocal)
  localPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(localPart) = c("measure", "local")
  
  setwd(FolderGlobal)
  globalPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(globalPart) = c("measure", "global")
  
  setwd(FolderBR)
  brPart = data.frame(read.csv("SummaryFoldsEvaluated.csv"))
  names(brPart) = c("measure", "br")
  
  Final = cbind(hybrid, br = brPart$br, local = localPart$local, global = globalPart$global)
  setwd(FolderDataset)
  write.csv(Final, "ResultadoFinal.csv", row.names = FALSE)
  
  retorno$Compare = Final
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO COMPARA                                                                          #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################