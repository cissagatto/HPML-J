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
# Script 11 - Binary Relance                                                                     #
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
BinaryRelevance <- function(ds, dataset_name, Folder10F, FolderBR){
  
  FolderTR = paste(Folder10F, "/Tr", sep="")
  FolderTS = paste(Folder10F, "/Ts", sep="")  
  
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  confMatFinal = data.frame(medidas)
  folds = c("")
  
  i = 1
  while(i<=10){
    
    library("mldr")
    library("utiml")
    
    cat("\nFold ", i)
    
    Folder = paste(FolderBR, "/Split-", i, sep="")
    if(dir.exists(Folder)==TRUE){
      cat("\n")
    } else {
      dir.create(Folder)
    }
    
    setwd(FolderTR)
    nome = paste(dataset_name, "-Split-Tr-", i, ".csv", sep="")
    treino1 = data.frame(read.csv(nome))
    x = seq(ds$LabelStart, ds$LabelEnd, by=1)
    treino = mldr_from_dataframe(treino1, labelIndices = c(x))
    
    setwd(FolderTS)
    nome = paste(dataset_name, "-Split-Ts-", i, ".csv", sep="")
    teste1 = data.frame(read.csv(nome))
    teste = mldr_from_dataframe(teste1, labelIndices = c(x))
    
    brmodel <- br(treino, "J48", seed=123)
    prediction <- predict(brmodel, teste)
    evaluated <- multilabel_evaluate(teste, prediction)
    evaluated2 = data.frame(evaluated)
    
    # Build a confusion matrix
    salva3 = paste("ConfMatFold-", i, ".txt", sep="")
    setwd(Folder)
    sink(file=salva3, type="output")
    confmat <- multilabel_confusion_matrix(teste, prediction)
    print(confmat)
    sink()
    
    resConfMat <- multilabel_evaluate(confmat)
    resConfMat = data.frame(resConfMat)
    confMatFinal = cbind(confMatFinal, resConfMat)
    names(confMatFinal) = c("Measures", "Fold")
    
    folds[i] = paste("Fold-", i, sep="")
    
    i = i + 1
    gc()
  }
  
  setwd(FolderBR)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, "FoldsEvaluated.csv", row.names = FALSE)
  
  confMatFinal2 = data.frame(t(confMatFinal))
  confMatFinal3 = confMatFinal2[-1,]
  colnames(confMatFinal3) = medidas
  teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  
  sumary = apply(teste,2,mean)
  sumary2 = data.frame(sumary)
  sumary3 = cbind(medidas, sumary2)
  names(sumary3) = c("Measures", "Summary")
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# BINARY RELAVANCE: END!!!!                                                                      #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################