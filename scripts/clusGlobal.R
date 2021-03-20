##################################################################################################
# HPML-J                                                                                         #
# Hybrid Partitions for Multi-label Classification version Jaccard                               #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This program is free software: you can redistribute it and/or modify it under the terms of the #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This program is distributed in the hope    #
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
# Script 9 - Clus Global                                                                         #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()


##################################################################################################
# FUNCTION GATHER FILES FOLDS GLOBAL                                                             #
#   Objective                                                                                    #
#       Joins the configuration, training and test files in a single folder for running the clus #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderConfifFiles: folder path                                                           #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
gatherFilesFoldsGlobal <- function(ds, dataset_name, number_folds, FolderConfigFiles, FolderGlobal){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  # specifying folders
  FolderTr = paste(diretorios$folderFolds, "/", dataset_name, "/Tr", sep="")
  FolderTs = paste(diretorios$folderFolds, "/", dataset_name, "/Ts", sep="")   
  
  # from fold = 1 to number_folds
    s = 1
    foldsParalel <- foreach(s = 1:number_folds) %dopar% {
      
      cat("\nFold: ", s)
      
      # specifying folder
      FS = paste(FolderGlobal, "/Split-", s, sep="")
      if(dir.exists(FS)==TRUE){
        cat("\n\tFolderGlobal exists")
      } else {
        dir.create(FS)
      }
      
      # names files
      nome_tr = paste(dataset_name, "-Split-Tr-", s, ".arff", sep="")
      nome_ts = paste(dataset_name, "-Split-Ts-", s, ".arff", sep="")
      nome_config = paste(dataset_name, "-Split-", s, ".s", sep="")
      
      # train
      setwd(FolderTr)
      if(file.exists(nome_tr) == TRUE){
        setwd(FolderTr)
        copia = paste(FolderTr, "/", nome_tr, sep="")
        cola = paste(FS, "/", nome_tr, sep="")
        file.copy(copia, cola, overwrite = TRUE)
        cat("\n\tTransfer Train", s)
      } else {
        cat("\n")
      }
      
      # test
      setwd(FolderTs)
      if(file.exists(nome_ts) == TRUE){
        setwd(FolderTs)
        copia = paste(FolderTs, "/", nome_ts, sep="")
        cola = paste(FS, "/", nome_ts, sep="")
        file.copy(copia, cola, overwrite = TRUE)
        cat("\n\tTransfer test", s)
      } else {
        cat("\n")
      }
    
      gc()
    }

  gc()
  cat("\n##################################################################################################")
  cat("\n# GLOBAL CLUS: END OF THE GATHER FILES FOLDS FUNCTION                                            #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION EXECUTE CLUS GLOBAL                                                                   #
#   Objective                                                                                    #
#       Tests global partitions                                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder path                                                                      #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
executeClusGlobal <- function(ds, dataset_name, number_folds, Folder){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  # specifying folder
  folderUtils = paste(FolderRoot, "/utils", sep="")
  
  # from fold = 1 to number_folds
    i = 1
    clusGlobalParalel <- foreach(i = 1:number_folds) %dopar% {
      
      library("RWeka")
      library("rJava")
      
      cat("\nFold: ", i)
      
      # specifying folders
      FolderSplit = paste(Folder, "/Split-", i, sep="")
      nome_config = paste(FolderSplit, "/", dataset_name, "-Split-", i, ".s", sep="")
      
      inicio = ds$LabelStart
      fim = ds$LabelEnd
      
      # emotions-Split-Tr-1.arff
      trainFileName = paste(FolderSplit, "/", dataset_name, "-Split-Tr-", i , ".arff", sep="")
      # emotions-Split-Ts-1.arff
      testFileName = paste(FolderSplit, "/", dataset_name, "-Split-Ts-", i, ".arff", sep="")
      
      sink(nome_config, type = "output")
      
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

      cat("\nExecute CLUS\n")
      setwd(FolderSplit)
      str = paste("java -jar ", folderUtils , "/Clus.jar ", nome_config, sep="")
      cat("\n")
      print(system(str))
      cat("\n")
      
      # delete files
      um = paste(dataset_name, "-Split-", i, ".model", sep="")
      dois = paste(dataset_name, "-Split-", i, ".s", sep="")
      tres = paste(dataset_name, "-Split-Tr-", i, ".arff", sep="")
      quatro = paste(dataset_name, "-Split-Ts-", i, ".arff", sep="")
      
      setwd(FolderSplit)
      unlink(um, recursive = TRUE)
      #unlink(dois, recursive = TRUE)
      unlink(tres, recursive = TRUE)
      unlink(quatro, recursive = TRUE)
      
      gc()
    }

  gc()
  cat("\n##################################################################################################")
  cat("\n# GLOBAL CLUS: END OF FUNCTION EXECUTE CLUS                                                      #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER PREDICTS GLOBAL                                                                #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder path                                                                      #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gatherPredictsGlobal <- function(ds, dataset_name, number_folds, Folder){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()   
  
  # from fold = 1 to number_folds
    f = 1
    predGlobalParalel <- foreach(f = 1:number_folds) %dopar% {    
      
      library("foreign")    
      
      cat("\nFold: ", f)    
      
      # specifying folder
      FolderSplit = paste(Folder, "/Split-", f, sep="")    
      
      #cat("\n\tOpen Test.Pred.Arff ", f)
      setwd(FolderSplit)    
      nome = paste(FolderSplit, "/", dataset_name, "-Split-" , f, ".test.pred.arff", sep="")
      predicoes = data.frame(read.arff(nome))
      
      inicio = ds$LabelStart
      fim = ds$LabelEnd
      comeco = 1+(fim - inicio)
      
      # cat("\n\tSave Y_true")
      classes = data.frame(predicoes[,1:comeco])
      write.csv(classes, "y_true.csv", row.names = FALSE)    
      
      rotulos = c(colnames(classes))
      n_r = length(rotulos)
      nomeColuna = c()
      a = 1 
      while(a <= n_r){
        nomeColuna[a] = paste("Pruned.p.", rotulos[a], sep="")
        a = a + 1
        gc()
      }
      
      # cat("\n\tSave Y_pred")
      setwd(FolderSplit)
      pred = data.frame(predicoes[nomeColuna])
      names(pred) = rotulos
      write.csv(pred, "y_predict.csv", row.names = FALSE)  
      
      gc()
    }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER PREDICTS                                               #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS GLOBAL PARTITIONS                                                     #
#   Objective                                                                                    #
#      Evaluates the global partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: path of global partition results                                                 #
#   Return                                                                                       #
#       Assessment measures for each global partition                                            #
##################################################################################################
gatherEvalGlobal <- function(ds, dataset_name, number_folds, Folder, FolderReports){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  retorno = list()
  
  # vector with names measures
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  # dta frame
  confMatFinal = data.frame(medidas)
  folds = c("")
  
  # from fold = 1 to number_labels
  f = 1
  while(f<= number_folds){
    cat("\nFold: ", f)
    
    FolderSplit = paste(Folder, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    #cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv(paste(FolderSplit, "/ResConfMat.csv", sep="")))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    #cat("\n\tDelete unecessary files")
    setwd(FolderSplit)
    unlink("ResConfMat", recursive = TRUE)
    
    folds[f] = paste("Fold-", f, sep="")
    
    # delete files
    setwd(FolderSplit)
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  } 
  
  # save measures
  setwd(Folder)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, "FoldsEvaluated.csv", row.names = FALSE)
  
  # adjust
  confMatFinal2 = data.frame(t(confMatFinal))
  confMatFinal3 = confMatFinal2[-1,]
  colnames(confMatFinal3) = medidas
  teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  
  # summary
  sumary = apply(teste,2,mean)
  sumary2 = data.frame(sumary)
  sumary3 = cbind(medidas, sumary2)
  names(sumary3) = c("Measures", "Summary")
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  # save
  setwd(FolderReports)
  write.csv(sumary3, paste(dataset_name, "-SummaryFoldsEvaluated-G.csv", sep=""), row.names = FALSE)
  
  # return
  retorno$summaryEvaluation = sumary3
  retorno$foldsEvaluated = confMatFinal
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER EVALUATED                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION DELETE GLOBAL PARTITION                                                               #
#   Objective                                                                                    #
#       deletes all unnecessary files                                                            #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderGlobal path of hybrid partition results                                            #
#   Return                                                                                       #
##################################################################################################
deleteGlobal <-function(ds, dataset_name, number_folds, FolderGlobal){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # from fold = 1 to number_labes
    f = 1
    apagaGlobal <- foreach (f = 1:number_folds) %dopar%{
      cat("\nFold  ", f)
      FolderSplit = paste(FolderGlobal, "/Split-", f, sep="")
      setwd(FolderSplit)
      unlink("ResConfMat.csv", recursive = TRUE)
      gc()
    }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO DELETE                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION CLUS GLOBAL                                                                           #
#   Objective                                                                                    #
#       Tests and evaluate the globl partitions                                                  #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderGlobal path of hybrid partition results                                            #
#   Return                                                                                       #
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusGlobal <- function(ds, dataset_name, number_folds, FolderRD, FolderConfigFiles,FolderGlobal, FolderReports){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Tests each Splits Globaly                                                        #")
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Joins the configuration, training and test files for running the clus             #")
  timeGatherFiles = system.time(gatherFilesFoldsGlobal(ds, dataset_name, number_folds, FolderConfigFiles, FolderGlobal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Execute CLUS GLOBAL                                                               #")
  timeClusGlobal = system.time(executeClusGlobal(ds, dataset_name, number_folds, FolderGlobal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Splits the real outputs and the predicted outputs                                 #")
  timeGatherPreds = system.time(gatherPredictsGlobal(ds, dataset_name, number_folds, FolderGlobal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Evaluates Global Partitions                                                       #")
  timeEvalGlobal = system.time(evaluateGlobal(ds, dataset_name, number_folds, FolderGlobal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Gather Evaluated Measures                                                         #")
  timeGE = system.time(gatherEvalGlobal(ds, dataset_name, number_folds, FolderGlobal, FolderReports))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Delete files                                                                      #")
  timeDel = system.time(deleteGlobal(ds, dataset_name, number_folds, FolderGlobal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: Save Runtime                                                                      #")
  RunTimeGlobal = rbind(timeGatherFiles, timeClusGlobal, timeGatherPreds, timeEvalGlobal, timeGE)
  setwd(FolderReports)
  write.csv(RunTimeGlobal, "RunTimeGlobal.csv")
  cat("\n##################################################################################################")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END!!!!                                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################