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
# Script 10 - Clus Local                                                                         #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()


##################################################################################################
# FUNCTION GATHER FILES FOLDS LOCAL                                                              #
#   Objective                                                                                    #
#       Joins training and test files in a single folder for running the clus                    #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderCV: folder path                                                                    #
#       FolderLocal: path of local partitions                                                    #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
gatherFilesFoldsLocal <- function(ds, dataset_name, number_folds, FolderCV, FolderLocal){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  #specifying folders
  F10F = diretorios$FolderCV
  FolderTr = paste(diretorios$folderFolds, "/", dataset_name, "/Tr", sep="")
  FolderTs = paste(diretorios$folderFolds, "/", dataset_name, "/Ts", sep="")   
    
  # from fold = 1 to number_folds
    s = 1
    gfflParalel <- foreach(s = 1:number_folds) %dopar% {
      
      cat("\nFold: ", s)
      
      # creating folder
      FS = paste(FolderLocal, "/Split-", s, sep="")
      if(dir.exists(FS)==TRUE){
        cat("\n\tFolder Local exists")
      } else {
        dir.create(FS)
      }
      
      nome_tr = paste(dataset_name, "-Split-Tr-", s, ".csv", sep="")
      nome_ts = paste(dataset_name, "-Split-Ts-", s, ".csv", sep="")
      
      # copying train files
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
      
      # copying test files
      setwd(FolderTs)
      if(file.exists(nome_ts) == TRUE){
        setwd(FolderTs)
        copia = paste(FolderTs, "/", nome_ts, sep="")
        cola = paste(FS, "/", nome_ts, sep="")
        file.copy(copia, cola, overwrite = TRUE)
        cat("\n\tTransfer Test", s)
      } else {
        cat("\n")
      }   
    }
    
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END OF TRANSFER FILES FUNCTION                                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION SPLIT LABELS                                                                          #
#   Objective                                                                                    #
#       For each label creates a dataset with that specific label                                #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       names_labels: label names                                                                #
#       FolderCV: folder path                                                                    #
#       FolderLocal: path of local partitions                                                    #
#   Return                                                                                       #
#       arff files for each label                                                                #
##################################################################################################
splitLabels <- function(ds, dataset_name, number_folds, namesLabels, FolderCV, FolderLocal){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  # specifying folder
  folderUtils = paste(FolderRoot, "/utils", sep="")
  
  namesLabels = namesLabels 
  
  # from fold = 1 to number_folds
    f = 1
    slParalel <- foreach (f = 1:number_folds) %dopar%{
      
      cat("\nFold: ", f)
      
      library("foreign")
      
      # specifying fold
      FolderSplit = paste(FolderLocal, "/Split-", f, sep="")
      
      nome_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nome_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
      
      # get train
      setwd(FolderSplit)
      treino = data.frame(read.csv(nome_tr))
      atributosTr = treino[ds$AttStart:ds$AttEnd]
      classesTr = treino[ds$LabelStart:ds$LabelEnd]
      
      # get teste
      teste = data.frame(read.csv(nome_ts))
      atributosTs = teste[ds$AttStart:ds$AttEnd]
      classesTs = teste[ds$LabelStart:ds$LabelEnd]
      
      # from label 1 to total labels
      j = 1
      while(j<=ds$Labels){
        
        library("foreign")
        
        cat("\n\tLabel [", namesLabels[j], "] \n")
        
        # get train
        classeTr = data.frame(classesTr[,namesLabels[j]])
        names(classeTr) = toString(namesLabels[j])
        thisGroupTr = cbind(atributosTr, classeTr) 
        
        # get test
        classeTs = data.frame(classesTs[,namesLabels[j]])
        names(classeTs) = toString(namesLabels[j])
        thisGroupTs = cbind(atributosTs, classeTs) 
        
        # specifying folder
        FolderLabel = paste(FolderSplit, "/", namesLabels[j], sep="")
        if(dir.exists(FolderLabel)==TRUE){
          cat("\n")
        } else {
          dir.create(FolderLabel)
        }
        
        cat("\n\tCreating Train File")
        #cat("\n\t[", namesLabels[j], "]: Save Train CSV\n")
        rotuloTr = paste(namesLabels[j], "-tr-", f, ".csv", sep="")
        setwd(FolderLabel)
        write.csv(thisGroupTr, rotuloTr, row.names = FALSE)
        
        #cat("\n\t[", namesLabels[j], "]: Convert TRAIN CSV to ARFF\n")
        setwd(FolderLabel)
        arg1Tr = rotuloTr
        arg2Tr = paste(namesLabels[j], "-tr-", f, ".arff", sep="")
        arg3Tr = paste(ncol(thisGroupTr), "-", ncol(thisGroupTr), sep="")
        str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1Tr , " ", arg2Tr, " ", arg3Tr , sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        #cat("\n\t[", namesLabels[j], "]: Verify and correct {0} and {1}\n")
        arquivo = paste(FolderLabel, "/", arg2Tr, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        cat("\n")
        print(system(str0))
        cat("\n")
        
        cat("\n\tCreating Test File")
        #cat("\n\t[", namesLabels[j], "]: Save Test CSV\n")
        rotuloTs = paste(namesLabels[j], "-ts-", f, ".csv", sep="")
        setwd(FolderLabel)
        write.csv(thisGroupTs, rotuloTs, row.names = FALSE)
        
        #cat("\n\t[", namesLabels[j], "]: Convert TEST CSV to ARFF\n")
        setwd(FolderLabel)
        arg1Ts = rotuloTs
        arg2Ts = paste(namesLabels[j], "-ts-", f, ".arff", sep="")
        arg3Ts = paste(ncol(thisGroupTs), "-", ncol(thisGroupTs), sep="")
        str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        #cat("\n\t[", namesLabels[j], "]: Verify and correct {0} and {1}\n")
        arquivo = paste(FolderLabel, "/", arg2Ts, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        cat("\n")
        print(system(str0))
        cat("\n")
        
        setwd(FolderLabel)
        cat("\n\t[", namesLabels[j], "]: Generated Config File\n")
        
        setwd(FolderLabel)
        nome_config = paste(namesLabels[j], "-", f , ".s", sep="")
        sink(nome_config, type = "output")
        
        cat("[General]")        
        cat("\nCompatibility = MLJ08")
        
        cat("\n\n[Data]")
        cat(paste("\nFile = ", arg2Tr, sep=""))
        
        cat(paste("\nTestSet = ", arg2Ts, sep=""))
        
        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")
        
        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", ncol(thisGroupTr), sep=""))
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
        
        cat("\n\tExecute CLUS")
        str = paste("java -jar ", folderUtils, "/Clus.jar ", nome_config, sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        # delete files
        cinco = paste(namesLabels[j], "-", f, ".s", sep="")
        seis = paste(namesLabels[j], "-", f, ".model", sep="")
        
        setwd(FolderLabel)
        unlink(nome_config, recursive = TRUE)
        unlink(rotuloTr, recursive = TRUE)
        unlink(rotuloTs, recursive = TRUE)
        #unlink(cinco, recursive = TRUE)
        unlink(seis, recursive = TRUE)
        unlink(arg2Tr, recursive = TRUE)
        unlink(arg2Ts, recursive = TRUE)
        
        j = j + 1
        gc()
      } # end labels
      
      # delete files
      setwd(FolderSplit)
      unlink(nome_tr, recursive = TRUE)
      unlink(nome_ts, recursive = TRUE)
      
      gc()
    } # end folds
    
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION SPLIT LABELS                                                          #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS LOCAL                                                                 #
#   Objective                                                                                    #
#       Splits the real outputs and the predicted outputs                                        #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       names_labels: label names                                                                #
#       FolderLocal: path of local partitions                                                    #
#   Return                                                                                       #
#       true labels and predict labels                                                           #
##################################################################################################
gatherLocalPredicts <- function(ds, dataset_name, number_folds, namesLabels, FolderLocal){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  # from fold = 1 to number_folds
    f = 1
    glpParalel <- foreach (f = 1:number_folds) %dopar%{
      
      library("foreign")
      
      cat("\nFold: ", f)             
      
      # specifying fold
      FolderSplit = paste(FolderLocal, "/Split-", f, sep="")    
      
      # data frame
      apagar = c(0)
      y_true = data.frame(apagar)
      y_pred = data.frame(apagar)    
      
      # from label 1 to total labels
      j = 1
      while(j<=ds$Labels){
        
        cat("\n\tLabel [", namesLabels[j], "] \n")      
        
        # specifying folder
        FolderLabel = paste(FolderSplit, "/", namesLabels[j], sep="")      
        
        # cat("\n\t[", namesLabels[j], "]: Open Test.Pred.Arff\n")
        setwd(FolderLabel)
        
        library("foreign")
        testPred1 = paste(FolderLabel, "/", namesLabels[j], "-" , f, ".test.pred.arff", sep="")
        testPred2 = data.frame(read.arff(testPred1))
        
        #cat("\n\t[", namesLabels[j], "]: Y TRUE\n")
        classes = data.frame(testPred2[,1])
        names(classes) = namesLabels[j]      
        
        #cat("\n\t[", namesLabels[j], "]: Y PRED\n")
        coluna = paste("Pruned.p.", namesLabels[j], sep="")
        pred = data.frame(testPred2[,coluna])
        names(pred) = namesLabels[j]      
        
        # put together
        y_true = cbind(y_true, classes)
        y_pred = cbind(y_pred, pred)      
        
        nome2 = paste(namesLabels[j], "-", j, ".test.pred.arff", sep="")
        unlink(nome2)
        
        j = j + 1
        gc()
      } # end labels
      
      #cat("\nSave Fold: ", f)
      setwd(FolderSplit)
      y_true = y_true[,-1]
      y_pred = y_pred[,-1]
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      
      gc()
    } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION GATGHER LOCAL PREDICTS                                                #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION EVALUATE LOCAL                                                                        #
#   Objective                                                                                    #
#       Evaluation the test                                                                      #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: path of local partition test                                                     #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
evaluateLocal <- function(ds, dataset_name, number_folds, Folder){  
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # data frame
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  # from fold = 1 to number_folds
    f = 1
    avaliaParalel <- foreach (f = 1:number_folds) %dopar%{    
    
      library("utiml")
      library("mldr")    
      
      cat("\nFold: ", f)    
      setwd(Folder)
      
      # specifying folder
      FolderSplit = paste(Folder, "/Split-", f, sep="")
      
      # get true and predict labels
      setwd(FolderSplit)
      y_pred = data.frame(read.csv("y_predict.csv"))
      y_true = data.frame(read.csv("y_true.csv"))
      
      # compute measures
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
      
      # save conf matx
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
      
      # delete files
      setwd(FolderSplit)
      unlink("y_predict.csv", recursive = TRUE)
      unlink("y_true.csv", recursive = TRUE)
      
      gc()
    } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION EVALUATE LOCAL CLUS                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER EVALUATION LOCAL PARTITIONS                                                    #
#   Objective                                                                                    #
#      Evaluates the local partitions                                                            #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: path of local partition results                                                  #
#       FolderReports: path to store results                                                     # 
#   Return                                                                                       #
#       Assessment measures for each local partition                                             #
##################################################################################################
gatherEvalLocal <- function(ds, dataset_name, number_folds, Folder, FolderReports){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # vector with names measures
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  # data frame
  confMatFinal = data.frame(medidas)
  
  folds = c("")
 
  # from fold = 1 to number_folds
  f = 1
  while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    # specifying folder
    FolderSplit = paste(Folder, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    #cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv("ResConfMat.csv"))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    folds[f] = paste("Fold-", f, sep="")
    
    # delete files
    setwd(FolderSplit)
    unlink("ResConfMat.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  } # end folds
  
  # save measures
  setwd(Folder)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, "FoldsEvaluated.csv", row.names = FALSE)
  
  # adjust
  confMatFinal2 = data.frame(t(confMatFinal))
  confMatFinal3 = confMatFinal2[-1,]
  colnames(confMatFinal3) = medidas
  teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  
  # sumary
  sumary = apply(teste,2,mean)
  sumary2 = data.frame(sumary)
  sumary3 = cbind(medidas, sumary2)
  names(sumary3) = c("Measures", "Summary")
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  # save
  setwd(FolderReports)
  write.csv(sumary3, paste(dataset_name, "-SummaryFoldsEvaluated-L.csv", sep=""), row.names = FALSE)
  
  gc()  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION GATHER EVALUATION LOCAL                                               #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION CLUS LOCAL                                                                            #
#   Objective                                                                                    #
#       Tests and evaluate the local partitions                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: name dataset                                                               #
#       number_folds: number of folds created                                                    #
#       namesLabels: names labels                                                                #
#       FolderCV: folder dataset                                                                 #
#       FolderLocal: path of local partition test                                                #
#       FolderReports: path to store results                                                     #
#   Return                                                                                       #
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusLocal <- function(ds, dataset_name, number_folds, namesLabels, FolderCV, FolderLocal, FolderReports){ 
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Tests each Splits Localy                                                           #")
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Joins training and test files in a single folder for running the clus              #")
  timeGatherFiles = system.time(gatherFilesFoldsLocal(ds, dataset_name, number_folds, FolderCV, FolderLocal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Split the labels                                                                   #")
  timeSplitLabels = system.time(splitLabels(ds, dataset_name, number_folds, namesLabels, FolderCV, FolderLocal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Splits the real outputs and the predicted outputs                                  #")
  timeGatherPreds = system.time(gatherLocalPredicts (ds, dataset_name, number_folds, namesLabels, FolderLocal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Evaluates Local Partition                                                          #")
  timeEvalLocal = system.time(evaluateLocal(ds, dataset_name, number_folds, FolderLocal))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Gather Evaluated Measures\n")
  timeGE = system.time(gatherEvalLocal(ds, dataset_name, number_folds, FolderLocal, FolderReports))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Save Runtime                                                                       #")
  RunTimeLocal = rbind(timeGatherFiles, timeSplitLabels, timeGatherPreds, timeEvalLocal, timeGE)
  
  setwd(FolderReports)
  write.csv(RunTimeLocal, "RunTimeLocal.csv")
  cat("\n##################################################################################################")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END!!                                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
