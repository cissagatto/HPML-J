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
gatherFilesFoldsLocal <- function(ds, dataset_name, FolderCV, FolderLocal, number_folds){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  F10F = diretorios$FolderCV
  FolderTr = paste(diretorios$folderFolds, "/", dataset_name, "/Tr", sep="")
  FolderTs = paste(diretorios$folderFolds, "/", dataset_name, "/Ts", sep="")   
    
    s = 1
    gfflParalel <- foreach(s = 1:number_folds) %dopar% {
      cat("\nFold: ", s)
      
      FS = paste(FolderLocal, "/Split-", s, sep="")
      if(dir.exists(FS)==TRUE){
        cat("\n\tFolder Local exists")
      } else {
        dir.create(FS)
      }
      
      nome_tr = paste(dataset_name, "-Split-Tr-", s, ".csv", sep="")
      nome_ts = paste(dataset_name, "-Split-Ts-", s, ".csv", sep="")
      
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
#       names_labels: label names                                                                #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderCV: folder path                                                                    #
#       FolderLocal: path of local partitions                                                    #
#   Return                                                                                       #
#       arff files for each label                                                                #
##################################################################################################
splitLabels <- function(namesLabels, ds, dataset_name, FolderCV, FolderLocal, number_folds){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  folderUtils = paste(FolderRoot, "/utils", sep="")
  
  namesLabels = namesLabels 
  
    f = 1
    slParalel <- foreach (f = 1:number_folds) %dopar%{
      cat("\nFold: ", f)
      
      library("foreign")
      
      FolderSplit = paste(FolderLocal, "/Split-", f, sep="")
      
      nome_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nome_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
      
      setwd(FolderSplit)
      treino = data.frame(read.csv(nome_tr))
      atributosTr = treino[ds$AttStart:ds$AttEnd]
      classesTr = treino[ds$LabelStart:ds$LabelEnd]
      
      teste = data.frame(read.csv(nome_ts))
      atributosTs = teste[ds$AttStart:ds$AttEnd]
      classesTs = teste[ds$LabelStart:ds$LabelEnd]
      
      j = 1
      while(j<=ds$Labels){
        
        library("foreign")
        
        cat("\n\tLabel [", namesLabels[j], "] \n")
        
        classeTr = data.frame(classesTr[,namesLabels[j]])
        names(classeTr) = toString(namesLabels[j])
        thisGroupTr = cbind(atributosTr, classeTr) 
        
        classeTs = data.frame(classesTs[,namesLabels[j]])
        names(classeTs) = toString(namesLabels[j])
        thisGroupTs = cbind(atributosTs, classeTs) 
        
        FolderLabel = paste(FolderSplit, "/", namesLabels[j], sep="")
        if(dir.exists(FolderLabel)==TRUE){
          cat("\n")
        } else {
          dir.create(FolderLabel)
        }
        
        cat("\n\t[", namesLabels[j], "]: Save Train CSV\n")
        rotuloTr = paste(namesLabels[j], "-tr-", f, ".csv", sep="")
        setwd(FolderLabel)
        write.csv(thisGroupTr, rotuloTr, row.names = FALSE)
        
        cat("\n\t[", namesLabels[j], "]: Convert TRAIN CSV to ARFF\n")
        setwd(FolderLabel)
        arg1Tr = rotuloTr
        arg2Tr = paste(namesLabels[j], "-tr-", f, ".arff", sep="")
        arg3Tr = paste(ncol(thisGroupTr), "-", ncol(thisGroupTr), sep="")
        str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1Tr , " ", arg2Tr, " ", arg3Tr , sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        cat("\n\t[", namesLabels[j], "]: Verify and correct {0} and {1}\n")
        arquivo = paste(FolderLabel, "/", arg2Tr, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        cat("\n")
        print(system(str0))
        cat("\n")
        
        cat("\n\t[", namesLabels[j], "]: Save Test CSV\n")
        rotuloTs = paste(namesLabels[j], "-ts-", f, ".csv", sep="")
        setwd(FolderLabel)
        write.csv(thisGroupTs, rotuloTs, row.names = FALSE)
        
        cat("\n\t[", namesLabels[j], "]: Convert TEST CSV to ARFF\n")
        setwd(FolderLabel)
        arg1Ts = rotuloTs
        arg2Ts = paste(namesLabels[j], "-ts-", f, ".arff", sep="")
        arg3Ts = paste(ncol(thisGroupTs), "-", ncol(thisGroupTs), sep="")
        str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        cat("\n\t[", namesLabels[j], "]: Verify and correct {0} and {1}\n")
        arquivo = paste(FolderLabel, "/", arg2Ts, sep="")
        str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
        cat("\n")
        print(system(str0))
        cat("\n")
        
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
        
        cat("\n\n[Tree]")
        cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
        
        cat("\n\n[Model]")
        cat("\nMinimalWeight = 5.0")
        
        cat("\n\n[Output]")
        cat("\nWritePredictions = {Test}")
        cat("\n")
        sink()     
        
        cat("\n\tExecute CLUS\n")
        str = paste("java -jar ", folderUtils, "/Clus.jar ", nome_config, sep="")
        cat("\n")
        print(system(str))
        cat("\n")
        
        cinco = paste(namesLabels[j], "-", f, ".s", sep="")
        seis = paste(namesLabels[j], "-", f, ".model", sep="")
        
        setwd(FolderLabel)
        unlink(nome_config, recursive = TRUE)
        unlink(rotuloTr, recursive = TRUE)
        unlink(rotuloTs, recursive = TRUE)
        unlink(cinco, recursive = TRUE)
        unlink(seis, recursive = TRUE)
        unlink(arg2Tr, recursive = TRUE)
        unlink(arg2Ts, recursive = TRUE)
        
        j = j + 1
        gc()
      }
      
      setwd(FolderSplit)
      unlink(nome_tr, recursive = TRUE)
      unlink(nome_ts, recursive = TRUE)
      
      #f = f + 1
      gc()
    }
    
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
#       names_labels: label names                                                                #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderLocal: path of local partitions                                                    #
#   Return                                                                                       #
#       true labels and predict labels                                                           #
##################################################################################################
gatherLocalPredicts <- function(namesLabels, ds, dataset_name, FolderLocal, number_folds){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  
    f = 1
    glpParalel <- foreach (f = 1:number_folds) %dopar%{
      library("foreign")
      cat("\nFold: ", f)             
      FolderSplit = paste(FolderLocal, "/Split-", f, sep="")    
      apagar = c(0)
      y_true = data.frame(apagar)
      y_pred = data.frame(apagar)    
      j = 1
      while(j<=ds$Labels){
        cat("\n\tLabel [", namesLabels[j], "] \n")      
        
        FolderLabel = paste(FolderSplit, "/", namesLabels[j], sep="")      
        
        cat("\n\t[", namesLabels[j], "]: Open Test.Pred.Arff\n")
        setwd(FolderLabel)
        
        library("foreign")
        testPred1 = paste(FolderLabel, "/", namesLabels[j], "-" , f, ".test.pred.arff", sep="")
        testPred2 = data.frame(read.arff(testPred1))
        
        cat("\n\t[", namesLabels[j], "]: Y TRUE\n")
        classes = data.frame(testPred2[,1])
        names(classes) = namesLabels[j]      
        
        cat("\n\t[", namesLabels[j], "]: Y PRED\n")
        coluna = paste("Pruned.p.", namesLabels[j], sep="")
        pred = data.frame(testPred2[,coluna])
        names(pred) = namesLabels[j]      
        
        y_true = cbind(y_true, classes)
        y_pred = cbind(y_pred, pred)      
        j = j + 1
        gc()
      }
      
      cat("\nSave Fold: ", f)
      setwd(FolderSplit)
      y_true = y_true[,-1]
      y_pred = y_pred[,-1]
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      
      #f = f + 1
      gc()
    }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION GATGHER LOCAL PREDICTS                                                #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION AVALIA LOCAL                                                                          #
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
evaluateLocal <- function(ds, dataset_name, Folder, number_folds){  
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
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
      
      setwd(FolderSplit)
      unlink("y_predict.csv", recursive = TRUE)
      unlink("y_true.csv", recursive = TRUE)
      
      #f = f + 1
      gc()
    }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END FUNCTION EVALUATE LOCAL CLUS                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS LOCAL PARTITIONS                                                      #
#   Objective                                                                                    #
#      Evaluates the local partitions                                                            #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: path of local partition results                                                  #
#   Return                                                                                       #
#       Assessment measures for each local partition                                             #
##################################################################################################
gatherEvalLocal <- function(ds, dataset_name, Folder, number_folds){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  confMatFinal = data.frame(medidas)
  folds = c("")
 
  f = 1
  while(f<=number_folds){
    cat("\nFold: ", f)
    
    FolderSplit = paste(Folder, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv("ResConfMat.csv"))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    folds[f] = paste("Fold-", f, sep="")
    
    setwd(FolderSplit)
    unlink("ResConfMat.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  } # fim do while
  
  setwd(Folder)
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
  cat("\n# CLUS LOCAL: END FUNCTION GATHER EVALUATION LOCAL                                               #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION CLUS LOCAL                                                                            #
#   Objective                                                                                    #
#       Tests and evaluate the local partitions                                                  #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderCV: folder dataset                                                                 #
#       FolderLocal: path of local partition test                                                #
#   Return                                                                                       #
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusLocal <- function(ds, dataset_name, FolderCV, FolderLocal, namesLabels, number_folds){ 
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: Tests each Splits Localy                                                           #")
  cat("\n##################################################################################################")
  
  cat("\nCLUS LOCAL: Joins training and test files in a single folder for running the clus\n")
  timeGatherFiles = system.time(gatherFilesFoldsLocal(ds, dataset_name, FolderCV, FolderLocal, number_folds))
  
  cat("\nCLUS LOCAL: Split the labels\n")
  timeSplitLabels = system.time(splitLabels(namesLabels, ds, dataset_name, FolderCV, FolderLocal, number_folds))
  
  #cat("\nLOCAL: Execute CLUS\n")
  #timeClusLocal = system.time(executeClusLocal(ds, dataset_name, FolderLocal, namesLabels))
  
  cat("\nCLUS LOCAL: Splits the real outputs and the predicted outputs\n")
  timeGatherPreds = system.time(gatherLocalPredicts (namesLabels, ds, dataset_name, FolderLocal, number_folds))
  
  cat("\nCLUS LOCAL: Evaluates the split classification\n")
  timeEvalLocal = system.time(evaluateLocal(ds, dataset_name, FolderLocal, number_folds))
  
  cat("\nCLUS LOCAL: Gather Evaluated Measures\n")
  timeGE = system.time(gatherEvalLocal(ds, dataset_name, FolderLocal, number_folds))
  
  cat("\nCLUS LOCAL: Save Runtime\n")
  RunTimeLocal = rbind(timeGatherFiles, timeSplitLabels, timeGatherPreds, timeEvalLocal, timeGE)
  setwd(FolderLocal)
  write.csv(RunTimeLocal, "RunTimeLocal.csv")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS LOCAL: END!!                                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}