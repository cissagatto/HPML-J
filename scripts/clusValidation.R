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
# Script 7 - Clus Validation                                                                     #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()


##################################################################################################
# FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                                     #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gatherPredsHybPartVAL <- function(ds, dataset_name, number_folds, FolderHybPart){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # from fold 1 to number_folders
  f = 1
  gPHP3 <- foreach(f = 1:number_folds) %dopar%{  
 
    cat("\nFold: ", f)
    
    # folder split
    FolderSplitHyb = paste(FolderHybPart, "/Split-", f, sep="")
    #cat("\n\t\tFolder: ", FolderSplitHyb)
    
    # number of partitions
    num.part = ds$Labels-1
    
    # data frame
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)   
    
    # from partition 2 to partition = l- 2
    p = 2
    while(p<=num.part){
      
      cat("\n\tPartition: ", p)
      
      # specifying folder for the partition
      FolderPartHyb = paste(FolderSplitHyb, "/Partition-", p, sep="")
      
      # from group 1 to last group
      g = 1
      while(g<=p){
        
        cat("\n\tGroup: ", g)
        
        # specifying folder for the group
        FolderGroupHyb = paste(FolderPartHyb, "/Group-", g, sep="")
        
        #cat("\nGather y_true ", g, "\n")
        setwd(FolderGroupHyb)
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)
        
        #cat("\nGather y_predict ", g, "\n")
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)
        
        # deleting files
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)
        unlink("inicioFimRotulos.csv", recursive = TRUE)
        
        g = g + 1
        gc()
      } # end groups of the partition
      
      #cat("\nSave files ", g, "\n")
      setwd(FolderPartHyb)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      
      p = p + 1 
      gc()
    } # end partitions
    
    gc()
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION EVALUATION HYBRID PARTITIONS                                                          #
#   Objective                                                                                    #
#      Evaluates the hybrid partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
evalHybPartVAL <- function(ds, dataset_name, number_folds, FolderHybPart){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # data frame
  apagar = c(0)
  confMatPartitions = data.frame(apagar)
  partitions = c()
  
  # from fold = 1 to number_folder
  f = 1
  evalHybPartParallel <- foreach(f = 1:number_folds) %dopar%{  
    
    library("mldr")
    library("utiml")
    
    cat("\nFold: ", f)
    
    # specifyin folder for the fold
    FolderSplitHyb = paste(FolderHybPart, "/Split-", f, sep="")
    
    # number of partitions
    num.part = ds$Labels-1
    
    # from partition = 2 to parttion = l-2
    p = 2
    while(p<=num.part){
      
      cat("\n\tParticao: ", p)
      
      partitions[p] = paste("partitions-", p, sep="")
      
      # specifying folder for the partition
      FolderPartHyb = paste(FolderSplitHyb, "/Partition-", p, sep="")
      
      # get the true and predict lables
      setwd(FolderPartHyb)
      y_true = data.frame(read.csv("y_true.csv"))
      y_pred = data.frame(read.csv("y_predict.csv"))
      
      # compute measures multilabel
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
      
      #cat("\n\t\tSave Confusion Matrix")
      setwd(FolderPartHyb)
      salva3 = paste("Conf-Mat-Fold-", f, "-Partition-", p, ".txt", sep="")
      sink(file=salva3, type="output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()
      
      # creating a data frame
      confMatPart = multilabel_evaluate(confmat)
      confMatPart = data.frame(confMatPart)
      names(confMatPart) = paste("Partition-", p, sep="")      
      namae = paste("EvaluatedPartition.csv", sep="")
      
      #cat("\n\t\tSave Measures this partition")
      write.csv(confMatPart, namae)  
      
      # delete files
      setwd(FolderPartHyb)
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)
      
      p = p + 1
      gc()
    } # end partitions
    
    gc()
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION EVALUATION HYBRID PARTITIONS                                     #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER EVALUATIONS                                                                    #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for all folds                                                        #
##################################################################################################
gatherEvaluationVAL <- function(ds, dataset_name, number_folds, FolderHybPart){  
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # vector with names
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=number_folds){  
    
    cat("\nFold: ", f)
    
    # data frame
    apagar = c(0)
    avaliado2 = data.frame(apagar)
    partitions = c(0)
    
    # specifying folder for the fold
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    
    # number of partitions
    num.part = ds$Labels-1
    
    # from partition = 2 to partition = l- 2
    p = 2
    while(p<=num.part){
      
      cat("\n\tPartition: ", p)
      
      partitions[p] = paste("partition-", p, sep="")
      
      # specifying folder for the partition
      FolderPart = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPart)
      
      # save measures
      str = paste(FolderPart, "/EvaluatedPartition.csv", sep="")
      avaliado = data.frame(read.csv(str))
      names(avaliado)[1] = "medidas"
      avaliado3 = data.frame(avaliado[,-1])
      avaliado2 = cbind(avaliado2, avaliado3)
      names(avaliado2)[p] = partitions[p]
      
      p = p + 1
      gc()
    } # end partitions
    
    #cat("\nSAVE MEASURES")
    avaliado2$apagar = measures
    names(avaliado2)[1] = "measures"
    setwd(FolderSplit)
    write.csv(avaliado2, "EvalPartFold.csv", row.names = FALSE)
    
    f = f + 1
    gc() 
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION GATHEER EVALUATIONS                                              #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER F1 MACRO                                                                       #
#   Objective                                                                                    #
#       Get the partitions with the best macro-f1                                                #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Best macro-f1 partitions                                                                 #
##################################################################################################
gF1macroVAL <- function(ds, dataset_name, number_folds, FolderHybPart, FolderReports){
   
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # vector with measures names
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  # data frame
  split = c(0)
  name.part = c(0)
  num.part = c(0)
  macroF1 = c(0)
  F1MacroSummary = data.frame(split, name.part, num.part, macroF1)
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    # specifying folder for the fold
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    
    # number of partitions
    num.part = ds$Labels-1
    
    # save evaluation for this fold
    setwd(FolderSplit)
    avaliacoes = data.frame(read.csv("EvalPartFold.csv"))
    f1Macro = avaliacoes[8,]
    rownames(f1Macro) = "f1macro"
    f1Macro2 = f1Macro[,-1]
    f1Macro3 = t(f1Macro2)
    f1Macro4 = data.frame(f1Macro3[order(f1Macro3, decreasing = TRUE),])
    names(f1Macro4) = "F1macro"
    particao = c(rownames(f1Macro4))
    f1Macro5 = cbind(particao, f1Macro4)
    bestF1Macro = f1Macro5[1,]
    
    # update data frame
    split = f
    name.part = toString(bestF1Macro$particao)
    num.part = as.numeric(str_sub(name.part, start = 11))
    macroF1 = as.numeric(bestF1Macro$F1macro)
    F1MacroSummary = rbind(F1MacroSummary, data.frame(split, name.part, num.part, macroF1))
    
    f = f + 1 
    gc() 
  } # end folds
  
  # save the best f1 macro
  setwd(FolderHybPart)
  F1MacroSummary = F1MacroSummary[-1,]
  write.csv(F1MacroSummary, "BestF1Macro.csv", row.names = FALSE)
  
  setwd(FolderReports)
  write.csv(F1MacroSummary, paste(dataset_name, "-BestF1Macro-H.csv", sep=""), row.names = FALSE)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION GATHER F1 MACRO                                                  #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")  
}


##################################################################################################
# FUNCTION DELETE HYBRID PARTITION                                                               #
#   Objective                                                                                    #
#       deletes all unnecessary files                                                            #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
##################################################################################################
delHybPartVAL <- function(ds, number_folds, FolderHybPart){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # from fold = 1 to number_folds
  f = 1
  apagaVal <- foreach (f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    
    # set folder for the fold
    setwd(FolderHybPart) 
    
    # specifying folder
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    
    # number of partitions
    num.part = ds$Labels-1
    
    # from partition = 2 to partition = l - 2
    p = 2
    while(p<=num.part){
      cat("\n\tPartition: ", p)
      
      # set folder split
      setwd(FolderSplit)
      
      # specifying fold
      FolderPart = paste(FolderSplit, "/Partition-", p, sep="")
      
      # set folder partition
      setwd(FolderPart)
      
      # from group = 1 to last group
      g = 1
      while(g<=p){
        cat("\n\tGroup:", g)
        setwd(FolderPart)
        FolderGroup = paste(FolderPart, "/Group-", g, sep="")
        setwd(FolderGroup)
        nome7 = paste("SummaryPartition-", g, ".csv")
        unlink(nome7, recursive = TRUE)
        g = g + 1
        gc()
      } # end groups
      
      p = p + 1
      gc()
    } # end partitions
    
    # delete others
    setwd(FolderSplit)
    nome = paste(FolderSplit, "/Partition-", ds$Labels, sep="")
    unlink(nome, recursive = TRUE)
    
    gc()
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION DELETE HYBRID PARTITIONS                                         #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}




##################################################################################################
# FUNCTION CLUS VALIDATION                                                                       #
#   Objective                                                                                    #
#       validate and evaluate the hybrid partitions                                              #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderHybPart: path of hybrid partition validation                                       #
#   Return                                                                                       #
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusValidation <- function(ds, dataset_name, number_folds, FolderHybPart, FolderReports){
  
  # set folder
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  # call functions
  
  cat("\n################################################################################################")
  cat("\n# CLUS VALIDATION:                                                                             #")
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# Clus Validation: Gather the Predicts Hybrid Partition                                        #") 
  timeGPHP = system.time(gatherPredsHybPartVAL(ds, dataset_name, number_folds, FolderHybPart))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# Clus Validation: Evaluate Hybrid Partitions                                                  #")
  timeEHP = system.time(evalHybPartVAL(ds, dataset_name, number_folds, FolderHybPart))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# Clus Validation: Gather Evaluations Hybrid Partitions                                        #")
  timeGEHP = system.time(gatherEvaluationVAL(ds, dataset_name, number_folds, FolderHybPart))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# Clus Validation: Gather F1 Macro Hybrid Partitions                                           #")
  timeGF1 = system.time(gF1macroVAL(ds, dataset_name, number_folds, FolderHybPart, FolderReports))
  cat("\n################################################################################################")
  
  #cat("\n################################################################################################")
  #cat("\n# Clus Validation: Delete Files Hybrid Partitions                                              #")
  #timeDel = system.time(delHybPartVAL(ds, number_folds, FolderHybPart))
  #cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Save Runtime                                                                    #")
  RuntimeTest = rbind(timeGPHP, timeEHP, timeGEHP, timeGF1)
  
  setwd(FolderReports)
  write.csv(RuntimeTest, "RuntimeValidation.csv")
  cat("\n################################################################################################")
  
  gc()
  cat("\n################################################################################################")
  cat("\n# CLUS VALIDATION: END!!!                                                                      #") 
  cat("\n################################################################################################")
  cat("\n\n\n\n")
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################