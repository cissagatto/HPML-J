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
gatherPredsHybPart <- function(ds, dataset_name, FolderHybPart, number_folds){
  
  f = 1
  gphpParallel <- foreach(f = 1:number_folds) %dopar%{  
    cat("\n\nSplit: ", f)
    FolderSplitHyb = paste(FolderHybPart, "/Split-", f, sep="")
    cat("\n\t\tFolder: ", FolderSplitHyb)
    num.part = ds$Labels-1
    
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)   
    
    p = 2
    while(p<=num.part){
      cat("\n\tParticao: ", p)
      FolderPartHyb = paste(FolderSplitHyb, "/Partition-", p, sep="")
      g = 1
      while(g<=p){
        cat("\n\t\tGrupo: ", g, "\n")
        
        FolderGroupHyb = paste(FolderPartHyb, "/Group-", g, sep="")
        
        setwd(FolderGroupHyb)
        cat("\nGather y_true ", g, "\n")
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)
        
        cat("\nGather y_predict ", g, "\n")
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)
        
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)
        unlink("inicioFimRotulos.csv", recursive = TRUE)
        
        g = g + 1
        gc()
      }
      
      cat("\nSave files ", g, "\n")
      setwd(FolderPartHyb)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      
      p = p + 1
      gc()
    }
    f = f + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                                     #
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
evalHybPart <- function(ds, dataset_name, FolderHybPart, number_folds){
  
  apagar = c(0)
  confMatPartitions = data.frame(apagar)
  partitions = c()
  
  f = 1
  evalHybPartParallel <- foreach(f = 1:number_folds) %dopar%{  
    cat("\n\nSplit: ", f)
    FolderSplitHyb = paste(FolderHybPart, "/Split-", f, sep="")
    num.part = ds$Labels-1
    
    p = 2
    while(p<=num.part){
      cat("\n\tParticao: ", p)
      
      partitions[p] = paste("partitions-", p, sep="")
      FolderPartHyb = paste(FolderSplitHyb, "/Partition-", p, sep="")
      
      setwd(FolderPartHyb)
      y_true = data.frame(read.csv("y_true.csv"))
      y_pred = data.frame(read.csv("y_predict.csv"))
      
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
      
      cat("\n\t\tSave Confusion Matrix")
      setwd(FolderPartHyb)
      salva3 = paste("Conf-Mat-Fold-", f, "-Partition-", p, ".txt", sep="")
      sink(file=salva3, type="output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()
      
      confMatPart = multilabel_evaluate(confmat)
      confMatPart = data.frame(confMatPart)
      names(confMatPart) = paste("Partition-", p, sep="")      
      namae = paste("EvaluatedPartition.csv", sep="")
      
      cat("\n\t\tSave Measures this partition")
      write.csv(confMatPart, namae)  
      
      setwd(FolderPartHyb)
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)
      
      p = p + 1
      gc()
    }
    f = f + 1
    gc()
  }
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
gatherEvaluations <- function(ds, dataset_name, FolderHybPart, number_folds){
  
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  f = 1
  gatheEVParel <- foreach(f = 1:number_folds) %dopar%{  
    cat("\n\nSplit: ", f)
    
    apagar = c(0)
    avaliado2 = data.frame(apagar)
    partitions = c(0)
    
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    num.part = ds$Labels-1
    
    p = 2
    while(p<=num.part){
      cat("\nPartition: ", p)
      partitions[p] = paste("partition-", p, sep="")
      
      FolderPart = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPart)
      
      avaliado = data.frame(read.csv("EvaluatedPartition.csv"))
      names(avaliado)[1] = "medidas"
      avaliado3 = data.frame(avaliado[,-1])
      avaliado2 = cbind(avaliado2, avaliado3)
      names(avaliado2)[p] = partitions[p]
      
      p = p + 1
      gc()
    }
    
    cat("\nSAVE MEASURES")
    avaliado2$apagar = measures
    names(avaliado2)[1] = "measures"
    setwd(FolderSplit)
    write.csv(avaliado2, "EvalPartFold.csv", row.names = FALSE)
    
    f = f + 1 # incrementa folds
    gc() # garbage collection
  } 
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
gatherF1macro <- function(ds, dataset_name, FolderHybPart, number_folds){
  
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  split = c(0)
  nome_particao = c(0)
  numero_particao = c(0)
  macroF1 = c(0)
  F1MacroSummary = data.frame(split, nome_particao, numero_particao, macroF1)
  
  f = 1
  while(f<=number_folds){
    cat("\n\nSplit: ", f)
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    num.part = ds$Labels-1
    
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
    
    split = f
    nome_particao = toString(bestF1Macro$particao)
    numero_particao = as.numeric(str_sub(nome_particao, start = 11))
    macroF1 = as.numeric(bestF1Macro$F1macro)
    F1MacroSummary = rbind(F1MacroSummary, data.frame(split, nome_particao, numero_particao, macroF1))
    
    f = f + 1 # incrementa folds
    gc() # garbage collection
  } 
  setwd(FolderHybPart)
  F1MacroSummary = F1MacroSummary[-1,]
  write.csv(F1MacroSummary, "BestF1Macro.csv", row.names = FALSE)
  
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
deleteHybPart <- function(ds, FolderHybPart, number_folds){
  
  f = 1
  apagaVal <- foreach (f = 1:number_folds) %dopar%{
    setwd(FolderHybPart) 
    FolderSplit = paste(FolderHybPart, "/Split-", f, sep="")
    num.part = ds$Labels-1
    
    p = 2
    while(p<=num.part){
      setwd(FolderSplit)
      FolderPart = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPart)
      g = 1
      while(g<=p){
        setwd(FolderPart)
        FolderGroup = paste(FolderPart, "/Group-", g, sep="")
        setwd(FolderGroup)
        nome7 = paste("SummaryPartition-", g, ".csv")
        unlink(nome7, recursive = TRUE)
        g = g + 1
        gc()
      }
      p = p + 1
      gc()
    }
    setwd(FolderSplit)
    nome = paste(FolderSplit, "/Partition-", ds$Labels, sep="")
    unlink(nome, recursive = TRUE)
    f = f + 1
    gc()
  }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS VALIDATION: END FUNCTION DELETE HYBRID PARTITIONS                                         #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################