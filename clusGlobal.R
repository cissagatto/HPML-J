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
# Script 9 - Clus Global                                                                         #
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
# FUNCTION CONFIG FILES CLUS                                                                     #
# Objective:                                                                                     #
#
# Parameters:                                                                                    #
#
# Return:                                                                                        #
#
##################################################################################################
configFilesClus <- function(ds, dataset_name, FolderConfigFiles){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  nome_config = c("")  
  setwd(FolderConfigFiles)
  
  # FOLD 1 TO 10
  j = 1
  filesParalel <- foreach (j = 1:10) %dopar%{
    
    cat("\nFold: ", j)    
    setwd(FolderConfigFiles)
    
    cat("\nCreate file config - ", j)
    nome_config[j] = paste(dataset_name, "-Split-", j , ".s", sep="")
    sink(nome_config[j], type = "output")
    
    cat("[General]")
    cat("\nCompatibility = MLJ08")
    
    cat("\n\n[Data]")
    nome_arquivo_2 = paste(dataset_name, "-Split-Tr-", j , ".arff", sep="")
    cat(paste("\nFile = ", nome_arquivo_2, sep=""))
    
    nome_arquivo_3 = paste(dataset_name, "-Split-Ts-", j , ".arff", sep="")
    cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
    
    cat("\n\n[Attributes]")
    cat("\nReduceMemoryNominalAttrs = yes")
    
    cat("\n\n[Attributes]")
    cat(paste("\nTarget = ", ds$LabelStart, "-", ds$LabelEnd, sep=""))
    cat("\nWeights = 1")
    
    cat("\n\n[Tree]")
    cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
    
    cat("\n\n[Model]")
    cat("\nMinimalWeight = 5.0")
    
    cat("\n\n[Output]")
    cat("\nWritePredictions = {Test}")
    cat("\n")
    sink()
    
    j = j + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO CONFIG FILES CLUS                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
#
##################################################################################################
gatherFilesFoldsGlobal <- function(ds, dataset_name, FolderConfigFiles, FolderGlobal){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  
  F10F = diretorios$folder10F
  FolderTr = paste(diretorios$folder10F, "/", dataset_name, "/Tr", sep="")
  FolderTs = paste(diretorios$folder10F, "/", dataset_name, "/Ts", sep="")   
  
  s = 1
  foldsParalel <- foreach(s = 1:10) %dopar% {
    cat("\nFold: ", s)
    
    FS = paste(FolderGlobal, "/Split-", s, sep="")
    if(dir.exists(FS)==TRUE){
      cat("\n\tFolderGlobal exists")
    } else {
      dir.create(FS)
    }
    
    nome_tr = paste(dataset_name, "-Split-Tr-", s, ".arff", sep="")
    nome_ts = paste(dataset_name, "-Split-Ts-", s, ".arff", sep="")
    nome_config = paste(dataset_name, "-Split-", s, ".s", sep="")
    
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
      cat("\n\tTransfer test", s)
    } else {
      cat("\n")
    }
    
    setwd(FolderConfigFiles)
    if(file.exists(nome_config) == TRUE){
      setwd(FolderConfigFiles)
      copia = paste(FolderConfigFiles, "/", nome_config, sep="")
      cola = paste(FS, "/", nome_config, sep="")
      file.copy(copia, cola, overwrite = TRUE)
      cat("\n\tTransfer config", s)
    } else {
      cat("\n")
    }    
    s = s + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO GATHER FILES FOLDS                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
executeClusGlobal <- function(ds, dataset_name, Folder){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()  
  folderUtils = paste(FolderRoot, "/Utils", sep="")
  
  i = 1
  clusGlobalParalel <- foreach(i = 1:10) %dopar% {
  #while(i<=10){
    library("RWeka")
    library("rJava")
    cat("\nFold: ", i)
    FolderSplit = paste(Folder, "/Split-", i, sep="")
    nome_config = paste(dataset_name, "-Split-", i, ".s", sep="")
    
    cat("\nExecute CLUS\n")
    setwd(FolderSplit)
    str = paste("java -jar ", folderUtils , "/Clus.jar ", nome_config, sep="")
    system(str)
    
    um = paste(dataset_name, "-Split-", i, ".model", sep="")
    dois = paste(dataset_name, "-Split-", i, ".s", sep="")
    tres = paste(dataset_name, "-Split-Tr-", i, ".arff", sep="")
    quatro = paste(dataset_name, "-Split-Ts-", i, ".arff", sep="")
    
    setwd(FolderSplit)
    unlink(um, recursive = TRUE)
    unlink(dois, recursive = TRUE)
    unlink(tres, recursive = TRUE)
    unlink(quatro, recursive = TRUE)
    
    cat("\n")
    i = i + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO EXECUTE CLUS                                                        #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
gatherPredictsGlobal <- function(ds, dataset_name, Folder){
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()   
  
  f = 1
  predGlobalParalel <- foreach(f = 1:10) %dopar% {    
    library("foreign")    
    cat("\nFold: ", f)    
    FolderSplit = paste(Folder, "/Split-", f, sep="")    
    cat("\n\tOpen Test.Pred.Arff ", f)
    setwd(FolderSplit)    
    nome = paste(dataset_name, "-Split-" , f, ".test.pred.arff", sep="")
    predicoes = data.frame(read.arff(nome))
    
    inicio = ds$LabelStart
    fim = ds$LabelEnd
    comeco = 1+(fim - inicio)
    
    cat("\n\tSave Y_true")
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
    
    cat("\n\tSave Y_pred")
    setwd(FolderSplit)
    pred = data.frame(predicoes[nomeColuna])
    names(pred) = rotulos
    write.csv(pred, "y_predict.csv", row.names = FALSE)  
    
    f = f + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO GATHER PREDICTS                                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
gatherEvalGlobal <- function(ds, dataset_name, Folder){
  
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
  while(f<=10){
    cat("\nFold: ", f)
    
    FolderSplit = paste(Folder, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv("ResConfMat.csv"))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    cat("\n\tDelete unecessary files")
    setwd(FolderSplit)
    unlink("ResConfMat", recursive = TRUE)
    
    folds[f] = paste("Fold-", f, sep="")
    
    setwd(FolderSplit)
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
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
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO GATHER EVALUATED                                                    #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
#
##################################################################################################
deleteGlobal <-function(ds, dataset_name, FolderGlobal){
  
  f = 1
  apagaGlobal <- foreach (f = 1:10) %dopar%{
  #while(f<=10){
    
    cat("\nFold  ", f)
    
    FolderSplit = paste(FolderGlobal, "/Split-", f, sep="")
    
    setwd(FolderSplit)
    unlink("ResConfMat.csv", recursive = TRUE)
    
    f = f + 1
    
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO DELETE                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
#
##################################################################################################
clusGlobal <- function(ds, dataset_name, FolderRD, FolderConfigFiles, FolderGlobal){
  
  cat("\nGLOBAL: Tests each Splits Globaly\n")
  
  cat("\nGLOBAL: Generating the CLUS configuration files for the splits of each dataset\n")
  timeConfigFiles = system.time(configFilesClus(ds, dataset_name, FolderConfigFiles))
  
  cat("\nGLOBAL: Joins the configuration, training and test files in a single folder for running the clus\n")
  timeGatherFiles = system.time(gatherFilesFoldsGlobal(ds, dataset_name, FolderConfigFiles, FolderGlobal))
  
  cat("\nGLOBAL: Execute CLUS\n")
  timeClusGlobal = system.time(executeClusGlobal(ds, dataset_name, FolderGlobal))
  
  cat("\nGLOBAL: Splits the real outputs and the predicted outputs\n")
  timeGatherPreds = system.time(gatherPredictsGlobal(ds, dataset_name, FolderGlobal))
  
  cat("\nGLOBAL: Evaluates the split classification\n")
  timeEvalGlobal = system.time(avalia(ds, dataset_name, FolderGlobal))
  
  cat("\nGLOBAL: Gather Evaluated Measures\n")
  timeGE = system.time(gatherEvalGlobal(ds, dataset_name, FolderGlobal))
  
  cat("\nGLOBAL: Delete files\n")
  timeDel = system.time(deleteGlobal(ds, dataset_name, FolderGlobal))
  
  cat("\nGLOBAL: Save Runtime\n")
  RunTimeGlobal = rbind(timeConfigFiles, timeGatherFiles, timeClusGlobal, timeGatherPreds, timeEvalGlobal, timeGE, timeDel)
  setwd(FolderGlobal)
  write.csv(RunTimeGlobal, "RunTimeGlobal.csv")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM!!!!                                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################