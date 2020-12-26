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
# Script 9 - Clus Random                                                                         #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()

generatedRandomPartitions2 <- function(ds, namesLabels, dataset_name,
                                      FolderRandom, number_folds){
  
  number_groups = c(0)
  fold = c(0)
  summary = data.frame(fold, number_groups)
  
  cat("\nCreating Random Partitions")
  f = 1
  #randomPart <- foreach(f = 1:number_folds) %dopar%{  
  while(f<=number_folds){
    cat("\n\n\nFold = ", f)
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==TRUE){
    } else {
      dir.create(FolderSplit)
    }
    
    # Sorteia um número de grupos aleatoriamente
    #num.grupos <- sample(2:(num.labels-1),1)
    num.grupos <- sample(2:(ds$Labels-1),1)
    number_groups = num.grupos
    fold = f
    summary = rbind(summary, data.frame(fold, number_groups))
  
    # Cria uma particao aleatória
    #particao <- sample(1:num.grupos,num.labels,replace=TRUE)
    particao = sample(1:num.grupos, ds$Labels, replace = TRUE)
    
    # Garante que todos os números sao sorteados (dummies)
    #while(sum(1:num.grupos %in% particao) != num.grupos){
    #  particao <- sample(1:num.grupos,num.labels,replace=TRUE)
    #}
    while(sum(1:num.grupos %in% particao) != num.grupos){
      particao <- sample(1:num.grupos, ds$Labels, replace = TRUE)
    }
    
    # Ordena as partições e retorna os índices
    particao.sorted <- sort(particao,index.return=TRUE)
    
    # TRANSFORMA EM UM DATA FRAME
    particao2 = data.frame(particao.sorted)
    
    # ALTERA OS NOMES DAS COLUNAS DO DATA FRAME
    names(particao2) = c("num.grupo","num.rotulo")
    
    # COLOCA A PARTIÇÃO EM EM ORDEM ALFABÉTICA
    particao3 = particao2[order(particao2$num.grupo, decreasing = FALSE), ] 
    
    ordem.labels = sort(namesLabels, index.return = TRUE)
    
    rotulos = data.frame(ordem.labels)
    
    names(rotulos) = c("names.labels","index")
    
    # COLOCA OS RÓTULOS EM ORDEM ALFABÉTICA
    rotulos2 = rotulos[order(rotulos$index, decreasing = FALSE), ] 
    
    # ASSOCIA OS RÓTULOS COM OS ÍNDICES E OS GRUPOS
    fold = f
    pFinal = data.frame(cbind(fold, particao3, rotulos2))
    
    setwd(FolderSplit)
    write.csv(pFinal, "random_partition.csv")
    
    f = f + 1
    gc()
  }
  
  setwd(FolderRandom)
  write.csv(summary[-1,], "summary_partitions.csv")
  
  
  
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM: END FUNCTION generated Random Partitions                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}


##################################################################################################
# 
##################################################################################################
RandomPartitions2 <- function(ds, namesLabels, dataset_name, 
                             FolderRandom, FolderDSF, number_folds){
  
  cat("\n\n################################################################################################")
  cat("\n# START BUILD AND TEST RANDOM PARTITIONS                                                 #")
  cat("\n##################################################################################################")
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  FolderTr = paste(FolderDSF, "/Tr", sep="")
  dirFolderTr = c(dir(FolderTr))
  n_dirFolderTr = length(dirFolderTr)
  
  FolderTs = paste(FolderDSF, "/Ts", sep="")
  dirFolderTs = c(dir(FolderTs))
  n_dirFolderTs = length(dirFolderTs) 
  
  setwd(FolderRandom)
  summaryPartitions = data.frame(read.csv("summary_partitions.csv"))
  summaryPartitions = summaryPartitions[,-1]
  
  f = 1  
  randomP <- foreach(f = 1:number_folds) %dopar%{  
    
    cat("\n\n\nFOLD: ", f)
    
    ############################################################################################################
    cat("\nLOAD LIBRARY \n")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    ############################################################################################################
    cat("\nSET WORKSPACE \n")
    sistema = c(Sys.info())
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
      setwd(FolderRoot)
    } else {
      FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
      setwd(FolderRoot)
    }
    setwd(FolderRoot)
    #cat("\n\n\t", FolderRoot, "\n\n")
    
    ############################################################################################################
    setwd(FolderRoot)
    folderUtils = paste(FolderRoot, "/utils", sep="")
    dataset_name = dataset_name    
    #cat("\n\n\t", folderUtils, "\n\n")
    
    ############################################################################################################
    FolderScripts = paste(FolderRoot, "/scripts/", sep="")
    #cat("\n\n\t", FolderScripts, "\n\n")
    setwd(FolderScripts)
    
    ############################################################################################################
    cat("\nLOAD FUNCTION CONVERT ARFF \n")
    converteArff <- function(arg1, arg2, arg3){
      str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n")
    }
    
    ############################################################################################################
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    
    ###############################################################################
    setwd(FolderRoot)
    folderUtils = paste(FolderRoot, "/utils", sep="")
      
    ###################################################################################
    setwd(FolderSplit)
    partition = data.frame(read.csv("random_partition.csv"), stringsAsFactors = F)
    partition2 = partition[,-1]
    partition3 = partition2[order(partition2$num.grupo, decreasing = FALSE),]
 
    ####################################################################################
    nomeTr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    nomeTs = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")  
      
    ####################################################################################
    cat("\nTOTAL GROUPS < > 1\n")
    cat("\nSTART MOUNT GROUPS OF LABELS FOR EACH PARTITION\n")
    
    ng = summaryPartitions[f,]$number_groups
    
    # DO GRUPO 1 ATÉ O ÚLTIMO GRUPO DA PARTIÇÃO
    g = 1
    while(g<=ng){
        
      setwd(FolderSplit)
        
      ############################################################################################################
      cat("\nLOAD LIBRARY \n")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      library("mldr")
      library("utiml")
      library("foreign")
        
      ####################################################################################
      cat("\n\tGRUPO: ", g)
      
      ####################################################################################
      setwd(FolderRoot)
      folderUtils = paste(FolderRoot, "/utils", sep="")
      
      ####################################################################################
      nome_grupo = paste("grupo_", g, sep="")           
      nome_grupo_2 = paste("Group-", g, sep="")
      
      ####################################################################################
      nomeTr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeTs = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")   
      
      ####################################################################################
      FolderGroup = paste(FolderSplit, "/Group-", g, sep="")
      if(dir.exists(FolderGroup)==FALSE){
        dir.create(FolderGroup)
      }
      cat("\n\n\t", FolderGroup, "\n\n")
      setwd(FolderGroup)
      
      ####################################################################################
      a = partition3 %>% filter(., partition3$num.grupo == g)
      
      ####################################################################################
      nome_grupo = paste("grupo_", g, sep="")           
      grSpecThisPart = a
      totalLabelsThisGr = nrow(a)
      
      ####################################################################################
      cat("\nTRAIN: Get the original file\n")
      setwd(FolderTr)
      nomeTr2 = paste(FolderTr, "/", nomeTr, sep="")
      
      ####################################################################################
      cat("\nTRAIN: MOUNT GROUP\n")
      arquivoTr = data.frame(read.csv(nomeTr2), stringsAsFactors = F)
      atributosTr = arquivoTr[ds$AttStart:ds$AttEnd]
      rotulosTr = toString(grSpecThisPart$names.labels)
      classesTr = select(arquivoTr, grSpecThisPart$names.labels)
      thisGroupTr = cbind(atributosTr, classesTr)
      
      ####################################################################################
      cat("\nTRAIN: Save CSV\n")
      nomeCsTr = paste("grupo_Tr_", g, ".csv", sep="")
      nomeArTr = paste("grupo_Tr_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTr, nomeCsTr, row.names = FALSE)
      
      ####################################################################################
      cat("\nTRAIN: Start End Targets\n")
      inicio = ds$LabelStart
      fim = ncol(thisGroupTr)
      ifr = data.frame(inicio, fim)
      setwd(FolderGroup)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      ####################################################################################
      cat("\nTRAIN: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1 = paste(FolderGroup, "/", nomeCsTr, sep="")
      arg2 = paste(FolderGroup, "/", nomeArTr, sep="")
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      cat("\nTRAIN: Verify and correct {0} and {1}\n")
      arquivo = paste(FolderGroup, "/", nomeArTr, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")        
      print(system(str0))
      cat("\n\n")		
      
      ####################################################################################
      cat("\nTest: Get the file\n")
      setwd(FolderTs)
      nomeTs2 = paste(FolderTs, "/", nomeTs, sep="")
      
      ####################################################################################
      cat("\nTEST: MOUNT GROUP\n")
      arquivoTs = data.frame(read.csv(nomeTs2), stringsAsFactors = F)
      atributosTs = arquivoTs[ds$AttStart:ds$AttEnd]
      rotulosTs = toString(grSpecThisPart$names.labels)
      classesTs = select(arquivoTs, grSpecThisPart$names.labels)
      thisGroupTs = cbind(atributosTs, classesTs)
      
      ####################################################################################
      cat("\nTEST: Save CSV\n")
      nomeCsTs = paste("grupo_Ts_", g, ".csv", sep="")
      nomeArTs = paste("grupo_Ts_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTs, nomeCsTs, row.names = FALSE)
      
      ####################################################################################
      cat("\nTEST: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1 = paste(FolderGroup, "/", nomeCsTs, sep="")
      arg2 = paste(FolderGroup, "/", nomeArTs, sep="")
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      cat("\nVALIDATION: Verify and correct {0} and {1}\n")
      arquivo = paste(FolderGroup, "/", nomeArTs, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")        
      print(system(str0))
      cat("\n\n")				
      
      ####################################################################################
      cat("\nCreate config file clus\n")
      setwd(FolderGroup)
      nome_config = paste("grupo_", g, ".s", sep="")
      sink(nome_config, type = "output")
        
      cat("[General]")
      cat("\nCompatibility = MLJ08")
      
      cat("\n")
      cat("\n[Data]")
      nome_arquivo_2 = paste("grupo_Tr_", g, ".arff", sep="")
      cat(paste("\nFile = ", nome_arquivo_2, sep=""))
      nome_arquivo_3 = paste("grupo_Ts_", g, ".arff", sep="")
      cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
      
      cat("\n")
      cat("\n[Attributes]")
      cat("\nReduceMemoryNominalAttrs = yes")
      
      cat("\n")
      cat("\n[Attributes]")
      cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
      cat("\nWeights = 1")
      
      cat("\n")
      cat("\n[Tree]")
      cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
      
      cat("\n")
      cat("\n[Model]")
      cat("\nMinimalWeight = 5.0")
      
      cat("\n")
      cat("\n[Output]")
      cat("\nWritePredictions = {Test}")
      cat("\n")
      sink()
      
      ####################################################################################
      cat("\nExecute CLUS\n")
      nome_config2 = paste(FolderGroup, "/", nome_config, sep="")
      setwd(FolderGroup)      
      str = paste("java -jar ", folderUtils, "/Clus.jar ", nome_config2, sep="")        
      print(system(str))
      cat("\n\n")				
      
      ####################################################################################
      cat("\nOpen inicioFimRotulos.csv\n")
      targets = data.frame(read.csv("inicioFimRotulos.csv"))
      
      ####################################################################################
      cat("\nOpen predictions\n")
      setwd(FolderGroup)
      namae2 = paste(FolderGroup, "/", nome_grupo, ".test.pred.arff", sep="")
      cat("\n\tNOME DO ARQUIVO:", namae2, "\n")
      
      if(file.exists(namae2)==TRUE){
        cat("\nO ARQUIVO EXISTE\n")
      } else {
        cat("\nO ARQUIVO NÃO EXISTE\n")
      }
      
      predicoes = data.frame(read.arff(namae2), use_xml = FALSE)
      
      cat("\n\n")
      print(predicoes)
      cat("\n\n")
      
      ####################################################################################
      cat("\nSPLIT PREDICTIS\n")
      if(targets$inicio == targets$fim){
        cat("\nOnly one label in this group\n")
        
        ####################################################################################
        cat("\nSave Y_true\n")
        setwd(FolderGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        cat("\nSave Y_true\n")
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        ####################################################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        gc()
        
      } else {
        
        ####################################################################################
        cat("\nMore than one label in this group\n")
        comeco = 1+(targets$fim - targets$inicio)
        
        ####################################################################################
        cat("\nSave Y_true\n")
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        cat("\nSave Y_true\n")
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        t = 1 
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        gc()
      } # FIM DO ELSE
      
      nome1 = paste("grupo_Tr_", g, ".arff", sep="")
      nome2 = paste("grupo_Ts_", g, ".arff", sep="")
      nome3 = paste("grupo_Tr_", g, ".csv", sep="")
      nome4 = paste("grupo_Ts_", g, ".csv", sep="")
      nome5 = paste("grupo_", g, ".model", sep="")
      nome6 = paste("grupo_", g, ".s", sep="")
      
      setwd(FolderGroup)
      unlink(nome1, recursive = TRUE)
      unlink(nome2, recursive = TRUE)
      unlink(nome3, recursive = TRUE)
      unlink(nome4, recursive = TRUE)
      unlink(nome5, recursive = TRUE)
      unlink(nome6, recursive = TRUE)
      
      g = g + 1
      gc()
    
    } # FIM DO FOLD
  }
  cat("\n\n##################################################################################################")
  cat("\n# CLUS RANDOM: END FUNCTION Random Partitions                                                     #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}

##################################################################################################
# 
##################################################################################################
gatherPredsRandom2 <- function(ds, dataset_name, FolderRandom, number_folds){
  
  setwd(FolderRandom)
  summaryPartitions = data.frame(read.csv("summary_partitions.csv"))
  summaryPartitions = summaryPartitions[,-1]
  
  f = 1
  gphpParallel <- foreach(f = 1:number_folds) %dopar%{  
    
    ng = summaryPartitions[f,]$number_groups
    
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    cat("\n\nFold: ", f)
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)   
    
      
      ############################################################################################################
      cat("\nLOAD LIBRARY \n")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      library("mldr")
      library("utiml")
      library("foreign")
      
      g = 1
      while(g<=ng){
        
        ############################################################################################################
        cat("\nLOAD LIBRARY \n")
        library("stringr")
        library("AggregateR")    
        library("plyr")
        library("dplyr")
        library("mldr")
        library("utiml")
        library("foreign")
        
        cat("\n\tGrupo: ", g, "\n")
        
        FolderGroup = paste(FolderSplit, "/Group-", g, sep="")
        setwd(FolderGroup)
        
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
      setwd(FolderSplit)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      
  }
  gc()
  cat("\n\n##################################################################################################")
  cat("\n# CLUS RANDOM: END FUNCTION gather Preds Random                                                  #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}


##################################################################################################
# 
##################################################################################################
evalRandom2 <- function(ds, dataset_name, FolderRandom, number_folds){
  
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  confMatFinal = data.frame(medidas)
  folds = c("")
  
  f = 1
  while(f<=number_folds){  
    cat("\n\nFold: ", f, "\n")
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    
    setwd(FolderSplit)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    cat("\n\t\tSave Confusion Matrix")
    setwd(FolderSplit)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep="")
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep="")      
    namae = paste("EvaluatedPartition.csv", sep="")
    
    cat("\n\t\tSave Measures this fold")
    setwd(FolderSplit)
    write.csv(confMatPart, namae)  
    
    confMatFinal = cbind(confMatFinal, confMatPart)
    
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  }
  
  cat("\n\t\tSave all measures")
  setwd(FolderRandom)
  write.csv(confMatFinal, "FoldsEvaluated.csv", row.names = FALSE)
  
  confMatFinal2 = data.frame(t(confMatFinal))
  confMatFinal3 = confMatFinal2[-1,]
  colnames(confMatFinal3) = medidas
  teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  
  sumary = apply(teste,2,mean)
  sumary2 = data.frame(sumary)
  sumary3 = cbind(medidas, sumary2)
  names(sumary3) = c("Measures", "Summary")
  
  setwd(FolderRandom)
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  gc()
  cat("\n\n##################################################################################################")
  cat("\n# CLUS RANDOM: END OF FUNCTION EVALUATE                                                          #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



clusRandom_2 <- function(ds, namesLabels, dataset_name, FolderDSF, FolderResDataset,
                       FolderRandom, number_folds){
  
  cat("\nRANDOM: generated random partitions")  
  timeSplit = system.time(generatedRandomPartitions2(ds, namesLabels, 
                                              dataset_name, FolderRandom, number_folds))
  
  cat("\nRANDOM: build and validate partitions")  
  timeBuild = system.time(RandomPartitions2(ds, namesLabels, dataset_name, 
                                              FolderRandom, FolderDSF, number_folds))
  
  cat("\nRANDOM: gather predictions")  
  timePreds = system.time(gatherPredsRandom2(ds, dataset_name, FolderRandom, number_folds))
  
  cat("\nRANDOM: eval partitions")  
  timeEval = system.time(evalRandom2(ds, dataset_name, FolderRandom, number_folds))
  
  cat("\nRANDOM: Save Runtime")
  RunTimeRandom = rbind(timeSplit, timeBuild, timePreds, timeEval)
  
  setwd(FolderRandom)
  write.csv(RunTimeRandom, "RunTimeRandom.csv")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM: END!!!!                                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
}

