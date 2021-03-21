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
# Script 12 - Clus Random 2                                                                      #
##################################################################################################

#################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
shm = 0
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  shm = 1
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  shm = 0
}
shm = shm
setwd(FolderRoot)

# folder SCRIPTS
FolderScripts = paste(FolderRoot, "/scripts/", sep="")

# folder shm
FolderSHM = "/dev/shm/"



##################################################################################################
# FUNCTION GENERATED RANDOM PARTITIONS                                                           #
#   Objective                                                                                    #
#       generated random partitions                                                              #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       configuration partitions                                                                 #
##################################################################################################
generatedRandomPartitions2 <- function(ds, dataset_name, number_folds, namesLabels, FolderRandom){
  
  number_groups = c(0)
  fold = c(0)
  summary = data.frame(fold, number_groups)
  
  f = 1
  while(f<=number_folds){
    cat("\nFold = ", f)
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==TRUE){
    } else {
      dir.create(FolderSplit)
    }
    
    # Sorteia um número de grupos aleatoriamente
    num.grupos <- sample(2:(ds$Labels-1),1)
    number_groups = num.grupos
    fold = f
    summary = rbind(summary, data.frame(fold, number_groups))
  
    # Cria uma particao aleatória
    particao = sample(1:num.grupos, ds$Labels, replace = TRUE)
    
    # Garante que todos os números sao sorteados (dummies)
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
  cat("\n# CLUS RANDOM 2: END FUNCTION generated Random Partitions                                        #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}


##################################################################################################
# FUNCTION RANDOM PARTITIONS                                                                     #
#   Objective                                                                                    #
#       build and validate the random partitions                                                 #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#       FolderDSF: path folder 10-folds                                                          #
#   Return                                                                                       #
#       partitions mounted to test                                                               #
##################################################################################################
RandomPartitions2 <- function(ds, namesLabels, dataset_name, number_folds, FolderRandom, FolderDSF){
  
  diretorios <- directories(shm)
  
  FolderTr = paste(FolderDSF, "/Tr", sep="")
  dirFolderTr = c(dir(FolderTr))
  n_dirFolderTr = length(dirFolderTr)
  
  FolderTs = paste(FolderDSF, "/Ts", sep="")
  dirFolderTs = c(dir(FolderTs))
  n_dirFolderTs = length(dirFolderTs) 
  
  setwd(FolderRandom)
  summaryPartitions = data.frame(read.csv("summary_partitions.csv"))
  summaryPartitions = summaryPartitions[,-1]
  
  fold = c()
  silho = c()
  qualidadeSilho = data.frame(fold, silho)
  
  f = 1  
  randomP <- foreach(f = 1:number_folds) %dopar%{  
    
    cat("\nFold: ", f)
    
    ############################################################################################################
    #cat("\nLOAD LIBRARY \n")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    ############################################################################################################
    #cat("\nSET WORKSPACE \n")
    sistema = c(Sys.info())
    shm = 0
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
      shm = 1
    } else {
      FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
      shm = 0
    }
    shm = shm
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
    #cat("\nLOAD FUNCTION CONVERT ARFF \n")
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
    #cat("\nTOTAL GROUPS < > 1\n")
    #cat("\nSTART MOUNT GROUPS OF LABELS FOR EACH PARTITION\n")
    
    ng = summaryPartitions[f,]$number_groups
    
    ####################################################################################
    grupoSilhuetaTR = data.frame()
    grupoSilhuetaTS = data.frame()
    
    # DO GRUPO 1 ATÉ O ÚLTIMO GRUPO DA PARTIÇÃO
    g = 1
    while(g<=ng){
        
      setwd(FolderSplit)
        
      ############################################################################################################
      #cat("\nLOAD LIBRARY \n")
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
      #cat("\n\n\t", FolderGroup, "\n\n")
      setwd(FolderGroup)
      
      ####################################################################################
      a = partition3 %>% filter(., partition3$num.grupo == g)
      
      ####################################################################################
      nome_grupo = paste("grupo_", g, sep="")           
      grSpecThisPart = a
      totalLabelsThisGr = nrow(a)
      
      cat("\nTrain File")
      ####################################################################################
      #cat("\nTRAIN: Get the original file\n")
      setwd(FolderTr)
      nomeTr2 = paste(FolderTr, "/", nomeTr, sep="")
      
      ####################################################################################
      #cat("\nTRAIN: MOUNT GROUP\n")
      arquivoTr = data.frame(read.csv(nomeTr2), stringsAsFactors = F)
      atributosTr = arquivoTr[ds$AttStart:ds$AttEnd]
      rotulosTr = toString(grSpecThisPart$names.labels)
      classesTr = select(arquivoTr, grSpecThisPart$names.labels)
      thisGroupTr = cbind(atributosTr, classesTr)
      
      ####################################################################################
      # grupo silhueta
      esteGrupoTR = cbind(clusters = g, data.frame(t(classesTr)))
      grupoSilhuetaTR = rbind(grupoSilhuetaTR, esteGrupoTR)
      
      ####################################################################################
      #cat("\nTRAIN: Save CSV\n")
      nomeCsTr = paste("grupo_Tr_", g, ".csv", sep="")
      nomeArTr = paste("grupo_Tr_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTr, nomeCsTr, row.names = FALSE)
      
      ####################################################################################
      #cat("\nTRAIN: Start End Targets\n")
      inicio = ds$LabelStart
      fim = ncol(thisGroupTr)
      ifr = data.frame(inicio, fim)
      setwd(FolderGroup)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      ####################################################################################
      #cat("\nTRAIN: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1 = paste(FolderGroup, "/", nomeCsTr, sep="")
      arg2 = paste(FolderGroup, "/", nomeArTr, sep="")
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      #cat("\nTRAIN: Verify and correct {0} and {1}\n")
      arquivo = paste(FolderGroup, "/", nomeArTr, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")        
      print(system(str0))
      cat("\n\n")		
      
      cat("\nTest File")
      ####################################################################################
      #cat("\nTest: Get the file\n")
      setwd(FolderTs)
      nomeTs2 = paste(FolderTs, "/", nomeTs, sep="")
      
      ####################################################################################
      #cat("\nTEST: MOUNT GROUP\n")
      arquivoTs = data.frame(read.csv(nomeTs2), stringsAsFactors = F)
      atributosTs = arquivoTs[ds$AttStart:ds$AttEnd]
      rotulosTs = toString(grSpecThisPart$names.labels)
      classesTs = select(arquivoTs, grSpecThisPart$names.labels)
      thisGroupTs = cbind(atributosTs, classesTs)
      
      ####################################################################################
      # grupo silhueta
      esteGrupoTs = cbind(clusters = g, data.frame(t(classesTs)))
      grupoSilhuetaTS = rbind(grupoSilhuetaTS, esteGrupoTs)
      
      ####################################################################################
      #cat("\nTEST: Save CSV\n")
      nomeCsTs = paste("grupo_Ts_", g, ".csv", sep="")
      nomeArTs = paste("grupo_Ts_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTs, nomeCsTs, row.names = FALSE)
      
      ####################################################################################
      #cat("\nTEST: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1 = paste(FolderGroup, "/", nomeCsTs, sep="")
      arg2 = paste(FolderGroup, "/", nomeArTs, sep="")
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      #cat("\nVALIDATION: Verify and correct {0} and {1}\n")
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
      cat("\nHeuristic = VarianceReduction")
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
      #cat("\nOpen inicioFimRotulos.csv\n")
      targets = data.frame(read.csv("inicioFimRotulos.csv"))
      
      ####################################################################################
      #cat("\nOpen predictions\n")
      setwd(FolderGroup)
      namae2 = paste(FolderGroup, "/", nome_grupo, ".test.pred.arff", sep="")
      #cat("\n\tNOME DO ARQUIVO:", namae2, "\n")
      
      if(file.exists(namae2)==TRUE){
        #cat("\nO ARQUIVO EXISTE\n")
      } else {
        #cat("\nO ARQUIVO NÃO EXISTE\n")
      }
      
      predicoes = data.frame(read.arff(namae2), use_xml = FALSE)
      
      cat("\n\n")
      print(predicoes)
      cat("\n\n")
      
      ####################################################################################
      #cat("\nSPLIT PREDICTIS\n")
      if(targets$inicio == targets$fim){
        #cat("\nOnly one label in this group\n")
        
        ####################################################################################
        #cat("\nSave Y_true\n")
        setwd(FolderGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        #cat("\nSave Y_true\n")
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
        #cat("\nMore than one label in this group\n")
        comeco = 1+(targets$fim - targets$inicio)
        
        ####################################################################################
        #cat("\nSave Y_true\n")
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        #cat("\nSave Y_true\n")
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
    
    #setwd(FolderSplit)
    #library(cluster)
    #a = dist(grupoSilhuetaTR)
    #b = as.dist(a)
    #sil = silhouette(grupoSilhuetaTR$clusters, dist(grupoSilhuetaTR))
    #sil = sortSilhouette(sil)
    #write.csv(sil, paste("silhueta-p-", f, ".csv", sep=""))
    
    #pdf(paste("sil-p-", f, ".pdf", sep=""), width = 10, height = 8)
    #plot(sil)
    #dev.off()
    #cat("\n")     
    
    # Summary of silhouette analysis
    #si.sum = summary(sil)
    #print(si.sum)
    
    # Average silhouette width of each cluster
    #print(si.sum$clus.avg.widths)
    
    # The size of each clusters
    #print(si.sum$clus.sizes)
    
    # The total average (mean of all individual silhouette widths)
    #avgTotal = si.sum$avg.width
    
    #fold = f
    #silho = avgTotal
    #qualidadeSilho = rbind(qualidadeSilho, data.frame(fold, silho))
    
    #library("factoextra")
    #pdf(paste("fviz-sil-p-", f, ".pdf", sep=""), width = 10, height = 8)
    #print(fviz_silhouette(sil))
    #dev.off()
    cat("\n")     
    
  }
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 2: END FUNCTION Random Partitions                                                  #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}


##################################################################################################
# FUNCTION GATHER PREDICTS RANDOM                                                                #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gatherPredsRandom2 <- function(ds, dataset_name, number_folds, FolderRandom){
  
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
    
    cat("\nFold: ", f)
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)   
    
      
      ############################################################################################################
      #cat("\nLOAD LIBRARY \n")
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
        #cat("\nLOAD LIBRARY \n")
        library("stringr")
        library("AggregateR")    
        library("plyr")
        library("dplyr")
        library("mldr")
        library("utiml")
        library("foreign")
        
        cat("\n\tGrupo: ", g)
        
        FolderGroup = paste(FolderSplit, "/Group-", g, sep="")
        setwd(FolderGroup)
        
        #cat("\nGather y_true ", g, "\n")
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)
        
        #cat("\nGather y_predict ", g, "\n")
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)
        
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)
        unlink("inicioFimRotulos.csv", recursive = TRUE)
      
      g = g + 1
      gc()
    }
      
      #cat("\nSave files ", g, "\n")
      setwd(FolderSplit)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      
  }
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 2: END FUNCTION gather Preds Random                                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}



##################################################################################################
# FUNCTION EVALUATION RANDOM                                                                     #
#   Objective                                                                                    #
#      Evaluates the random partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       Assessment measures for each random partition                                            #
##################################################################################################
evalRandom2 <- function(ds, dataset_name, number_folds, FolderRandom, FolderReports){
  
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  confMatFinal = data.frame(medidas)
  folds = c("")
  
  f = 1
  while(f<=number_folds){  
    cat("\nFold: ", f)
    
    FolderSplit = paste(FolderRandom, "/Split-", f, sep="")
    
    setwd(FolderSplit)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
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
    
    #cat("\n\t\tSave Measures this fold")
    setwd(FolderSplit)
    write.csv(confMatPart, namae)  
    
    confMatFinal = cbind(confMatFinal, confMatPart)
    
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  }
  
  #cat("\n\t\tSave all measures")
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
  
  setwd(FolderReports)
  write.csv(sumary3, paste(dataset_name, "-BestF1Macro-R2.csv", sep=""), row.names = FALSE)
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 2: END OF FUNCTION EVALUATE                                                        #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


deleteAllR2 <- function(number_folds, FolderRandom2){
  
  setwd(FolderRandom2)
  sumario = data.frame(read.csv("summary_partitions.csv"))
  
  f = 1
  apagaRandom2 <- foreach (f = 1:number_folds) %dopar%{
    
    cat("Fold: ", f)
    
    FolderSplit = paste(FolderRandom2, "/Split-", f, sep="")
    setwd(FolderSplit)
    unlink("EvaluatedPartition.csv")
    
    sumario2 = sumario[f,]
    num.group = sumario2$number_groups
    
    g = 1
    while(g<=num.group){
      
      cat("Group: ", g)
      
      FolderG = paste(FolderSplit, "/Group-", g, sep="")
      setwd(FolderG)
      
      nome1 = paste("grupo_", g, ".test.pred.arff", sep="")
      unlink(nome1)
      
      nome2 = paste("grupo_", g, ".s", sep="")
      unlink(nome2)
      
      g = g + 1
      gc()
    }
    
    gc()
  }
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 2: END OF DELETE                                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION CLUS RANDOM 2                                                                         #
#   Objective                                                                                    #
#       validate and test the random partitions                                                  #
#   Parameters                                                                                   #
#       ds
#       namesLabels
#       dataset_name
#       FolderDSF
#       FolderResDataset
#       FolderRandom
#       number_folds
#  Return
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusRandom_2 <- function(ds, namesLabels, dataset_name, number_folds, FolderDSF, FolderResDataset, FolderRandom, FolderReports){
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2                                                                                  #")
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: generated random partitions                                                     #")  
  timeSplit = system.time(generatedRandomPartitions2(ds, dataset_name, number_folds, namesLabels, FolderRandom))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: build and validate partitions                                                   #")  
  timeBuild = system.time(RandomPartitions2(ds, namesLabels, dataset_name, number_folds, FolderRandom, FolderDSF))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: gather predictions                                                              #")  
  timePreds = system.time(gatherPredsRandom2(ds, dataset_name, number_folds, FolderRandom))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: eval random partitions 2                                                        #")  
  timeEval = system.time(evalRandom2(ds, dataset_name, number_folds, FolderRandom, FolderReports))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: delete                                                                          #")
  timeDel = system.time(deleteAllR2(number_folds, FolderRandom))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: Save Runtime                                                                    #")
  RunTimeRandom = rbind(timeSplit, timeBuild, timePreds, timeEval, timeDel)
  
  setwd(FolderReports)
  write.csv(RunTimeRandom, "RunTimeRandom2.csv")
  cat("\n##################################################################################################")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 2: END!!!!                                                                         #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################