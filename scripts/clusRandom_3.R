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
# Script 13 - Clus Random 3                                                                      #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder
diretorios = directories()

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
generatedRandomPartitionsV3 <- function(ds, namesLabels, dataset_name, number_folds, FolderRandom3){
  
  num.particoes = ds$Labels - 1
  ordem.labels = sort(namesLabels, index.return = TRUE)
  rotulos = data.frame(ordem.labels)
  names(rotulos) = c("rotulos","indice")
  
  fold = c(0)
  part = c(0)
  groups = c(0)
  ng = data.frame(fold, part, groups)
  
  f = 1
  #randomPart <- foreach(f = 1:number_folds) %dopar%{  
  while(f<=number_folds){  
    cat("\nFold = ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    if(dir.exists(FolderVal)==TRUE){
    } else {
      dir.create(FolderVal)
    }
    
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==TRUE){
    } else {
      dir.create(FolderSplit)
    }
    
    p = 2
    while(p<=num.particoes){
      
      cat("\n\tPartition = ", p)
      
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      if(dir.exists(FolderPartition)==TRUE){
      } else {
        dir.create(FolderPartition)
      }
      
      # Sorteia um número de grupos aleatoriamente
      num.grupos <- sample(2:(ds$Labels-1),1)
      
      fold = f
      part = p
      groups = num.grupos
      ng = rbind(ng, data.frame(fold, part, groups))
      
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
      setwd(FolderPartition)
      #cat("\nSave partitions")
      write.csv(pFinal, paste("partition-", p, ".csv", sep=""))
      
      final2 = data.frame(cbind(fold, particao3[, 1, drop = FALSE], rotulos2[,1, drop = FALSE]))
      setwd(FolderPartition)
      write.csv(final2, "partition.csv")
      
      grupos.labels = vector("list", p)
      for(i in 1:p){
        grupos.labels[[i]] = pFinal$rotulos[pFinal$grupo == i]
      }
      
      g = 1
      while(g<=num.grupos){
        
        cat("\n\tGroup = ", g)
        
        FolderGroup = paste(FolderPartition, "/Group-", g, sep="")
        if(dir.exists(FolderGroup)==TRUE){
        } else {
          dir.create(FolderGroup)
        }
        
        g = g + 1
        gc()
      }
      
      p = p + 1
      gc()
    }
    
    f = f + 1
    gc()
    
  }
 
  setwd(FolderRandom3)
  write.csv(ng[-1,], "numero-grupos.csv")
  
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION generated Random Partitions                                        #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}




##################################################################################################
# FUNCTION RANDOM PARTITIONS VALIDATION                                                          #
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
RandomPartitionsV3 <- function(ds, namesLabels, dataset_name, number_folds, FolderRandom3, FolderDSF){

  num.particoes = ds$Labels - 1
  #cat("numero de partições: ", num.particoes)
  
  sf = setFolder()
  setwd(sf$Folder)
  FolderRoot = sf$Folder
  diretorios = directories()
  
  FolderTr = paste(FolderDSF, "/Tr", sep="")
  dirFolderTr = c(dir(FolderTr))
  n_dirFolderTr = length(dirFolderTr)
  
  FolderVl = paste(FolderDSF, "/Vl", sep="")
  dirFolderVl = c(dir(FolderVl))
  n_dirFolderVl = length(dirFolderVl) 
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  
  #cat("\n", FolderRoot)
  #cat("\n", FolderTr)
  #cat("\n", FolderVl)
  #cat("\n", FolderRoot)
  #print(grupos2)
  
  f = 1  
  randomP <- foreach(f = 1:number_folds) %dopar%{  
    
    fold = f
    
    cat("\nFOLD: ", f)
    
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
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
      setwd(FolderRoot)
    } else {
      FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
      setwd(FolderRoot)
    }
    setwd(FolderRoot)
    
    ############################################################################################################
    setwd(FolderRoot)
    folderUtils = paste(FolderRoot, "/utils", sep="")
    dataset_name = dataset_name    
    
    ############################################################################################################
    FolderScripts = paste(FolderRoot, "/scripts/", sep="")
    setwd(FolderScripts)
    
    ############################################################################################################
    #cat("\nLOAD FUNCTION CONVERT ARFF \n")
    converteArff <- function(arg1, arg2, arg3){
      str = paste("java -jar ", folderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n")
    }
    
    ############################################################################################################
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    
    ############################################################################################################
    #cat("\nGET THE GROUPS NUMBER\n")
    #a = grupos2 %>% filter(., grupos2$fold == f)
    #print(a)
    res_grupos1 = data.frame(grupos2 %>% filter(grupos2$fold == f))
    
    ############################################################################################################
    #cat("\nSTART MOUNT PARTITIONS\n")
    
    # DA PARTIÇÃO 2 ATÉ A ÚLTIMA PARTIÇÃO
    p = 2
    while(p<=num.particoes){
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
      
      ############################################################################################################
      cat("\n\tPartition: ", p)
      
      ############################################################################################################
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPartition)
      
      ###############################################################################
      setwd(FolderRoot)
      folderUtils = paste(FolderRoot, "/utils", sep="")
      
      ####################################################################################
      setwd(FolderPartition)
      partition = data.frame(read.csv(paste("partition-", p, ".csv", sep="")), stringsAsFactors = F)
      partition2 = partition[,-1]
      partition3 = partition2[order(partition2$num.grupo, decreasing = FALSE),]
      
      ####################################################################################
      nome_particao = paste("particao_", p, sep="")
      nome_particao_2 = paste("Partition-", p, sep="")
      
      ####################################################################################
      nomeTr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeVl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")  
      
      ####################################################################################
      #cat("\nTOTAL GROUPS < > 1\n")
      #cat("\nSTART MOUNT GROUPS OF LABELS FOR EACH PARTITION\n")
      
      grupos.labels = vector("list", p)
      
      #b = a %>% filter(., a$part == p)
      #print(b)
      #num.grupo = as.numeric(b$groups)
      
      res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == p))
      num.grupo = res_grupos2$groups
      
      # DO GRUPO 1 ATÉ O ÚLTIMO GRUPO DA PARTIÇÃO
      g = 1
      while(g<=num.grupo){
        
        setwd(FolderPartition)
        
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
        cat("\n\tGroup: ", g)
        
        ####################################################################################
        setwd(FolderRoot)
        folderUtils = paste(FolderRoot, "/utils", sep="")
        
        ####################################################################################
        nome_grupo = paste("grupo_", g, sep="")           
        nome_grupo_2 = paste("Group-", g, sep="")
        
        ####################################################################################
        nomeTr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
        nomeVl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")   
        
        ####################################################################################
        FolderGroup = paste(FolderPartition, "/Group-", g, sep="")
        #cat("\n\n\t", FolderGroup, "\n\n")
        setwd(FolderGroup)
        
        ####################################################################################
        res_part = partition3 %>% filter(partition3$num.grupo == g)
        
        ####################################################################################
        nome_grupo = paste("grupo_", g, sep="")           
        grSpecThisPart = res_part
        totalLabelsThisGr = nrow(res_part)
        
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
        
        cat("\nValidation File")
        ####################################################################################
        #cat("\nVALIDATION: Get the file\n")
        setwd(FolderVl)
        nomeVl2 = paste(FolderVl, "/", nomeVl, sep="")
        
        ####################################################################################
        #cat("\nVALIDATION: MOUNT GROUP\n")
        arquivoVl = data.frame(read.csv(nomeVl2), stringsAsFactors = F)
        atributosVl = arquivoVl[ds$AttStart:ds$AttEnd]
        rotulosVl = toString(grSpecThisPart$names.labels)
        classesVl = select(arquivoVl, grSpecThisPart$names.labels)
        thisGroupVl = cbind(atributosVl, classesVl)
        
        ####################################################################################
        #cat("\nVALIDATION: Save CSV\n")
        nomeCsVl = paste("grupo_Vl_", g, ".csv", sep="")
        nomeArVl = paste("grupo_Vl_", g, ".arff", sep="")
        setwd(FolderGroup)
        write.csv(thisGroupVl, nomeCsVl, row.names = FALSE)
        
        ####################################################################################
        #cat("\nVALIDATION: Convert CSV to ARFF\n")
        setwd(FolderGroup)
        arg1 = paste(FolderGroup, "/", nomeCsVl, sep="")
        arg2 = paste(FolderGroup, "/", nomeArVl, sep="")
        arg3 = paste(inicio, "-", fim, sep="")
        converteArff(arg1, arg2, arg3)
        
        ####################################################################################
        #cat("\nVALIDATION: Verify and correct {0} and {1}\n")
        arquivo = paste(FolderGroup, "/", nomeArVl, sep="")
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
        nome_arquivo_3 = paste("grupo_Vl_", g, ".arff", sep="")
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
          cat("\nOnly one label in this group\n")
          
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
          cat("\nMore than one label in this group\n")
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
        }
        
        nome1 = paste("grupo_Tr_", g, ".arff", sep="")
        nome2 = paste("grupo_Vl_", g, ".arff", sep="")
        nome3 = paste("grupo_Tr_", g, ".csv", sep="")
        nome4 = paste("grupo_Vl_", g, ".csv", sep="")
        nome5 = paste("grupo_", g, ".model", sep="")
        nome6 = paste("grupo_", g, ".s", sep="")
        
        setwd(FolderGroup)
        unlink(nome1, recursive = TRUE)
        unlink(nome2, recursive = TRUE)
        unlink(nome3, recursive = TRUE)
        unlink(nome4, recursive = TRUE)
        unlink(nome5, recursive = TRUE)
        #unlink(nome6, recursive = TRUE)
        
        g = g + 1
        gc()
      } 
      
      p = p + 1
      gc()
    } 
    
    gc()
    
  }
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION Random Partitions                                                  #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS RANDOM VALIDATION                                                     #
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
gatherPredsRandomV3 <- function(ds, dataset_name, number_folds, FolderRandom3){
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  
  f = 1
  gphpParallel <- foreach(f = 1:number_folds) %dopar%{  
    
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    cat("\nFold: ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    setwd(FolderVal)
    
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    num.part = ds$Labels-1
    
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar) 
    
    p = 2
    while(p<=num.part){
      
      ############################################################################################################
      #cat("\nLOAD LIBRARY \n")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      library("mldr")
      library("utiml")
      library("foreign")
      
      cat("\n\tPartition: ", p)
      
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPartition)
      
      res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == p))
      num.grupo = res_grupos2$groups
      
      g = 1
      while(g<=num.grupo){
        
        ############################################################################################################
        #cat("\nLOAD LIBRARY \n")
        library("stringr")
        library("AggregateR")    
        library("plyr")
        library("dplyr")
        library("mldr")
        library("utiml")
        library("foreign")
        
        cat("\n\tGrupo: ", g, "\n")
        
        FolderGroup = paste(FolderPartition, "/Group-", g, sep="")
        setwd(FolderGroup)
        
        #cat("\nGather y_true ", g, "\n")
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)
        
        #cat("\nGather y_predict ", g, "\n")
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)
        
        g = g + 1
        gc()
      }
      
      #cat("\nSave files ", g, "\n")
      setwd(FolderPartition)
      y_pred = y_pred[,-1]
      y_true = y_true[,-1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)
      
      p = p + 1
      gc()
    }
    
    gc()
  }
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION gather Preds Random                                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}


##################################################################################################
# FUNCTION EVALUATION RANDOM VALIDATION                                                          #
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
evalRandomV3 <- function(ds, dataset_name, number_folds, FolderRandom3){
  
  apagar = c(0)
  confMatPartitions = data.frame(apagar)
  partitions = c()
  
  f = 1
  evalParallel <- foreach(f = 1:number_folds) %dopar%{  
    
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    cat("\nFold: ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    
    num.part = ds$Labels-1
    
    p = 2
    while(p<=num.part){
      
      ############################################################################################################
      #cat("\nLOAD LIBRARY \n")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      library("mldr")
      library("utiml")
      library("foreign")
      
      cat("\n\tPartition: ", p)
      
      partitions[p] = paste("partitions-", p, sep="")
      
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      
      setwd(FolderPartition)
      y_true = data.frame(read.csv("y_true.csv"))
      y_pred = data.frame(read.csv("y_predict.csv"))
      
      y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
      y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
      
      #cat("\n\t\tSave Confusion Matrix")
      setwd(FolderPartition)
      salva3 = paste("Conf-Mat-Fold-", f, "-Partition-", p, ".txt", sep="")
      sink(file=salva3, type="output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()
      
      confMatPart = multilabel_evaluate(confmat)
      confMatPart = data.frame(confMatPart)
      names(confMatPart) = paste("Partition-", p, sep="")      
      namae = paste("EvaluatedPartition.csv", sep="")
      
      #cat("\n\t\tSave Measures this partition")
      write.csv(confMatPart, namae)  
      
      p = p + 1
      gc()
    }
    
    gc()
  }
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION eval Random                                                        #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")  
}


##################################################################################################
# FUNCTION GATHER EVALUATIONS RANDOM VALIDATION                                                  #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       Assessment measures for each random partition                                            #
##################################################################################################
gatherEvaluationsV3 <- function(ds, dataset_name, number_folds, FolderRandom3){
  
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  f = 1
  gevParel <- foreach(f = 1:number_folds) %dopar%{  
    #while(f<=number_folds){
    
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    cat("\nFold: ", f)
    
    apagar = c(0)
    avaliado2 = data.frame(apagar)
    partitions = c(0)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    
    num.part = ds$Labels-1
    
    p = 2
    while(p<=num.part){
      ############################################################################################################
      #cat("\nLOAD LIBRARY \n")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      library("mldr")
      library("utiml")
      library("foreign")
      
      cat("\nPartition: ", p)
      partitions[p] = paste("partition-", p, sep="")
      
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      
      setwd(FolderPartition)
      
      avaliado = data.frame(read.csv("EvaluatedPartition.csv"))
      names(avaliado)[1] = "medidas"
      avaliado3 = data.frame(avaliado[,-1])
      avaliado2 = cbind(avaliado2, avaliado3)
      names(avaliado2)[p] = partitions[p]
      
      p = p + 1
      gc()
    }
    
    #cat("\nSAVE MEASURES")
    avaliado2$apagar = measures
    names(avaliado2)[1] = "measures"
    setwd(FolderSplit)
    write.csv(avaliado2, "EvalPartFold.csv", row.names = FALSE)
    
    # f = f + 1 # incrementa folds
    gc() # garbage collection
  } 
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION gather Evaluations                                                 #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")  
  
}



##################################################################################################
# FUNCTION GATHER F1 MACRO VALIDATION                                                            #
#   Objective                                                                                    #
#       Get the partitions with the best macro-f1                                                #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       Best macro-f1 partitions                                                                 #
##################################################################################################
gatherF1macroV3 <- function(ds, dataset_name, number_folds, FolderRandom3, FolderReports){
  
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  split = c(0)
  name.part = c(0)
  num.part = c(0)
  macroF1 = c(0)
  F1MacroSummary = data.frame(split, name.part, num.part, macroF1)
  
  f = 1
  while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    
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
    name.part = toString(bestF1Macro$particao)
    num.part = as.numeric(str_sub(name.part, start = 11))
    macroF1 = as.numeric(bestF1Macro$F1macro)
    F1MacroSummary = rbind(F1MacroSummary, data.frame(split, name.part, num.part, macroF1))
    
    f = f + 1 
    gc() 
  } 
  setwd(FolderRandom3)
  F1MacroSummary = F1MacroSummary[-1,]
  write.csv(F1MacroSummary, "BestF1Macro.csv", row.names = FALSE)
  
  setwd(FolderReports)
  write.csv(F1MacroSummary, paste(dataset_name, "-BestF1Macro-R3.csv", sep=""), row.names = FALSE)
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END FUNCTION GATHER F1 MACRO                                                    #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")  
}



###############################################################################################
#                                                                                             #
#                                 TEST RANDOM PARTITIONS                                      #
#                                                                                             #
###############################################################################################

##################################################################################################
# FUNCTION MOUNT RANDOM PARTITIONS TEST                                                          #
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
mountRandomParT3 <- function(ds, dataset_name, number_folds, FolderDSF, FolderRandom3){
  
  FolderTr = paste(FolderDSF, "/Tr", sep="")
  FolderTs = paste(FolderDSF, "/Ts", sep="")
  
  #cat("\n", FolderTr)
  #cat("\n", FolderTs)
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  #print(grupos2)
  #cat("\n")
  
  f = 1
  mrParalel <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\nFold: ", f)
    
    ############################################################################################################
    library("RWeka")
    library("rJava")
    library("foreign")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    
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
    #print(FolderRoot)
    
    ############################################################################################################
    setwd(FolderRoot)
    FolderUtils = paste(FolderRoot, "/utils", sep="")
    dataset_name = dataset_name    
    #print(FolderUtils)
    
    ############################################################################################################
    FolderScripts = paste(FolderRoot, "/scripts/", sep="")
    setwd(FolderScripts)
    #print(FolderScripts)
    
    ############################################################################################################
    #cat("\nLOAD FUNCTION CONVERT ARFF \n")
    converteArff <- function(arg1, arg2, arg3){
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n")
    }
    
    ############################################################################################################
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    #print(FolderVal)
    
    FolderTest = paste(FolderRandom3, "/Test", sep="")
    #print(FolderTest)
    
    FolderSplit = paste(FolderTest, "/Split-", f, sep="")
    #print(FolderSplit)
    
    if(dir.exists(FolderTest)==TRUE){
    } else {
      dir.create(FolderTest)
    }
    
    if(dir.exists(FolderSplit)==TRUE){
    } else {
      dir.create(FolderSplit)
    }
    
    ############################################################################################################
    #cat("\n\tSelect Best Partition for: ", f, "\n")
    setwd(FolderRandom3)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    #print(bestPartition)
    bestPartition2 = bestPartition[f,]
    particaoEscolhida = bestPartition2$num.part
    #cat("\n\t\tChoose Partition:", particaoEscolhida, "\n")
    
    ############################################################################################################
    FolderPV = paste(FolderVal, "/Split-", f, "/Partition-", particaoEscolhida, sep="")
    #print(FolderPV)
    
    if(dir.exists(FolderPV)==TRUE){
    } else {
      dir.create(FolderPV)
    }
    
    FolderPT = paste(FolderSplit, "/Partition-", particaoEscolhida, sep="")
    #print(FolderPT)
    
    if(dir.exists(FolderPT)==TRUE){
    } else {
      dir.create(FolderPT)
    }
    
    ############################################################################################################
    #cat("\n\t Partition Configuration \n")
    setwd(FolderPV)
    configParticao = data.frame(read.csv("partition.csv"), stringsAsFactors = F)
    configParticao2 = configParticao[,-1]
    configParticao3 = configParticao2[order(configParticao2$num.grupo, decreasing = FALSE),]
    cat("\n ")
    print(configParticao3)
    cat("\n ")
    
    ############################################################################################################
    #cat("\nGET THE GROUPS NUMBER OF PARTITION\n")
    res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == particaoEscolhida))
    print(res_grupos2)
    num.grupo = res_grupos2$groups
    #cat("\n\tNumber of groups for this partition", num.grupo)
    
    ############################################################################################################
    #cat("\n\t\tOpen Train file ", f, "\n")
    setwd(FolderTr)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))
    
    ############################################################################################################
    #cat("\n\t\tOpen Test file ", f, "\n")
    setwd(FolderTs)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))
    
    cat("\n\tMount Groups of Labels for Fold ", f, "\n")
    
    # GRUPOS
    g = 1
    while(g<=num.grupo){
      
      library("RWeka")
      library("rJava")
      library("foreign")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      
      cat("\n\tGroup: ", g, "\n")
      
      ############################################################################################################
      cat("\n\tSpecific Group Contents: ", g, "\n")
      grupoEspecifico = data.frame(configParticao2 %>% filter(., configParticao2$num.grupo == g))  
      #print(grupoEspecifico)
      
      ############################################################################################################      
      FolderGT = paste(FolderPT, "/Group-", g, sep="")
      #print(FolderGT)
      
      if(dir.exists(FolderGT)==TRUE){
      } else {
        dir.create(FolderGT)
      }
      
      ############################################################################################################      
      #cat("\n\tTRAIN: Mount Group ", g, "\n")
      atributos_tr = arquivo_tr[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_tr)
      #rotulos_tr = toString(grupoEspecifico$names.labels)
      classes_tr = select(arquivo_tr, grupoEspecifico$names.labels)
      n_c = ncol(classes_tr)
      grupo_tr = cbind(atributos_tr, classes_tr)
      fim_tr = ncol(grupo_tr)
      
      ############################################################################################################      
      #cat("\n\tTRAIN: Save Group", g, "\n")
      setwd(FolderGT)
      nome_tr = paste(dataset_name, "-split-tr-", f, "-group-", g, ".csv", sep="")
      write.csv(grupo_tr, nome_tr, row.names = FALSE)
      
      ############################################################################################################      
      #cat("\n\tINICIO FIM TARGETS: ", g, "\n")
      inicio = ds$LabelStart
      fim = fim_tr
      ifr = data.frame(inicio, fim)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      ############################################################################################################      
      #cat("\n\tTRAIN: Convert Train CSV to ARFF ", g , "\n")
      nome_arquivo_2 = paste(dataset_name, "-split-tr-", f, "-group-", g, ".arff", sep="")
      setwd(FolderGT)
      arg1Tr = paste(FolderGT, "/", nome_tr, sep="")
      arg2Tr = paste(FolderGT, "/", nome_arquivo_2, sep="")
      arg3Tr = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1Tr, " ", arg2Tr, " ", arg3Tr, sep="")      
      print(system(str))
      cat("\n\n")			  
      
      ############################################################################################################      
      #cat("\n\tTRAIN: Verify and correct {0} and {1} ", g , "\n")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arg2Tr, sep="")      
      print(system(str0))
      cat("\n\n")			  
      
      ############################################################################################################      
      #cat("\n\tTEST: Mount Group: ", g, "\n")
      atributos_ts = arquivo_ts[ds$AttStart:ds$AttEnd]
      #rotulos_ts = toString(grupoEspecifico$names.labels)
      classes_ts = select(arquivo_ts, grupoEspecifico$names.labels)
      grupo_ts = cbind(atributos_ts, classes_ts)
      fim_ts = ncol(grupo_ts)
      #cat("\n\tTest Group Mounted: ", g, "\n")
      
      ############################################################################################################      
      #cat("\n\tTEST: Save Group ", g, "\n")
      setwd(FolderGT)
      nome_ts = paste(dataset_name, "-split-ts-", f, "-group-", g, ".csv", sep="")
      write.csv(grupo_ts, nome_ts, row.names = FALSE)
      
      ############################################################################################################      
      #cat("\n\tTEST: Convert CSV to ARFF ", g , "\n")
      nome_arquivo_3 = paste(dataset_name, "-split-ts-", f,"-group-", g, ".arff", sep="")
      setwd(FolderGT)
      arg1Ts = paste(FolderGT, "/", nome_ts, sep="")
      arg2Ts = paste(FolderGT, "/", nome_arquivo_3, sep="")
      arg3Ts = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
      print(system(str))         
      cat("\n\n")			  
      
      ############################################################################################################      
      #cat("\n\tTEST: Verify and correct {0} and {1} ", g , "\n")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arg2Ts, sep="")
      print(system(str0))	  
      cat("\n\n")	
      
      #####################################################################################################
      #cat("\nCreating .s file for clus")
      nome_arquivo_2 = paste("emotions-split-tr-", f , "-group-", g, ".arff", sep="")
      nome_arquivo_3 = paste("emotions-split-ts-", f , "-group-", g, ".arff", sep="")
      
      if(inicio == fim){
        setwd(FolderGT)
        nome_config = paste(dataset_name, "-split-", f, "-group-", g, ".s", sep="")
        sink(nome_config, type = "output")
        
        cat("[General]")        
        cat("\nCompatibility = MLJ08")
        
        cat("\n\n[Data]")
        cat(paste("\nFile = ", nome_arquivo_2, sep=""))
        cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
        
        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")
        
        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", fim, sep=""))
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
        
        cat("\nExecute CLUS: ", g , "\n")
        nome_config2 = paste(FolderGT, "/", nome_config, sep="")
        str = paste("java -jar ", FolderUtils, "/Clus.jar ", nome_config2, sep="")
        
        cat("\n")
        print(system(str))
        cat("\n")
        
      } else {
        setwd(FolderGT)
        nome_config = paste(dataset_name, "-split-", f, "-group-", g, ".s", sep="")
        sink(nome_config, type = "output")
        
        cat("[General]")        
        cat("\nCompatibility = MLJ08")
        
        cat("\n\n[Data]")
        cat(paste("\nFile = ", nome_arquivo_2, sep=""))
        cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
        
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
        
        cat("\nExecute CLUS: ", g)
        nome_config2 = paste(FolderGT, "/", nome_config, sep="")
        str = paste("java -jar ", FolderUtils, "/Clus.jar ", nome_config2, sep="")
        
        cat("\n")
        print(system(str))
        cat("\n")
      }
      
      ####################################################################################
      um = paste(dataset_name, "-split-", f, "-group-", g, ".model", sep="")
      dois = paste(dataset_name, "-split-", f, "-group-", g, ".s", sep="")
      tres = paste(dataset_name, "-split-tr-", f, "-group-", g, ".arff", sep="")
      quatro = paste(dataset_name, "-split-ts-", f, "-group-", g, ".arff", sep="")
      cinco = paste(dataset_name, "-split-tr-", f, "-group-", g, ".csv", sep="")
      seis = paste(dataset_name, "-split-ts-", f, "-group-", g, ".csv", sep="")
      
      setwd(FolderGT)
      unlink(um, recursive = TRUE)
      unlink(dois, recursive = TRUE)
      unlink(tres, recursive = TRUE)
      unlink(quatro, recursive = TRUE)
      unlink(cinco, recursive = TRUE)
      unlink(seis, recursive = TRUE)
      
      g = g + 1
      gc()
    }
    
    gc()
  } 
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END OF MOUNT RANDOM PARTITION FOR TEST ON CLUS                                 #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION SPLIT PREDICTS RANDOM TEST                                                            #
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
splitsPredictionsRandomT3 <- function(ds, dataset_name, number_folds, FolderDSF, FolderRandom3){
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  
  f = 1
  gspParalel <- foreach(f = 1:number_folds) %dopar% {
    
    library("RWeka")
    library("rJava")
    library("foreign")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    
    cat("\nFold: ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderTest = paste(FolderRandom3, "/Test", sep="")
    FolderSplit = paste(FolderTest, "/Split-", f, sep="")
    
    #cat("\nSelect Best Partition for", f, "\n")
    setwd(FolderRandom3)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    particaoEscolhida = bestPartition2$num.part
    #cat("\n\tChoose Partition:", particaoEscolhida, "\n")
    
    FolderPV = paste(FolderVal, "/Split-", f, "/Partition-", particaoEscolhida, sep="")
    FolderPT = paste(FolderSplit, "/Partition-", particaoEscolhida, sep="")
    
    
    ############################################################################################################
    #cat("\nGET THE GROUPS NUMBER OF PARTITION\n")
    res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == particaoEscolhida))
    num.grupo = res_grupos2$groups
    cat("\n\tNumber of groups for this partition", num.grupo)
    
    g = 1
    while(g<=num.grupo){
      
      library("RWeka")
      library("rJava")
      library("foreign")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      
      cat("\n\tGroup: ", g)
      
      FolderGT = paste(FolderPT, "/Group-", g, sep="")
      if(dir.exists(FolderGT)==TRUE){
      } else {
        dir.create(FolderGT)
      }
      
      setwd(FolderGT)
      nome = paste(dataset_name, "-split-", f, "-group-", g, ".test.pred.arff", sep="")
      predicoes = data.frame(read.arff(nome))
      
      ifr = data.frame(read.csv("inicioFimRotulos.csv"))
      inicio = ifr$inicio
      fim = ifr$fim
      
      if(inicio == fim){
        
        setwd(FolderGT)
        
        #cat("\nSave Y_true: ", g, "\n")
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        
        #cat("\nSave Y_pred: ", g, "\n")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
      } else {
        setwd(FolderGT)
        comeco = 1+(fim - inicio)
        
        #cat("\nSave Y_true: ", g, "\n")
        classes = data.frame(predicoes[,1:comeco])
        write.csv(classes, "y_true.csv", row.names = FALSE)
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
        nomeColuna = c()
        t = 1 
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        
        setwd(FolderGT)
        #cat("\nSave Y_pred: ", g, "\n")
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        write.csv(pred, "y_predict.csv", row.names = FALSE)
      }
      g = g + 1
      gc()
    }
    
    gc()
  }
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END OF FUNCTION SPLIT Y TRUE AND Y PREDICT                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS RANDOM TEST                                                           #
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
gatherPredsRandomT3 <- function(ds, dataset_name, number_folds, FolderRandom3){
  
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  
  f = 1
  gaTestParalel <- foreach(f = 1:number_folds) %dopar% {
    
    library("RWeka")
    library("rJava")
    library("foreign")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    
    cat("\nFold: ", f)
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderTest = paste(FolderRandom3, "/Test", sep="")
    FolderSplit = paste(FolderTest, "/Split-", f, sep="")
    
    setwd(FolderRandom3)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    num.part = bestPartition2$num.part
    
    FolderPart = paste(FolderSplit, "/Partition-", num.part, sep="")
    
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    
    ############################################################################################################
    #cat("\nGET THE GROUPS NUMBER OF PARTITION\n")
    res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == num.part))
    num.grupo = res_grupos2$groups
    #cat("\n\tNumber of groups for this partition", num.grupo)
    
    g = 1
    while(g<=num.grupo){
      
      library("RWeka")
      library("rJava")
      library("foreign")
      library("stringr")
      library("AggregateR")    
      library("plyr")
      library("dplyr")
      
      cat("\n\tGroup: ", g)
      
      FolderGT = paste(FolderPart, "/Group-", g, sep="")
      
      setwd(FolderGT)
      y_pred1 = data.frame(read.csv("y_predict.csv"))
      y_true1 = data.frame(read.csv("y_true.csv"))
      
      y_pred = cbind(y_pred, y_pred1)
      y_true = cbind(y_true, y_true1)
      
      g = g + 1
      gc()
    }
    setwd(FolderSplit)
    y_true = y_true[,-1]
    y_pred = y_pred[,-1]
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    gc()
  }
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END OF FUNCTION GATHER PREDICTIS                                                #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION EVALUATION RANDOM TEST                                                                #
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
evalRandomT3 <- function(ds, dataset_name, number_folds, FolderRandom3, FolderReports){
  
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  confMatFinal = data.frame(medidas)
  folds = c("")
  
  f = 1
  while(f<=number_folds){  
    cat("\nFold: ", f)
    
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderTest = paste(FolderRandom3, "/Test", sep="")
    FolderSplit = paste(FolderTest, "/Split-", f, sep="")
    
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
    
    f = f + 1
    gc()
  }
  
  #cat("\n\t\tSave all measures")
  setwd(FolderTest)
  write.csv(confMatFinal, "FoldsEvaluated.csv", row.names = FALSE)
  
  confMatFinal2 = data.frame(t(confMatFinal))
  confMatFinal3 = confMatFinal2[-1,]
  colnames(confMatFinal3) = medidas
  teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  
  sumary = apply(teste,2,mean)
  sumary2 = data.frame(sumary)
  sumary3 = cbind(medidas, sumary2)
  names(sumary3) = c("Measures", "Summary")
  
  setwd(FolderTest)
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  setwd(FolderReports)
  write.csv(sumary3, paste(dataset_name, "-BestF1Macro-R3.csv", sep=""), row.names = FALSE)
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END OF FUNCTION EVALUATE                                                        #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER PARTITIONS TEST                                                                #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       Assessment measures for each random partition                                            #
##################################################################################################
gatherPartitionsT3 <- function(ds, namesLabels, number_folds, FolderRandom3){
  
  num.particoes = ds$Labels - 1
  
  f = 1
  while(f<=number_folds){
    cat("\nFold = ", f)
    FolderVal = paste(FolderRandom3, "/Validation", sep="")
    FolderSplit = paste(FolderVal, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    fold = f
    num.grupo = c(0)
    num.rotulo = c(0)
    names.labels = c("")
    index = c(0)
    partition3 = data.frame(fold, num.grupo, num.rotulo, names.labels, index)
  
    p = 2
    while(p<=num.particoes){
      
      cat("\nPartition: ", p)
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      setwd(FolderPartition)
      partition = data.frame(read.csv(paste("partition-", p, ".csv", sep="")))
      partition2 = partition[,-1]
      partition3 = rbind(partition3, partition2)
      p = p + 1
      gc()
    }
    
    setwd(FolderSplit)
    #cat("\nSave partitions\n")
    #print(partition3)
    #cat("\n")
    write.csv(partition3[-1,], "partitions.csv")
    
    f = f + 1
    gc()
  }
  
  gc()
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 3: END OF FUNCTION GATHER PARTITIONS                                               #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION DELETE RANDOM FILES                                                                   #
#   Objective                                                                                    #
#       delelete all unecessary files                                                            #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#
##################################################################################################
deleteRandomFiles3 <- function(ds, namesLabels, number_folds, FolderRandom3){
  
  num.particoes = ds$Labels - 1  
  
  setwd(FolderRandom3)
  grupos = data.frame(read.csv("numero-grupos.csv"))
  grupos2 = grupos[,-1]
  
  f = 1
  apagaRandom <- foreach (f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    FolderVal = paste(FolderRandom3, "/Validation", sep="" )
    FolderTest = paste(FolderRandom3, "/Test", sep="" )
    FolderSplitV = paste(FolderVal, "/Split-", f, sep="")
    FolderSplitT = paste(FolderTest, "/Split-", f, sep="" )
    
    setwd(FolderSplitT)
    unlink("y_predict.csv")
    unlink("y_true.csv")
    unlink("EvaluatedPartition.csv")
    
    p = 2
    while(p<=num.particoes){
      cat("\n\tPartition: ", p)
      
      FolderPartV = paste(FolderSplitV, "/Partition-", p, sep="")
      if(dir.exists(FolderPartV)==TRUE){
        setwd(FolderPartV)
        unlink("partition.csv")
        unlink(paste("partition-", p,".csv", sep=""))
        unlink("y_predict.csv")
        unlink("y_true.csv")
        naoexisteV = 0
      } else {
        naoexisteV = 1
      }
      
      FolderPartT = paste(FolderSplitT, "/Partition-", p, sep="" )
      if(dir.exists(FolderPartT)==TRUE){
        setwd(FolderPartT)
        unlink("particao.csv")
        unlink("y_predict.csv")
        unlink("y_true.csv")
        naoexisteT = 0
      } else {
        naoexisteT = 1
      }
      
      ############################################################################################################
      #cat("\nGET THE GROUPS NUMBER OF PARTITION\n")
      res_grupos2 = data.frame(grupos2 %>% filter(grupos2$fold == f, grupos2$part == p))
      num.grupo = res_grupos2$groups
      #cat("\n\tNumber of groups for this partition", num.grupo)
      
      g = 1
      while(g<=num.grupo){
        
        cat("\n\tGROUP: ", g)
        
        if(naoexisteT==1){
          
        } else {
          FolderGroupT = paste(FolderPartT, "/Group-", g, sep="" )
          setwd(FolderGroupT)
          unlink("inicioFimRotulos.csv")
          unlink("y_predict.csv")
          unlink("y_true.csv")
          
        }
        
        if(naoexisteV==1){
          
        } else {
          FolderGroupV = paste(FolderPartV, "/Group-", g, sep="")
          setwd(FolderGroupV)
          
          nome2 = paste(dataset_name, "-split-", f, "-group-", g, ".test.pred.arff", sep="")
          unlink(nome2)
          
          nome1 = paste("grupo_", g, ".csv", sep="")
          unlink(nome1)
          unlink("EvaluatedPartition.csv")
          unlink("particaoFinal.csv")
          unlink("inicioFimRotulos.csv")
          unlink("y_predict.csv")
          unlink("y_true.csv")
        }
        
        g = g + 1
        gc()
      }
      
      p = p + 1
      gc()
    }
    
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3: DELETE ALL FUNCTION!!!!                                                         #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
}

  

##################################################################################################
# FUNCTION CLUS RANDOM 3                                                                         #
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
clusRandom_3 <- function(ds, dataset_name, number_folds, namesLabels, FolderDSF, FolderResDataset, FolderRandom3, FolderReports){
  
  #FolderDSF = folders$folderDSFolds
  #FolderResDataset = folders$folderResDataset
  #FolderRandom3 = folders$folderRandom3
  #FolderReports = folders$folderReports
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: generated random partitions                                          #")  
  timeSplitVal = system.time(generatedRandomPartitionsV3(ds, namesLabels, dataset_name, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: build and validate partitions                                        #")  
  timeBuildVal = system.time(RandomPartitionsV3(ds, namesLabels, dataset_name, number_folds, FolderRandom3, FolderDSF))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: gather predictions partitions                                        #")  
  timePredsVal = system.time(gatherPredsRandomV3(ds, dataset_name, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: eval partitions                                                      #")  
  timeEvalVal = system.time(evalRandomV3(ds, dataset_name, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: gather eval partitions                                               #")  
  timeGatherVal = system.time(gatherEvaluationsV3(ds, dataset_name, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 VALIDATION: gather f1 partitions                                                 #")  
  timeF1Val = system.time(gatherF1macroV3(ds, dataset_name, number_folds, FolderRandom3, FolderReports))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: mount partitions                                                           #")  
  timeMountTest = system.time(mountRandomParT3(ds, dataset_name, number_folds, FolderDSF, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: predictions                                                                #")  
  timePredsTest = system.time(splitsPredictionsRandomT3(ds, dataset_name, number_folds, FolderDSF, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: gather predictions                                                         #")  
  timeGatherTest = system.time(gatherPredsRandomT3(ds, dataset_name, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: evaluation                                                                 #")  
  timeEvalTest = system.time(evalRandomT3(ds, dataset_name, number_folds, FolderRandom3, FolderReports))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: gather partitions                                                           #")
  timeGP = system.time(gatherPartitionsT3(ds, namesLabels, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3 TEST: delete unecessary files                                                    #")  
  timeDel = system.time(deleteRandomFiles3(ds, namesLabels, number_folds, FolderRandom3))
  cat("\n##################################################################################################")
  
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3: Save Runtime                                                                    #")
  RunTimeRandom = rbind(timeSplitVal, timeBuildVal, timePredsVal, timeEvalVal, timeGatherVal, timeF1Val,
                        timeMountTest, timePredsTest, timeGatherTest, timeEvalTest, timeGP)
  
  setwd(FolderReports)
  write.csv(RunTimeRandom, "RunTimeRandom3.csv")
  cat("\n##################################################################################################")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS RANDOM 3: END!!!!                                                                         #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
