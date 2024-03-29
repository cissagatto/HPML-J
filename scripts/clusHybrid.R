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
# Script 8 - CLUS HYBRID                                                                         #
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
# DATA FRAMES                                                                                    #
##################################################################################################

numero_split = c(0)

precisionSample = c(0)
precisionWeighted = c(0)
precisionMacro = c(0)
precisionMicro = c(0)
Precision = data.frame(numero_split, precisionSample, precisionWeighted, precisionMacro, precisionMicro)

recallSample = c(0)
recallWeighted = c(0)
recallMacro = c(0)
recallMicro = c(0)
Recall = data.frame(numero_split, recallSample, recallWeighted, recallMacro, recallMicro)

f1Sample = c(0)
f1Weighted = c(0)
f1Macro = c(0)
f1Micro = c(0)
F1 = data.frame(numero_split, f1Sample, f1Weighted, f1Macro, f1Micro)


##################################################################################################
# FUNCTION MOUNT HYBRID PARTITION                                                                #
#   Objective                                                                                    #
#       Mount hybrid partitions to test on clus                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHClust: hclust and cutree folder path                                              #
#       DsFolds: folder dataset                                                                  #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       partitions mounted to test                                                               #
##################################################################################################
mountHybPartTEST <- function(ds, dataset_name, number_folds, DsFolds, FolderHClust, 
                             FolderHybPart, FolderHybrid){
  
  diretorios = directories(shm)
  
  # specifying folders
  FolderTr = paste(DsFolds, "/Tr", sep="")
  FolderTs = paste(DsFolds, "/Ts", sep="")
  FolderUtils = paste(FolderRoot, "/utils", sep="")
  
  fold = c()
  part = c() 
  silho = c()
  qualidadeSilho = data.frame(fold, part, silho)
  
  # 10 FOLDS
  f = 1
  mountHybParalel <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\nFold: ", f)
    
    library("RWeka")
    library("rJava")
    library("foreign")
    
    FolderClusterSplit = paste(FolderHClust, "/Split-", f, sep="")
    
    # cat("\nSelect Best Partition for", f, "\n")
    setwd(FolderHybPart)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    particaoEscolhida = bestPartition2$num.part
    # cat("\n\tChoose Partition:", particaoEscolhida, "\n")
    
    # cat("\nSelect hclust coefficient for", f, "\n")
    setwd(FolderHClust)
    coeficiente = data.frame(read.csv("BestFoldsCoef.csv"))
    coeficiente_fold = coeficiente[f,]
    metodo_fold = toString(coeficiente_fold$metodo)
    # cat("\n\tChoose Method HClust: ", metodo_fold , "\n")
    
    # creating folder
    FolderHybridF = paste(FolderHybrid, "/Split-", f, sep="")
    if(dir.exists(FolderHybridF)== TRUE){
      cat("\n")
    } else {
      dir.create(FolderHybridF)
    }
    
    #cat("\nOpen File Config Best Partition for ", f, "\n")
    FolderClusterSplitPart = paste(FolderClusterSplit, "/", metodo_fold, "/Clusters", sep="")
    setwd(FolderClusterSplitPart)
    nome_arquivo = paste(FolderClusterSplitPart, "/cluster_", particaoEscolhida, ".csv", sep="")
    configParticao = data.frame(read.csv(nome_arquivo))
    names(configParticao)[1] = "Labels"
    
    #cat("\nOpen Train file ", f, "\n")
    setwd(FolderTr)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))
    
    #cat("\nOpen Test file ", f, "\n")
    setwd(FolderTs)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))
    
    ####################################################################################
    grupoSilhuetaTR = data.frame()
    grupoSilhuetaTS = data.frame()
    
    #cat("\nMount Groups of Labels for Fold ", f, "\n")
    k = 1
    while(k<=particaoEscolhida){
      
      cat("\n\tPartition: ", k)
      
      # creating folder
      FolderHybridFClus = paste(FolderHybridF, "/Group-", k, sep="")
      if(dir.exists(FolderHybridFClus)== TRUE){
        cat("\n")
      } else {
        dir.create(FolderHybridFClus)
      }
      
      #cat("\n\tSpecific Group: ", k, "\n")
      grupoEspecifico = data.frame(configParticao %>% filter(., configParticao$grupo == k))  
      
      cat("\nCreating Train File")
      #cat("\n\tTRAIN: Mount Group ", k, "\n")
      atributos_tr = arquivo_tr[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_tr)
      rotulos_tr = toString(grupoEspecifico$Labels)
      classes_tr = select(arquivo_tr, grupoEspecifico$Labels)
      n_c = ncol(classes_tr)
      grupo_tr = cbind(atributos_tr, classes_tr)
      fim_tr = ncol(grupo_tr)
      
      ####################################################################################
      # grupo silhueta
      esteGrupoTR = cbind(clusters = k, data.frame(t(classes_tr)))
      grupoSilhuetaTR = rbind(grupoSilhuetaTR, esteGrupoTR)
      
      #cat("\n\tTRAIN: Save Group", k, "\n")
      setwd(FolderHybridFClus)
      nome_tr = paste(dataset_name, "-split-tr-", f, "-group-", k, ".csv", sep="")
      write.csv(grupo_tr, nome_tr, row.names = FALSE)
      
      #cat("\n\tINICIO FIM TARGETS: ", k, "\n")
      inicio = ds$LabelStart
      fim = fim_tr
      ifr = data.frame(inicio, fim)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      #cat("\n\tTRAIN: Convert Train CSV to ARFF ", k , "\n")
      nome_arquivo_2 = paste(dataset_name, "-split-tr-", f, "-group-", k, ".arff", sep="")
      arg1Tr = nome_tr
      arg2Tr = nome_arquivo_2
      arg3Tr = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1Tr, " ", arg2Tr, " ", arg3Tr, sep="")
      cat("\n")		
      print(system(str))
      cat("\n")		
      
      #cat("\n\tTRAIN: Verify and correct {0} and {1} ", k , "\n")
      arquivo = paste(FolderHybridFClus, "/", arg2Tr, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      cat("\n")
      print(system(str0))
      cat("\n")
      
      cat("\nCreating Test File")
      #cat("\n\tTEST: Mount Group: ", k, "\n")
      atributos_ts = arquivo_ts[ds$AttStart:ds$AttEnd]
      rotulos_ts = toString(grupoEspecifico$Labels)
      classes_ts = select(arquivo_ts, grupoEspecifico$Labels)
      grupo_ts = cbind(atributos_ts, classes_ts)
      fim_ts = ncol(grupo_ts)
      #cat("\n\tTest Group Mounted: ", k, "\n")
      
      ####################################################################################
      # grupo silhueta
      esteGrupoTS = cbind(clusters = k, data.frame(t(classes_ts)))
      grupoSilhuetaTS = rbind(grupoSilhuetaTS, esteGrupoTS)
      
      #cat("\n\tTEST: Save Group ", k, "\n")
      setwd(FolderHybridFClus)
      nome_ts = paste(dataset_name, "-split-ts-", f, "-group-", k, ".csv", sep="")
      write.csv(grupo_ts, nome_ts, row.names = FALSE)
      
      #cat("\n\tTEST: Convert CSV to ARFF ", k , "\n")
      nome_arquivo_3 = paste(dataset_name, "-split-ts-", f,"-group-", k, ".arff", sep="")
      arg1Ts = nome_ts
      arg2Ts = nome_arquivo_3
      arg3Ts = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
      cat("\n")
      print(system(str))
      cat("\n")
      
      #cat("\n\tTEST: Verify and correct {0} and {1} ", k , "\n")
      arquivo = paste(FolderHybridFClus, "/", arg2Ts, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      cat("\n")
      print(system(str0))
      cat("\n")
      
      cat("\nCreating .s file for clus")
      if(inicio == fim){
        #cat("\nCreate config file for clus: ", k , "\n")
        
        nome_config = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
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
        
        cat("\nExecute CLUS: ", k , "\n")
        nome_config2 = paste(FolderHybridFClus, "/", nome_config, sep="")
        str = paste("java -jar ", FolderUtils, "/Clus.jar ", nome_config2, sep="")
        
        cat("\n")
        print(system(str))
        cat("\n")
        
      } else {
        # cat("\nCreate config file for clus: ", k , "\n")
        
        nome_config = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
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
        
        cat("\nExecute CLUS: ", k)
        nome_config2 = paste(FolderHybridFClus, "/", nome_config, sep="")
        str = paste("java -jar ", FolderUtils, "/Clus.jar ", nome_config2, sep="")
        
        cat("\n")
        print(system(str))
        cat("\n")
      } # end of groups
      
      
      #library(cluster)
      #a = dist(grupoSilhuetaTR)
      #b = as.dist(a)
      #sil = silhouette(grupoSilhuetaTR$clusters, dist(grupoSilhuetaTR))
      #sil = sortSilhouette(sil)
      #write.csv(sil, paste("silhueta-p", p, ".csv", sep=""))
      
      #pdf(paste("sil-p-", p, ".pdf", sep=""), width = 10, height = 8)
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
      #part = p 
      #silho = avgTotal
      #qualidadeSilho = rbind(qualidadeSilho, data.frame(fold, part, silho))
      
      #library("factoextra")
      #pdf(paste("fviz-sil-p-", p, ".pdf", sep=""), width = 10, height = 8)
      #print(fviz_silhouette(sil))
      #dev.off()
      #cat("\n")     
      
      # deleting files
      um = paste(dataset_name, "-split-", f, "-group-", k, ".model", sep="")
      dois = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
      tres = paste(dataset_name, "-split-tr-", f, "-group-", k, ".arff", sep="")
      quatro = paste(dataset_name, "-split-ts-", f, "-group-", k, ".arff", sep="")
      cinco = paste(dataset_name, "-split-tr-", f, "-group-", k, ".csv", sep="")
      seis = paste(dataset_name, "-split-ts-", f, "-group-", k, ".csv", sep="")
      
      setwd(FolderHybridFClus)
      unlink(um, recursive = TRUE)
      unlink(dois, recursive = TRUE)
      unlink(tres, recursive = TRUE)
      unlink(quatro, recursive = TRUE)
      unlink(cinco, recursive = TRUE)
      unlink(seis, recursive = TRUE)
      
      k = k + 1
      gc()
    } # end partitions
    
        gc()
  } # ending folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS HYBRID: END OF MOUNT HYBRID PARTITION FOR TEST ON CLUS                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION SPLITS PREDCTIONS HYBRIDS                                                             #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       DsFolds: folder dataset                                                                  #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       true labels and predict labels                                                           #
##################################################################################################
splitsPredsHybTEST <- function(ds, dataset_name, number_folds, DsFolds, FolderHybrid, FolderHybPart){
  
  diretorios = directories(shm)
  
  # from fold = 1 to number_folds
  f = 1
  gfhParalel <- foreach(f = 1:number_folds) %dopar% {
  
    cat("\nFold: ", f)
    
    setwd(FolderHybrid)
    FolderSplitClus = paste(FolderHybrid, "/Split-", f, sep="")
    
    # get the best partition
    setwd(FolderHybPart)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    num.part = bestPartition2$num.part
    
    # from group = 1 to last group
    g = 1
    while(g<=num.part){
      
      cat("\n\tGroup: ", g)
      
      library("foreign")
      
      setwd(FolderSplitClus)
      
      # specifying the folder group
      FolderSplitClusGroup = paste(FolderSplitClus, "/Group-", g, sep="")
      
      # open the predictions
      setwd(FolderSplitClusGroup)
      nome = paste(FolderSplitClusGroup, "/", dataset_name, "-split-", f, "-group-", g, ".test.pred.arff", sep="")
      predicoes = data.frame(read.arff(nome))
      
      # start and end labels
      ifr = data.frame(read.csv("inicioFimRotulos.csv"))
      inicio = ifr$inicio
      fim = ifr$fim
      
      # if start == end
      if(inicio == fim){
        
        setwd(FolderSplitClusGroup)
        
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
        setwd(FolderSplitClusGroup)
        comeco = 1+(fim - inicio)
        
        #cat("\nSave Y_true: ", g, "\n")
        classes = data.frame(predicoes[,1:comeco])
        write.csv(classes, "y_true.csv", row.names = FALSE)
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
        # changing the name of the columns
        nomeColuna = c()
        t = 1 
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        
        # cat("\nSave Y_pred: ", g, "\n")
        setwd(FolderSplitClusGroup)
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        write.csv(pred, "y_predict.csv", row.names = FALSE)
      }
      
      g = g + 1
      gc()
    } # end groups
    
    gc()
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS HYBRID: END OF FUNCTION SPLIT Y TRUE AND Y PREDICT                                        #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS HYBRID                                                                #
#   Objective                                                                                    #
#       Joins the real outputs and the predicted outputs in a single file                        #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       true labels and predict labels                                                           #
##################################################################################################
gatherPredsHybTEST <- function(ds, dataset_name, number_folds, FolderHybrid, FolderHybPart){
  
  diretorios = directories(shm)
  
  # from fold = 1 to number_folds
  f = 1
  gaTestParalel <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\nFold: ", f)
    
    # specifying folder
    setwd(FolderHybrid)
    FolderSplitClus = paste(FolderHybrid, "/Split-", f, sep="")
    
    # get the best partition
    setwd(FolderHybPart)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    num.part = bestPartition2$num.part
    
    # data frame
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    # from grou =  1 to num.part
    g = 1
    while(g<=num.part){
      
      cat("\n\tGroup: ", g)
      
      library("foreign")
      
      # specifying folder
      FolderSplitClusGroup = paste(FolderSplitClus, "/Group-", g, sep="")
      
      # get true and predict labels
      setwd(FolderSplitClusGroup)
      y_pred1 = data.frame(read.csv("y_predict.csv"))
      y_true1 = data.frame(read.csv("y_true.csv"))
      
      # put together
      y_pred = cbind(y_pred, y_pred1)
      y_true = cbind(y_true, y_true1)
      
      # delete files
      unlink("y_predict.csv", recursive = TRUE)
      unlink("y_true.csv", recursive = TRUE)
      
      g = g + 1
      gc()
    } # end groups
    
    # save
    setwd(FolderSplitClus)
    y_true = y_true[,-1]
    y_pred = y_pred[,-1]
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    gc()
  } # eng folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS HYBRID: END OF FUNCTION GATHER PREDICTIS                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION EVALUATION HYBRID                                                                     #
#   Objective                                                                                    #
#       Evaluation the test                                                                      #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
evalHybTEST <- function(ds, dataset_name, number_folds, FolderHybrid, FolderReports){
  
  diretorios = directories(shm)
  
  # vector with names measures
  medidas = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  # data frame
  confMatFinal = data.frame(medidas)
  
  # folds
  folds = c("")
  
  # from f = 1 to number_folds
  f = 1
  while(f<=number_folds){  
    
    cat("\nFold: ", f)
    
    library("mldr")
    library("utiml")
    
    # specifying folder
    setwd(FolderHybrid)
    FolderSplitClus = paste(FolderHybrid, "/Split-", f, sep="")
    
    # get the true and predict labels
    setwd(FolderSplitClus)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    # compute the measures
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
    setwd(FolderSplitClus)
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
    setwd(FolderSplitClus)
    write.csv(confMatPart, namae)  
    
    # put together
    confMatFinal = cbind(confMatFinal, confMatPart)
    
    # delete files
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  } # end folds
  
  #cat("\n\t\tSave all measures")
  setwd(FolderHybrid)
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
  
  # save
  setwd(FolderHybrid)
  write.csv(sumary3, "SummaryFoldsEvaluated.csv", row.names = FALSE)
  
  setwd(FolderReports)
  write.csv(sumary3, paste(dataset_name, "-SummaryFoldsEvaluated-H.csv", sep=""), row.names = FALSE)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS HYBRID: END OF FUNCTION EVALUATE                                                          #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION DELETE HYBRID                                                                         #
#   Objective                                                                                    #
#       deletes all unnecessary files                                                            #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
##################################################################################################
delHybTEST <-function(ds, dataset_name, number_folds, FolderHybrid, FolderHybPart){
  
  diretorios = directories(shm)
  
  # from fold = 1 to number_folds
  f = 1
  apagaHybrid <- foreach (f = 1:number_folds) %dopar%{
    
    cat("\nFold  ", f)
    
    # specifying folder
    FolderSplit = paste(FolderHybrid, "/Split-", f, sep="")
    
    # get the best partition
    setwd(FolderHybPart)
    bestPartition = data.frame(read.csv("BestF1Macro.csv"))
    bestPartition2 = bestPartition[f,]
    num.part = bestPartition2$num.part
    
    # from group = 1 to num.part
    g = 1
    while(g<=num.part){
      cat("\n\tGroup  ", g)
      FolderGroup = paste(FolderSplit, "/Group-", g, sep="")
      setwd(FolderGroup)
      unlink("inicioFimRotulos.csv", recursive = TRUE)
      
      # gPositiveGo-split-1-group-1.test.pred.arff
      nome1 = paste(dataset_name, "-split-", f, "-group-", g, ".test.pred.arff", sep="")
      unlink(nome1)
      
      g = g + 1
      gc()
    } # end groups
    
    gc()
  } # end folds
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS HYBRID: END OF FUNCTION DEELETE FILES                                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION CLUS HYBRID                                                                           #
#   Objective                                                                                    #
#       Tests and evaluate the choosen hybrid partitions                                         #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderHClust: hclust and cutree folder path                                              #
#       DsFolds: folder dataset                                                                  #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       Predictions, assessment measures and execution time                                      #
##################################################################################################
clusHybrid <- function(ds, dataset_name, number_folds, DsFolds, FolderHClust, 
                       FolderHybPart, FolderHybrid, FolderReports){
  
  diretorios = directories(shm)
  
  cat("\n#################################################################################################")
  cat("\n# CLUS HYBRID: Tests each Splits in the best found partition configuration                     #")
  cat("\n#################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Set up the Split groups and execute CLUS                                        #")
  tempoMonta= system.time(mountHybPartTEST(ds, dataset_name, number_folds, DsFolds, FolderHClust, FolderHybPart, FolderHybrid))  
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Split predictions                                                              #")
  tempoSplitPred = system.time(splitsPredsHybTEST(ds, dataset_name, number_folds, DsFolds, FolderHybrid, FolderHybPart))  
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Joins the real outputs and the predicted outputs in a single file               #")
  tempoJuntaPred = system.time(gatherPredsHybTEST(ds, dataset_name, number_folds, FolderHybrid,FolderHybPart))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Evaluation Choosen Hybrid Partition                                                                     #")
  tempoEval = system.time(evalHybTEST(ds, dataset_name, number_folds, FolderHybrid, FolderReports))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Delete files                                                                        #")
  tempoD = system.time(delHybTEST(ds, dataset_name,  number_folds, FolderHybrid, FolderHybPart))
  cat("\n################################################################################################")
  
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: Save Runtime                                                                   #")
  RuntimeTest = rbind(tempoMonta, tempoSplitPred, tempoJuntaPred, tempoEval)
  
  setwd(FolderReports)
  write.csv(RuntimeTest, "RuntimeHybrid.csv")
  cat("\n################################################################################################")
  
  gc()
  cat("\n################################################################################################")
  cat("\n# CLUS HYBRID: END!!!                                                                          #") 
  cat("\n################################################################################################")
  cat("\n\n\n\n")
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################