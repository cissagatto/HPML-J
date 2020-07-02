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
# Script 5 - Correlations                                                                        #
# LAST UPDATE: 2020-06-28                                                                        #
##################################################################################################

##################################################################################################
# Workspace configuration                                                                        #
##################################################################################################
diretorios = directories()
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder


##################################################################################################
# Scientific notation setup                                                                      #
##################################################################################################
options(scipen=30)


##################################################################################################
# FUNCTION CRIA CORRELATION                                                                      #
#   Objective:                                                                                   #
#                                                                                                #  
#   Parameters:                                                                                  #
#
#   Return:                                                                                      #
#                                                                                                #
##################################################################################################
computeJaccard <- function(ds, resLS, dataset_name, namesLabels, FolderHC){
  
  s = 1
  jaccardParalel <- foreach(s = 1:10) %dopar%{
    
    cat("\nFold: ", s, "\n")
    
    library("reshape2")
    library("philentropy")
    
    merge_matrix <- function(matrix_correlation){
      cat("\n\tMerge matrix")
      matrix_correlation <- round(matrix_correlation,4)
      melt_mat_cor <- melt(matrix_correlation)
      return (melt_mat_cor)
      cat("\n")
      gc()
    }
    
    get_lower_tri<-function(matrix_correlation){
      cat("\n\tGet Lower Tri")
      matrix_correlation_1[upper.tri(matrix_correlation)] <- NA
      return(matrix_correlation_1)
      cat("\n")
      gc()
    }
    
    get_upper_tri <- function(matrix_correlation){
      cat("\n\tGet Upper Tri")
      matrix_correlation[lower.tri(matrix_correlation)]<- NA
      return(matrix_correlation)
      cat("\n")
      gc()
    }
    
    cut_matrix <- function(matrix_correlation, measure){
      cat("\n\tCut Matrix")
      upper_tri <- get_upper_tri(matrix_correlation)
      melt_mat_cor <- melt(upper_tri, na.rm = TRUE) 
      return(melt_mat_cor)
      cat("\n")
      gc()
    }
    
    reorder_mat_cor <- function(matrix_correlation){
      cat("\n\tReorder Matrix")
      dd <- as.dist((1-matrix_correlation)/2)
      hc <- hclust(dd)
      print(hc)
      matrix_correlation <- matrix_correlation[hc$order, hc$order]
      return(matrix_correlation)
      cat("\n")
      gc()
    }
    
    FolderHCES = paste(FolderHC, "/Split-", s, sep="")
    if(dir.exists(FolderHCES)==TRUE){
      cat("\n")
    } else{
      dir.create(FolderHCES)  
    }
    
    setwd(FolderHCES)
    
    cat("\nGET LABELS SPACE\n")
    classes = resLS$Classes[s]
    classes = data.frame(classes)
    classes = t(classes)
    
    cat("\nCALCULA JACCARD\n")
    matrix_correlation = distance(classes, method = "jaccard", use.row.names = TRUE)
    write.csv(matrix_correlation, "matrix_correlation.csv", row.names = FALSE)
    nomesRotulos = colnames(matrix_correlation)
    
    cat("\nCHECK NA\n")
    matrix_correlation_na = is.na(matrix_correlation)
    matrix_correlation_na_2 = replace(x = matrix_correlation, list = is.na(matrix_correlation), values = 0)
    
    cat("\nGET COL NAMES\n")
    rownames(matrix_correlation) <- namesLabels
    matrix_correlation_2 = as.matrix(matrix_correlation)
    
    cat("\nREORGANIZE\n")
    matrix_correlation_order <- reorder_mat_cor(matrix_correlation_2)
    upper_tri <- get_upper_tri(matrix_correlation_order)
    
    cat("\nMELT MATRIX\n")
    melt_mat_cor <- melt(upper_tri, na.rm = TRUE)
    write.csv(melt_mat_cor, "melt_mat_cor.csv", row.names = FALSE)
    
    cat("\nincrement\n")
    s = s + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO CALCULA JACCARD                                                                  #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GET LOWER TRI                                                                         # 
#   Objective:                                                                                   #
#                                                                                                #  
#   Parameters:                                                                                  #
#      matrix_correlation: jaccard correlations                                                  #
#   Return:                                                                                      #
#                                                                                                #
##################################################################################################
CutreeHClust <- function(ds, resLS, dataset_name, namesLabels, FolderHClust){
  
  fold = c(0)
  average = c(0)
  complete = c(0)
  single = c(0)
  c4 = data.frame(fold, average, complete, single)
  
  # methods
  metodos = c("average", "complete", "single")
  
  s = 1
  cutreeParalel <- foreach(s = 1:10) %dopar%{
    cat("\nFold: ", s) 
    
    cat("\nLOAD LIBRARIES\n")
    library("cluster")
    library("dendextend")  
    library("pvclust")
    library("ggdendro")
    library("ape")
    library("GGally")    
    
    cat("\nCreate DataFrames\n")
    fold = c(0)
    average = c(0)
    complete = c(0)
    single2 = c(0)
    c4 = data.frame(fold, average, complete, single2)
    
    fold = c(0)
    metodo = c(0)
    coeficiente = c(0)
    coefHC = data.frame(fold, metodo, coeficiente)
    
    metodos = c("average", "complete", "single")      
    
    cat("\nCreate folder\n")
    FolderHCES = paste(FolderHClust, "/Split-", s, sep="")
    if(dir.exists(FolderHCES)==TRUE){
      cat("\n")
    } else {
      dir.create(FolderHCES)
    }
    
    cat("\nOpen matrix correlation\n")
    setwd(FolderHCES)
    matrix_correlation = data.frame(read.csv("matrix_correlation.csv"))
    rownames(matrix_correlation) <- namesLabels
    matrix_correlation_2 = as.matrix(matrix_correlation)    
    
    i = 1
    for(i in i:3){
      cat("\nMethod: ", metodos[i], "\n")
      
      cat("\nCreates the folder to save information for this method\n")      
      FolderMethods = paste(FolderHCES, "/", metodos[i], sep="")
      if(dir.exists(FolderMethods)==TRUE){
        cat("\n")
      } else {
        dir.create(FolderMethods)
      }
      
      cat("\nCreates the folder to save graphics\n")            
      FolderGraphics = paste(FolderMethods, "/Graphics", sep="")
      if(dir.exists(FolderGraphics)==TRUE){
        cat("\n")
      } else {
        dir.create(FolderGraphics)
      }
      
      cat("\nCreates the folder to save clusters\n")                  
      FolderClusters = paste(FolderMethods, "/Clusters", sep="")
      if(dir.exists(FolderClusters)==TRUE){
        cat("\n")
      } else {
        dir.create(FolderClusters)
      }
      
      cat("\nDEND\n")                  
      Dend <- matrix_correlation_2 %>% as.dist %>% hclust(method = metodos[i]) %>% as.dendrogram
      
      cat("\nOTTER DENDRO\n")                  
      OtterDendro = as.dendrogram(hclust(d = as.dist(matrix_correlation_2), method=metodos[i]))
      
      cat("\nAsDist = as.dist(matrix_correlation)\n")                  
      AsDist = as.dist(matrix_correlation)
      
      cat("\nAsDistMatrix = as.matrix(AsDist)\n")                  
      AsDistMatrix = as.matrix(AsDist)
      
      cat("\nHC = hclust(AsDist, method=metodos[i])\n")                  
      HC = hclust(AsDist, method=metodos[i])
      
      cat("\nDendro = as.dendrogram(HC)\n")                  
      Dendro = as.dendrogram(HC)
      
      cat("\nCreates the folder to save clusters\n")                  
      DendData <- dendro_data(Dendro, type = "rectangle")
      
      cat("\nSAVE COEFF\n")                  
      fold = s
      metodo = metodos[i]
      coeficiente = coef.hclust(HC)
      coefHC = rbind(coefHC, data.frame(fold, metodo, coeficiente))
      print(coefHC)   
      
      cat("\nGRAPHIC: RADIAL\n")
      setwd(FolderGraphics)
      jpeg("radial.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
      print(plot(as.phylo(HC), type = "radial", cex = 0.6, no.margin = TRUE))
      dev.off()
      cat("\n")
      
      cat("\nGRAPHIC: FAN\n")
      jpeg("fan.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
      print(plot(as.phylo(HC), type = "fan", cex = 0.6, no.margin = TRUE))
      dev.off()
      cat("\n")
      
      cat("\nGRAPHIC: UNROOT\n")      
      jpeg("unroot.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
      print(plot(as.phylo(HC), type = "unrooted", cex = 0.6, no.margin = TRUE))
      dev.off()
      cat("\n")
      
      cat("\nGRAPHIC: CLADOGRAM\n")
      jpeg("cladogram.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
      print(plot(as.phylo(HC), type = "cladogram", cex = 0.6, no.margin = TRUE))
      dev.off()
      cat("\n")
      
      cat("\nGRAPHIC: DENDRO\n")
      jpeg("hc_plot.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
      print(plot(Dendro))
      print(with(pvclust:::hc2axes(as.hclust(Dendro)), 
                 text(x.axis, y.axis, round(y.axis, 2),col = "red", adj = c(0.5, 1.5), cex = 0.5)))
      dev.off()
      cat("\n")     
      
      #################################################################################################################### 
      cat("\nClustering: from the first to the last label\n")                  	  
      clusters3 = data.frame(c(0))
      tables = data.frame()
      k = 1
      for(k in 1:ds$Labels){
        cat("\ncluster: ", k)                
        setwd(FolderClusters)
        
        cat("\nCUTREE\n")        
        cutLabels = cutree(HC, k)
        clusters = data.frame(cutree(HC, k))
        names(clusters) = "grupo"
        
        cat("\nORDER CUTREE\n")        
        clusters2 = clusters[order(clusters$grupo, decreasing = FALSE),]
        
        cat("\nSAVE CUTREE\n")        
        write.csv(clusters, paste("cluster_", k, ".csv", sep=""))		
        
        clusters3 = cbind(clusters3, clusters2)
        names(clusters3)[1] = "apagar"
        names(clusters3)[k+1] = paste("particao-",k, sep="")
        k = k + 1 # increment label
        gc() # garbage collection
      }
      
      cat("\nSAVE ALL CUTREE\n")        
      setwd(FolderMethods)
      write.csv(clusters3[,-1], "partitions.csv", row.names = FALSE)
      
      cat("\nSAVE ALL COEFICIENTES\n")        
      setwd(FolderHCES)
      write.csv(coefHC[-1,], "coeficientes.csv", row.names = FALSE)
      
      i = i + 1 # increment hclust method
      gc() # garbage collection
    }    
    
    s = s + 1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO HCLUST AND CUTREE                                                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GET LOWER TRI                                                                         # 
#   Objective:                                                                                   #
#                                                                                                #  
#   Parameters:                                                                                  #
#      matrix_correlation: jaccard correlations                                                  #
#   Return:                                                                                      #
#                                                                                                #
##################################################################################################
bestCoefficient <- function(FolderHClust){
  
  fold = c(0)
  metodo = c(0)
  coeficiente = c(0)
  maioresCoeficientes = data.frame(fold, metodo, coeficiente)
  
  s = 1
  while(s<=10){  
    cat("\nFold: ", s)
    
    FolderSplit = paste(FolderHClust, "/Split-", s, sep="")
    
    setwd(FolderSplit)
    coef = data.frame(read.csv("coeficientes.csv"))
    coef = coef[order(coef$coeficiente, decreasing = TRUE),]
    maior = coef[1,]
    highestCoef = maior
    metodoEscolhido = toString(highestCoef$metodo)
    metodo = c("average", "complete", "single")
    result = c(metodoEscolhido == metodo)
    
    fold = s
    metodo = metodoEscolhido
    coeficiente = maior$coeficiente
    maioresCoeficientes = rbind(maioresCoeficientes, data.frame(fold, metodo, coeficiente))
    
    setwd(FolderHClust)
    write.csv(maioresCoeficientes[-1,], "BestFoldsCoef.csv", row.names = FALSE)
    
    # if the result is FALSE then delete the folder
    # if the result is equal to TRUE you cannot delete it!
    # AVERAGE
    if(result[1]==FALSE){
      FD = paste(FolderSplit, "/average", sep="")
      unlink(FD, recursive = TRUE)
    } 
    
    # COMPLETE
    if(result[2]==FALSE){
      FD = paste(FolderSplit, "/complete", sep="")
      unlink(FD, recursive = TRUE)
    }
    
    # SINGLE
    if(result[3]==FALSE){
      FD = paste(FolderSplit, "/single", sep="")
      unlink(FD, recursive = TRUE)
    }
    
    s = s +1
    gc()
  }
  gc()
  cat("\n##################################################################################################")
  cat("\n# FIM DA FUNCAO HIGIEST COEFFICIENT                                                              #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################
