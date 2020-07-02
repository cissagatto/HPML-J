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
# Script 2 - Utilities                                                                           #
# LAST UPDATE: 2020-06-28                                                                        #
##################################################################################################

##################################################################################################
# FUNCTION SET FOLDER                                                                            #
#   Objective:                                                                                   #
#       Set the workspace                                                                        #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      Workspace                                                                                 #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/GLPML", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/GLPML", sep="")
  setwd(FolderRoot)
}
setwd(FolderRoot)
setFolder <- function(){
  retorno = list()
  sistema = c(Sys.info())
  if (sistema[1] == "Linux"){
    Folder = paste("/home/", sistema[7], "/GLPML", sep="")
    setwd(Folder)
  } else {
    Folder = paste("C:/Users/", sistema[7], "/GLPML", sep="")
    setwd(Folder)
  }
  FolderRoot = Folder
  retorno$sistema = sistema
  retorno$Folder = Folder
  return(retorno)
}



##################################################################################################
# FUNCTION DIRECTORIES                                                                           #
#   Objective:                                                                                   #
#      Creates all the necessary folders for the project. These are the main folders that must   # 
#      be created and used before the script starts to run                                       #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directories <- function(){
  
  retorno = list()
  # folder that have scripts for plot curves roc/aupc
  folderResults = paste(FolderRoot, "/Results", sep="")
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(folderResults)
  }
  
  # folder that have scripts for plot curves roc/aupc
  folderUtils = paste(FolderRoot, "/Utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(dirUtils)
  }
  
  # folder where datasets are stored
  folderDatasets = paste(FolderRoot, "/Datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  }
  
  # folder where originals datasets are stored
  folderDO = paste(folderDatasets, "/Originals", sep="")
  if(dir.exists(folderDO) == TRUE){
    setwd(folderDO)
    dirDO = dir(folderDO)
    n_DO = length(dirDO)
  } else {
    dir.create(folderDO)
    setwd(folderDO)
    dirDO = dir(folderDO)
    n_DO = length(dirDO)
  }
  
  # folder where the 10-fold calculated for each dataset will be stored
  folder10F = paste(folderDatasets, "/10-Folds", sep="")
  if(dir.exists(folder10F) == TRUE){
    setwd(folder10F)
    dir10F = dir(folder10F)
    n_10F = length(dir10F)
  } else {
    dir.create(folder10F)
    setwd(folder10F)
    dir10F = dir(folder10F)
    n_10F = length(dir10F)
  }
  
  # folder where the 10-fold calculated for each dataset will be stored
  folderInfo10F = paste(folderDatasets, "/Info10Folds", sep="")
  if(dir.exists(folderInfo10F) == TRUE){
    setwd(folderInfo10F)
    dirInfo10F = dir(folderInfo10F)
    n_Info10F = length(dirInfo10F)
  } else {
    dir.create(folderInfo10F)
    setwd(folderInfo10F)
    dirInfo10F = dir(folderInfo10F)
    n_Info10F = length(dirInfo10F)
  }
  
  # return folders
  retorno$folderResults = folderResults
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderDO = folderDO
  retorno$folder10F = folder10F
  retorno$folderInfo10F = folderInfo10F
  
  # return of folder contents
  retorno$dirResults = dirResults
  retorno$dirUtils = dirUtils
  retorno$dirDatasets = dirDatasets
  retorno$dirDO = dirDO
  retorno$dir10F = dir10F
  retorno$dirInfo10F = dirInfo10F
  
  # return of the number of objects inside the folder
  retorno$n_Results = n_Results
  retorno$n_Utils = n_Utils
  retorno$n_Datasets = n_Datasets
  retorno$n_DO = n_DO
  retorno$n_10F = n_10F
  retorno$n_Info10F = n_Info10F
  
  return(retorno)
  gc()
}


diretorios = directories()

creatingFoldersPrincipals<- function(dataset_name){
  
  diretorios = directories()
  
  retorno = list()
  
  # folder to store only the labels for each dataset
  folderDS10F = paste(diretorios$folder10F, "/", dataset_name, sep="")
  if(dir.exists(folderDS10F) == TRUE){
    setwd(folderDS10F)
    dir_DS10F = dir(folderDS10F)
    n_DS10F = length(dir_DS10F)
  } else {
    dir.create(folderDS10F)
    setwd(folderDS10F)
    dir_DS10F = dir(folderDS10F)
    n_DS10F = length(dir_DS10F)
  }
  
  # folder to store only the labels for each dataset
  folderResDataset = paste(diretorios$folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderResDataset) == TRUE){
    setwd(folderResDataset)
    dir_ResDataset = dir(folderResDataset)
    n_ResDataset = length(dir_ResDataset)
  } else {
    dir.create(folderResDataset)
    setwd(folderResDataset)
    dir_ResDataset = dir(folderResDataset)
    n_ResDataset = length(dir_ResDataset)
  }
  
  # Folder to save HCLUST/CUTREE results
  folderHClust = paste(folderResDataset, "/HClust", sep="")
  if(dir.exists(folderHClust) == TRUE){
    setwd(folderHClust)
    dir_HClust = dir(folderHClust)
    n_HClust = length(dir_HClust)
  } else {
    dir.create(folderHClust)
    setwd(folderHClust)
    dir_HClust = dir(folderHClust)
    n_HClust = length(dir_HClust)
  }
  
  # folder where the partition files mounted for each FOLD will be stored for later use
  folderHybPart = paste(folderResDataset, "/HybridPartition", sep="")
  if(dir.exists(folderHybPart) == TRUE){
    setwd(folderHybPart)
    dir_HybPart = dir(folderHybPart)
    n_HybPart = length(dir_HybPart)
  } else {
    dir.create(folderHybPart)
    setwd(folderHybPart)
    dir_HybPart = dir(folderHybPart)
    n_HybPart = length(dir_HybPart)
  }  
  
  # folder 
  folderHybrid = paste(folderResDataset, "/ClusHybrid", sep="")
  if(dir.exists(folderHybrid) == TRUE){
    setwd(folderHybrid)
    dir_Hybrid = dir(folderHybrid)
    n_Hybrid = length(dir_Hybrid)
  } else {
    dir.create(folderHybrid)
    setwd(folderHybrid)
    dir_Hybrid = dir(folderHybrid)
    n_Hybrid = length(dir_Hybrid)
  }
  
  # folder to store config files of each split for execute clus
  folderConfigFiles = paste(folderResDataset, "/ConfigFiles", sep="")
  if(dir.exists(folderConfigFiles) == TRUE){
    setwd(folderConfigFiles)
    dir_ConfigFiles = dir(folderConfigFiles)
    n_ConfigFiles = length(dir_ConfigFiles)
  } else {
    dir.create(folderConfigFiles)
    setwd(folderConfigFiles)
    dir_ConfigFiles = dir(folderConfigFiles)
    n_ConfigFiles = length(dir_ConfigFiles)
  }
  
  # folder 
  folderGlobal = paste(folderResDataset, "/ClusGlobal", sep="")
  if(dir.exists(folderGlobal) == TRUE){
    setwd(folderGlobal)
    dir_Global = dir(folderGlobal)
    n_Global = length(dir_Global)
  } else {
    dir.create(folderGlobal)
    setwd(folderGlobal)
    dir_Global = dir(folderGlobal)
    n_Global = length(dir_Global)
  }
  
  # folder 
  folderLocal = paste(folderResDataset, "/ClusLocal", sep="")
  if(dir.exists(folderLocal) == TRUE){
    setwd(folderLocal)
    dir_Local = dir(folderLocal)
    n_Local = length(dir_Local)
  } else {
    dir.create(folderLocal)
    setwd(folderLocal)
    dir_Local = dir(folderLocal)
    n_Local = length(dir_Local)
  }
  
  # folder 
  folderBR = paste(folderResDataset, "/BinRel", sep="")
  if(dir.exists(folderBR) == TRUE){
    setwd(folderBR)
    dir_BR = dir(folderBR)
    n_BR = length(dir_BR)
  } else {
    dir.create(folderBR)
    setwd(folderBR)
    dir_BR = dir(folderBR)
    n_BR = length(dir_BR)
  }
  
  # return folders
  retorno$folderDS10F = folderDS10F
  retorno$folderResDataset = folderResDataset
  retorno$folderHClust = folderHClust
  retorno$folderHybPart = folderHybPart
  retorno$folderConfigFiles = folderConfigFiles
  retorno$folderLocal = folderLocal
  retorno$folderGlobal = folderGlobal
  retorno$folderHybrid = folderHybrid
  retorno$folderBR = folderBR
  
  # return of folder contents
  retorno$dir_DS10F = dir_DS10F
  retorno$dir_ResDataset = dir_ResDataset
  retorno$dir_HClust = dir_HClust
  retorno$dir_HybPart = dir_HybPart
  retorno$dir__ConfigFiles = dir_ConfigFiles
  retorno$dir_Local = dir_Local
  retorno$dir_Global = dir_Global
  retorno$dir_Hybrid = dir_Hybrid
  retorno$dir_BR = dir_BR
  
  # return of the number of objects inside the folder
  retorno$n_DS10F = n_DS10F
  retorno$n_ResDataset = n_ResDataset
  retorno$n_HClust = n_HClust
  retorno$n_HybPart = n_HybPart
  retorno$n_Local = n_Local
  retorno$n_Global = n_Global
  retorno$n_Hybrid = n_Hybrid
  retorno$n_BR = n_BR
  
  return(retorno)
  
}

##################################################################################################
# FUNCTION CONVERT TO ARFF                                                                       #
# Objective:                                                                                     #
#
# Parameters:                                                                                    #
#
# Return:                                                                                        #
# 
##################################################################################################
converteArff <- function(arg1, arg2, arg3){  
  str = paste("java -jar ", diretorios$folderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  system(str)
  cat("\n\n")  
}



##################################################################################################
# FUNCTION CRIA PASTAS CLUS                                                                      #
# Objective:                                                                                     #
#
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
##################################################################################################
criaPastasCSC <- function(dataset_name){
  diretorios = directories()
  #setwd(FolderRoot)
  # "C:/Users/elain/PJ10Fold/ConfigSplitsClus/DATASET
  Pasta1 = paste(diretorios$folderCSC, "/", dataset_name, sep="")
  dir.create(Pasta1)
  print(Pasta1)
  cat("\n")    
  # do primeiro fold ate o ultimo --> sao 10 folds
  j = 1
  while(j<=10){
    cat("\n\tFOLD: ", j)
    setwd(Pasta1)
    Pasta2 = paste(Pasta1, "/Split-", j, sep="")
    dir.create(Pasta2)
    setwd(Pasta2)
    print(Pasta2)
    cat("\n")
    j = j + 1
    gc()
  } 
  gc()
}



##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
# Objective:                                                                                     #
#     Gets the information that is in the "datsets.csv" file.                                    #  
# Parameters:                                                                                    #
#     dataset: the specific dataset                                                              #
# Return:                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  return(retorno)
  gc()
}



##################################################################################################
# FUNCTION ADD CSV                                                                               #
# Objective:                                                                                     #
#     Add ".csv" in the file name                                                                #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     filenames: filename with ".csv"                                                            #
##################################################################################################
AddCSV <- function(filenames){
  j = 1
  for(j in 1:length(filenames)){
    str = paste(filenames[j], ".csv", sep="")
    filenames[j] = str
    j = j + 1
    gc()
  }
  return(filenames)
}


##################################################################################################
# FUNCTION REMOVE CSV                                                                            #
# Objective:                                                                                     #
#     Remove ".csv" from the file name                                                           #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     folderNames: a vector with folder names whithout ".csv"                                    #
##################################################################################################
RemoveCSV <- function(filenames){
  folderNames = filenames
  j = 0
  for(j in 1:length(folderNames)){
    a = str_length(folderNames[j])
    a = a - 4
    folderNames[j] = str_sub(folderNames[j], end = a)  
    j = j + 1
    gc()
  }  
  return(folderNames)
}


##################################################################################################
# FUNCTION REMOVE ARFF                                                                           #
# Objective:                                                                                     #
#     Remove ".arff" from the file name                                                          #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     folderNames: a vector with folder names without ".arff"                                    #
##################################################################################################
RemoveARFF <- function(filenames){
  folderNames = filenames
  j = 0
  for(j in 1:length(folderNames)){
    a = str_length(folderNames[j])
    a = a - 5
    folderNames[j] = str_sub(folderNames[j], end = a)  
    j = j + 1
    gc()
  }  
  return(folderNames)
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################