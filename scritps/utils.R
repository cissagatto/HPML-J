##################################################################################################
# HPML-J                                                                                         #
##################################################################################################

##################################################################################################
# Script 2 - Utilities                                                                           #
##################################################################################################

##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
##################################################################################################

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
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
FolderScripts = paste(FolderRoot, "/scripts/", sep="")
setwd(FolderScripts)


##################################################################################################
# FUNCTION SET FOLDER                                                                            #
#   Objective:                                                                                   #
#       Set the folder path                                                                      #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      The correct path of the root folder                                                       #
##################################################################################################
setwd(FolderRoot)
setFolder <- function(){
  retorno = list()
  sistema = c(Sys.info())
  
  if (sistema[1] == "Linux"){
    Folder = paste("/home/", sistema[7], "/HPML-J", sep="")
    setwd(Folder)
  } else {
    Folder = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
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
  
  #folderReports = paste(FolderRoot, "/reports", sep="")
  #if(dir.exists(folderReports) == TRUE){
    #setwd(folderReports)
    #dirReports = dir(folderReports)
    #n_Reports = length(folderReports)
  #} else {
    #dir.create(folderReports)
    #setwd(folderReports)
    #dirReports = dir(folderReports)
    #n_Reports = length(folderReports)
  #}
  
  folderResults = paste(FolderRoot, "/results", sep="")
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
  
  folderUtils = paste(FolderRoot, "/utils", sep="")
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
  
  folderDatasets = paste(FolderRoot, "/datasets", sep="")
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
  
  folderDO = paste(folderDatasets, "/originals", sep="")
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
  
  folderFolds = paste(folderDatasets, "/folds", sep="")
  if(dir.exists(folderFolds) == TRUE){
    setwd(folderFolds)
    dirFolds = dir(folderFolds)
    n_Folds = length(dirFolds)
  } else {
    dir.create(folderFolds)
    setwd(folderFolds)
    dirFolds = dir(folderFolds)
    n_Folds = length(dirFolds)
  }
  
  folderInfoFolds = paste(folderDatasets, "/infoFolds", sep="")
  if(dir.exists(folderInfoFolds) == TRUE){
    setwd(folderInfoFolds)
    dirInfoFolds = dir(folderInfoFolds)
    n_InfoFolds = length(dirInfoFolds)
  } else {
    dir.create(folderInfoFolds)
    setwd(folderInfoFolds)
    dirInfoFolds = dir(folderInfoFolds)
    n_InfoFolds = length(dirInfoFolds)
  }
  
  # return folders
  retorno$folderResults = folderResults
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderDO = folderDO
  retorno$folderFolds = folderFolds
  retorno$folderInfoFolds = folderInfoFolds
  #retorno$folderReports = folderReports
  
  # return of folder contents
  retorno$dirResults = dirResults
  retorno$dirUtils = dirUtils
  retorno$dirDatasets = dirDatasets
  retorno$dirDO = dirDO
  retorno$dirFolds = dirFolds
  retorno$dirInfoFold = dirInfoFolds
  #retorno$dirReports = dirReports
  
  # return of the number of objects inside the folder
  retorno$n_Results = n_Results
  retorno$n_Utils = n_Utils
  retorno$n_Datasets = n_Datasets
  retorno$n_DO = n_DO
  retorno$n_Folds = n_Folds
  retorno$n_InfoFolds = n_InfoFolds
  #retorno$n_Reports = n_Reports
  
  return(retorno)
  gc()
}


##################################################################################################
# FUNCTION CREATING FOLDER PRINCIPALS                                                            #
#   Objective                                                                                    #
#       Creates the specific folders for the specific dataset                                    #
#   Parameters                                                                                   #
#       dataset_name: dataset name. It is used to create the folders.                            #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
creatingFoldersPrincipals<- function(dataset_name){
  
  diretorios = directories()
  
  retorno = list()
  
  folderDSFolds = paste(diretorios$folderFolds, "/", dataset_name, sep="")
  if(dir.exists(folderDSFolds) == TRUE){
    setwd(folderDSFolds)
    dir_DSFolds = dir(folderDSFolds)
    n_DSFolds = length(dir_DSFolds)
  } else {
    dir.create(folderDSFolds)
    setwd(folderDSFolds)
    dir_DSFolds = dir(folderDSFolds)
    n_DSFolds = length(dir_DSFolds)
  }
  
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
  
  folderResReports = paste(diretorios$folderReports, "/", dataset_name, sep="")
  if(dir.exists(folderResReports) == TRUE){
    setwd(folderResReports)
    dir_ResReports = dir(folderResReports)
    n_ResReports = length(dir_ResReports)
  } else {
    dir.create(folderResReports)
    setwd(folderResReports)
    dir_ResReports = dir(folderResReports)
    n_ResReports = length(dir_ResReports)
  }
  
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
  
  folderRandom1 = paste(folderResDataset, "/ClusRandom_1", sep="")
  if(dir.exists(folderRandom1) == TRUE){
    setwd(folderRandom1)
    dir_Random1 = dir(folderRandom1)
    n_Random1 = length(dir_Random1)
  } else {
    dir.create(folderRandom1)
    setwd(folderRandom1)
    dir_Random1 = dir(folderRandom1)
    n_Random1 = length(dir_Random1)
  }
  
  folderRandom2 = paste(folderResDataset, "/ClusRandom_2", sep="")
  if(dir.exists(folderRandom2) == TRUE){
    setwd(folderRandom2)
    dir_Random2 = dir(folderRandom2)
    n_Random2 = length(dir_Random2)
  } else {
    dir.create(folderRandom2)
    setwd(folderRandom2)
    dir_Random2 = dir(folderRandom2)
    n_Random2 = length(dir_Random2)
  }
  
  # return folders
  retorno$folderDSFolds = folderDSFolds
  retorno$folderResDataset = folderResDataset
  retorno$folderHClust = folderHClust
  retorno$folderHybPart = folderHybPart
  retorno$folderConfigFiles = folderConfigFiles
  retorno$folderLocal = folderLocal
  retorno$folderGlobal = folderGlobal
  retorno$folderHybrid = folderHybrid
  retorno$folderRandom1 = folderRandom1
  retorno$folderRandom2 = folderRandom2
  
  # return of folder contents
  retorno$dir_DSFolds = dir_DSFolds
  retorno$dir_ResDataset = dir_ResDataset
  retorno$dir_HClust = dir_HClust
  retorno$dir_HybPart = dir_HybPart
  retorno$dir__ConfigFiles = dir_ConfigFiles
  retorno$dir_Local = dir_Local
  retorno$dir_Global = dir_Global
  retorno$dir_Hybrid = dir_Hybrid
  retorno$dir_Random1 = dir_Random1
  retorno$dir_Random2 = dir_Random2
  
  # return of the number of objects inside the folder
  retorno$n_DSFolds = n_DSFolds
  retorno$n_ResDataset = n_ResDataset
  retorno$n_HClust = n_HClust
  retorno$n_HybPart = n_HybPart
  retorno$n_Local = n_Local
  retorno$n_Global = n_Global
  retorno$n_Hybrid = n_Hybrid
  retorno$n_Random1 = n_Random1
  retorno$n_Random2 = n_Random2
  
  return(retorno)
  
}


##################################################################################################
# FUNCTION CONVERT TO ARFF                                                                       #
#     Objective                                                                                  #
#        Convert csv file correctly to arff file                                                 #
#     Parameters                                                                                 #
#        arg1: existing csv file name                                                            #
#        arg 2: name of the arff file to be created                                              #
#        arg 3: specific number of labels that are part of the file. Example: starts at label    # 
#        30 and ends at label 50.                                                                #
#     Return:                                                                                    #
#        The arff file in the specific folder                                                    #
##################################################################################################
converteArff <- function(arg1, arg2, arg3){  
  str = paste("java -jar ", diretorios$folderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n\n")  
}



##################################################################################################
# FUNCTION CRIA PASTAS CLUS                                                                      #
#     Objective                                                                                  #
#       Creates folders to store the clus configuration files for running the global clus        #
#     Parameters                                                                                 #
#       dataset_name: dataset name. It is used to create the folders.                            #
#     Return                                                                                     #
#       Created folders                                                                          #
##################################################################################################
criaPastasCSC <- function(dataset_name){
  diretorios = directories()
  
  Pasta1 = paste(diretorios$folderCSC, "/", dataset_name, sep="")
  dir.create(Pasta1)
  print(Pasta1)
  cat("\n")    
  
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
#  Objective                                                                                     #
#     Gets the information that is in the "datsets.csv" file.                                    #  
#  Parameters                                                                                    #
#     dataset: the specific dataset                                                              #
#  Return                                                                                        #
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
#   Objective                                                                                    #
#     Add ".csv" in the file name                                                                #
#   Parameters                                                                                   #
#     fileNames: a vector with file names                                                        #
#   Return                                                                                       #
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
#   Objective                                                                                    #
#     Remove ".csv" from the file name                                                           #
#   Parameters                                                                                   #
#     fileNames: a vector with file names                                                        #
#   Return                                                                                       #
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
#   Objective                                                                                    #
#     Remove ".arff" from the file name                                                          #
#   Parameters                                                                                   #
#     fileNames: a vector with file names                                                        #
#   Return                                                                                       #
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