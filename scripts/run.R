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
# Script 13 - Run                                                                                #
##################################################################################################

##################################################################################################
# Java Options Configuration                                                                     #
##################################################################################################
options(java.parameters = "-Xmx16g")


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


##################################################################################################
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################

FolderScripts = paste(FolderRoot, "/scripts/", sep="")

cat("\nLoad Packages\n")
setwd(FolderScripts)
source("libraries.R")

cat("\nLoad Source Utils\n")
setwd(FolderScripts)
source("utils.R")

cat("\nLoad Source Pre-Processing\n")
setwd(FolderScripts)
source("preprocessing.R")

cat("\nLoad Source Miscellaneous\n")
setwd(FolderScripts)
source("miscellaneous.R")

cat("\nLoad Source Correlations\n")
setwd(FolderScripts)
source("correlations.R")

cat("\nLoad Source Hybrid Partitions\n")
setwd(FolderScripts)
source("partitions.R")

cat("\nLoad Source Clus Hybrid Partitions Validation\n")
setwd(FolderScripts)
source("clusValidation.R")

cat("\nLoad Source Clus Test Hybrid Partition\n")
setwd(FolderScripts)
source("clusHybrid.R")

cat("\nLoad Source Clus Test Global\n")
setwd(FolderScripts)
source("clusGlobal.R")

cat("\nLoad Source Clus Test Local\n")
setwd(FolderScripts)
source("clusLocal.R")

cat("\nLoad Source Clus Random 1\n")
setwd(FolderScripts)
source("clusRandom_1.R")

cat("\nLoad Source Clus Random 2\n")
setwd(FolderScripts)
source("clusRandom_2.R")

cat("\nLoad Source Clus Random 3\n")
setwd(FolderScripts)
source("clusRandom_3.R")


##################################################################################################
# Configures Scientific Notation                                                                #
##################################################################################################
cat("\nSet Global Scientific Notation\n")
options(scipen=30)


##################################################################################################
# Opens the file "datasets.csv"                                                                  #
##################################################################################################
cat("\nOpen Dataset Infomation File\n")
diretorios = directories()
setwd(diretorios$folderDatasets)
datasets = data.frame(read.csv("datasets.csv"))
n = nrow(datasets)
cat("\nTotal of Datasets: ", n, "\n")

cat("\nSet Workspace\n")
setwd(FolderRoot)


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
executeHPMLJ <- function(n_dataset, number_cores, number_folds){
  
  cat("\n\n################################################################################################")
  cat("\n# START EXECUTE HPML-J                                                                           #")
  cat("\n##################################################################################################\n\n") 
  
  diretorios = directories()
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n# Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n") 
    }
  }
  cl = cl
  
  retorno = list()
  DataSetInfo = list()
  Results = list()
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Get dataset information: ", n_dataset, "                                                  #")
  ds = datasets[n_dataset,]
  names(ds)[1] = "Id"
  info = infoDataSet(ds)
  dataset_name = toString(ds$Name)
  cat("\nDataset: ", dataset_name)   
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Generate folders                                                                          #")
  timeFolders = system.time(folders <- creatingFoldersPrincipals(dataset_name)) 
  folders = folders
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Generate and save fols for cross-validation                                               #")
  timeCV = system.time(resCV <- crossValidation(ds, dataset_name, number_folds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Verifiy datasets                                                                          #")
  timeVerify = system.time(resVerify <- verifyDataset(ds, dataset_name, number_folds, folders$folderDSFolds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Separate the label space                                                                  #")
  timeLabelSpace = system.time(resLS <- labelSpace(ds, dataset_name, number_folds, folders$folderDSFolds))
  namesLabels = resLS$NamesLabels
  cat("\n##################################################################################################\n\n") 
  
# ******************************************************************************************************************** #
  
  cat("\n\n################################################################################################")
  cat("\n# Run: RANDOM PARTITIONS VERSION 1                                                               #")
  timeRandom1 = system.time(clusRandom_1(ds, namesLabels, number_folds, dataset_name, 
                                         folders$folderDSFolds, folders$folderResDataset, 
                                         folders$folderRandom1, folders$folderReports))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: RANDOM PARTITIONS VERSION 2                                                               #")
  timeRandom2 = system.time(clusRandom_2(ds, namesLabels, dataset_name, number_folds,
                                         folders$folderDSFolds, folders$folderResDataset, 
                                         folders$folderRandom2, folders$folderReports))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: RANDOM PARTITIONS VERSION 3                                                               #")
  timeRandom3 = system.time(clusRandom_3(ds, dataset_name, number_folds, namesLabels, 
                                         folders$folderDSFolds, folders$folderResDataset, 
                                         folders$folderRandom3, folders$folderReports))
  cat("\n##################################################################################################\n\n") 
  
  # ******************************************************************************************************************** #
  
  cat("\n\n################################################################################################")
  cat("\n# Run: VALIDATION AND TEST HYBRID PARTITIONS                                                     #")
  cat("\n# Run: Calculate correlations using jaccard                                                      #")
  timeJaccard = system.time(computeJaccard(ds, dataset_name, number_folds, namesLabels, resLS, folders$folderHClust))
  
  cat("\n# Run: Calculate HClust and Cutree                                                               #")
  timeHClust = system.time(CutreeHClust(ds, dataset_name, number_folds, namesLabels, resLS, folders$folderHClust))
  
  cat("\n# Run: Choose the highest HClust coefficient                                                     #")
  timeBestCoef = system.time(bestCoefficient(number_folds, folders$folderHClust))
  
  cat("\n# Run: Builds groups for each of the selected partitions, for each of the folds and Validating in clus #")
  timeHybPart = system.time(hybridPartitions(ds, dataset_name, number_folds, folders$folderDSFolds, folders$folderHClust, folders$folderHybPart)) 
  
  cat("\n# Run: Clus Validation                                                                           #")
  timeHybVal = system.time(clusValidation(ds, dataset_name, number_folds, folders$folderHybPart, folders$folderReports))
  
  cat("\n# Run: Clus Test                                                                                 #")
  timeHybTest = system.time(clusHybrid(ds, dataset_name, number_folds, folders$folderDSFolds, folders$folderHClust, folders$folderHybPart, folders$folderHybrid, folders$folderReports))
  cat("\n# Run: END VALIDATION AND TEST HYBRID PARTITIONS                                                 #")
  cat("\n\n################################################################################################\n\n")

# ******************************************************************************************************************** #

  cat("\n\n################################################################################################")
  cat("\n# RUN: Clus Local                                                                               #")
  timeLocal = system.time(clusLocal(ds, dataset_name, number_folds, namesLabels,
                                    folders$folderResDataset, folders$folderLocal, 
                                    folders$folderReports))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Clus Global                                                                               #")
  timeGlobal = system.time(clusGlobal(ds, dataset_name, number_folds, 
                                      folders$folderResDataset, folders$folderConfigFiles, 
                                      folders$folderGlobal, folders$folderReport))
  cat("\n##################################################################################################\n\n") 
  
# ******************************************************************************************************************** #
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Compare the results                                                                       #")
  timeCompara = system.time(resCompara <- compareMethods(dataset_name, folders$folderRandom1, 
                                    folders$folderRandom2, folders$folderRandom3, 
                                    folders$folderLocal, folders$folderGlobal, 
                                    folders$folderHybrid, folders$folderResDataset, 
                                    folders$folderReports))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Count Partitions                                                                          #")
  timeResPart = system.time(resPart <- resumePartitions(ds, dataset_name, 
                                      folders$folderRandom1, folders$folderRandom2, 
                                      folders$folderRandom3, folders$folderHybPart, 
                                      folders$folderResDataset, folders$folderReports))  
  cat("\n##################################################################################################\n\n") 

  cat("\n\n################################################################################################")
  cat("\n# Run: Resume Hybrid Partitions                                                                  #")
  timeResume1 = system.time(resHP <- resumeHP(dataset_name, folders$folderHClust, folders$folderHybPart, folders$folderReports))  
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Delete unecessary files                                                                   #")
  FolderCF = folders$folderResDataset
  setwd(FolderCF)
  unlink("ConfigFiles", recursive = TRUE)
  
  #cat("\n deleting all folders and files generated")    
  #timeDelAll = system.time(deleteAll(number_folds, folders$folderHybrid, folders$folderHClust, 
  #                                       folders$folderRandom,folders$folderLocal, folders$folderGlobal, 
  #                                      folders$folderRandom))
  
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Save Runtime                                                                                   #")
  timesExecute = rbind(timeFolders, timeCV, timeVerify, timeLabelSpace, timeLocal, timeGlobal, timeRandom1,
                       timeRandom2, timeRandom3, timeJaccard, timeHClust, timeBestCoef, timeHybPart, timeHybVal,
                       timeHybTest, timeCompara, timeResume1, timeResPart)
  setwd(folders$folderReports)
  write.csv(timesExecute, "AllRunTime.csv")
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Stop Parallel                                                                              #")
  parallel::stopCluster(cl) 	
  cat("\n##################################################################################################\n\n") 
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF HPML-J. Thanks God!                                                                     #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################