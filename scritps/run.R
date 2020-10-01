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
# Script 12 - Run                                                                                #
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
# Runs for all datasets listed in the "datasets.csv" file                                       #
##################################################################################################
executeHPMLJ <- function(n_dataset, number_cores, number_folds){
  
  diretorios = directories()
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- makeCluster(number_cores, outfile="")
    registerDoParallel(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\nRunning Sequentially!")
      cat("\n################################################################################################\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\nRunning in parallel with ", number_cores, " cores!")
      cat("\n################################################################################################\n")
    }
  }
  cl = cl
  
  namae1 = paste("output-", n_dataset, ".txt", sep="")
  setwd(diretorios$folderResults)
  sink(namae1, type="output")
  
  retorno = list()
  DataSetInfo = list()
  Results = list()
  
  cat("\n\n################################################################################################")
  cat("\nGet dataset information: ", n_dataset)
  ds = datasets[n_dataset,]
  names(ds)[1] = "Id"
  info = infoDataSet(ds)
  dataset_name = toString(ds$Name)
  cat("\nDataset: ", dataset_name)   
  
  cat("\n\n################################################################################################")
  cat("\nGenerate folders")
  timeFolders = system.time(folders <- creatingFoldersPrincipals(dataset_name)) 
  folders = folders
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGenerate and save fols for cross-validation.")
  timeCV = system.time(resCV <- crossValidation(ds, dataset_name, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nVerifiy datasets")
  # folders$folderDSFolds --> specific dataset folder (train, test and validation)
  timeVerify = system.time(resVerify <- verifyDataset(folders$folderDSFolds, ds, dataset_name, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nSeparate the label space")
  timeLabelSpace = system.time(resLS <- labelSpace(ds, dataset_name, folders$folderDSFolds, number_folds))
  namesLabels = resLS$NamesLabels
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nClus Local")
  # folders$folderDSFolds --> specific folder of dataset results
  # folders$folderLocal --> results folder for local partitions
  timeLocal = system.time(clusLocal(ds, dataset_name, folders$folderResDataset, 
                                    folders$folderLocal, namesLabels, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nClus Global")
  # folders$folderGlobal --> results folder for global partitions
  # folders$folderConfigFiles --> folder containing the clus configuration files
  timeGlobal = system.time(clusGlobal(ds, dataset_name, folders$folderResDataset, 
                                      folders$folderConfigFiles, folders$folderGlobal, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCalculate correlations using jaccard.")
  # folders$folderHClust --> specific folder to store hclust and cutree results
  timeJaccard = system.time(computeJaccard(ds, resLS, dataset_name, namesLabels, 
                                           folders$folderHClust, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCalculate HClust and Cutree.")
  timeHClust = system.time(CutreeHClust(ds, resLS, dataset_name, namesLabels, 
                                        folders$folderHClust, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nChoose the highest HClust coefficient")
  timeBestCoef = system.time(bestCoefficient(folders$folderHClust, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nBuilds groups for each of the selected partitions, for each of the folds and Validating in clus")
  # folders$folderHybPart --> specific folder to store hybrid partition validation results
  timeHybPart = system.time(hybridPartitions(ds, dataset_name, folders$folderDSFolds, folders$folderHClust, 
                                             folders$folderHybPart, number_folds)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather the Predicts Hybrid Partition")
  timeGPHP = system.time(gatherPredsHybPart(ds, dataset_name, folders$folderHybPart, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nEvaluate Hybrid Partition")
  timeEHP = system.time(evalHybPart(ds, dataset_name, folders$folderHybPart, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather Evaluations")
  timeGEHP = system.time(gatherEvaluations(ds, dataset_name, folders$folderHybPart, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather F1 Macro")
  timeGF1 = system.time(gatherF1macro(ds, dataset_name, folders$folderHybPart, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nClus Test")
  # folders$folderHybrid --> specific folder to store the test results of the hybrid partitions
  timeTest = system.time(clusHybrid(ds, dataset_name, folders$folderDSFolds, folders$folderHClust, 
                                    folders$folderHybPart, folders$folderHybrid, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nDelete Hyb Part Files")
  timeDelHP = system.time(deleteHybPart(ds, folders$folderHybPart, number_folds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCompare the results")
  timeCompara = system.time(resCompara <- compareMethods(folders$folderLocal, folders$folderGlobal, 
                                                         folders$folderHybrid, folders$folderResDataset))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nResume")
  timeResume = system.time(resResume <- resumeResults(folders$folderHClust, folders$folderHybPart, 
                                                      folders$folderResDataset))  
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nAnalyse")
  timeResume = system.time(resAnalyse <- analysePartitions(ds, folders$folderHClust, namesLabels, number_folds))  
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nRuntime")
  timesExecute = rbind(timeFolders, timeCV, timeVerify, timeLabelSpace, timeJaccard, timeHClust, timeBestCoef, 
                       timeHybPart, timeGPHP, timeEHP, timeGEHP, timeGF1, timeDelHP, timeTest, timeLocal,
                       timeGlobal, timeCompara, timeResume)
  
  setwd(folders$folderResDataset)
  write.csv(timesExecute, "RunTime.csv")
  
  cat("\n\n################################################################################################")
  cat("\nDelete files")
  FolderCF = folders$folderResDataset
  setwd(FolderCF)
  unlink("ConfigFiles", recursive = TRUE)
  
  cat("\n\n################################################################################################")
  cat("\nStop Parallel")
  on.exit(stopCluster(cl))
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF HPML-J                                                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
  sink() 
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################