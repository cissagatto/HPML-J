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
# Script 12 - Run                                                                                #
# LAST UPDATE: 2020-06-28                                                                        #
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
  FolderRoot = paste("/home/", sistema[7], "/GLPML", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/GLPML", sep="")
  setwd(FolderRoot)
}
setwd(FolderRoot)
dir(FolderRoot)

##################################################################################################
# LIBRARIES                                                                                      #
##################################################################################################
cat("\nLoad Packages\n")
setwd(FolderRoot)
source("libraries.R")

cat("\nLoad Source Utils\n")
setwd(FolderRoot)
source("utils.R")

cat("\nLoad Source Pre-Processing\n")
setwd(FolderRoot)
source("preprocessing.R")

cat("\nLoad Source Miscellaneous\n")
setwd(FolderRoot)
source("miscellaneous.R")

cat("\nLoad Source Correlations\n")
setwd(FolderRoot)
source("correlations.R")

cat("\nLoad Source Partitions\n")
setwd(FolderRoot)
source("partitions.R")

cat("\nLoad Source Clus Validation\n")
setwd(FolderRoot)
source("clusValidation.R")

cat("\nLoad Source Clus Test Hybrid Partition\n")
setwd(FolderRoot)
source("clusHybrid.R")

cat("\nLoad Source Clus Test Global\n")
setwd(FolderRoot)
source("clusGlobal.R")

cat("\nLoad Source Clus Test Local\n")
setwd(FolderRoot)
source("clusLocal.R")

cat("\nLoad Source Binary Relevance\n")
setwd(FolderRoot)
source("BinRel.R")


##################################################################################################
# Configures Scientific Notation                                                                #
##################################################################################################
cat("\nSet Global Scientific Notation\n")
options(scipen=30)


##################################################################################################
# Opens the file "datasets.csv"                                                                  #
##################################################################################################
cat("\nSet Workspace\n")
setwd(FolderRoot)

cat("\nOpen Dataset Infomation File\n")
datasets = data.frame(read.csv("datasets.csv"))

n = nrow(datasets)
cat("\nTotal of Datasets: ", n, "\n")

cat("\nSet Global Directory\n")
diretorios = directories()


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                       #
##################################################################################################
executa <- function(i){
  
  cat("\n\n################################################################################################")
  cat("\nStart Parallel")
  cl <- makeCluster(10, outfile="")
  registerDoParallel(cl)
  
  namae1 = paste("output-", i, ".txt", sep="")
  setwd(FolderRoot)
  sink(namae1, type="output")
  
  retorno = list()
  DataSetInfo = list()
  Results = list()
  diretorios = directories()
  
  
  cat("\n\n################################################################################################")
  cat("\nGet dataset information: ", i)
  ds = datasets[i,]
  names(ds)[1] = "Id"
  info = infoDataSet(ds)
  dataset_name = toString(ds$Name)
  cat("\nDataset: ", dataset_name)   
  
  cat("\n\n################################################################################################")
  cat("\nGenerate folders")
  timeFolders = system.time(folders <- creatingFoldersPrincipals(dataset_name)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGenerate and save 10-folds for cross-validation.")
  timeCV = system.time(resCV <- crossValidation(ds, dataset_name))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nVerifiy datasets")
  timeVerify = system.time(resVerify <- verificaDataset(folders$folderDS10F, ds, dataset_name))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nSeparate the label space")
  timeLabelSpace = system.time(resLS <- labelSpace(ds, dataset_name, folders$folderDS10F))
  namesLabels = resLS$NamesLabels
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nClus Local")
  timeLocal = system.time(clusLocal(ds, dataset_name, folders$folderResDataset, folders$folderLocal, namesLabels))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nBinary Relavance")
  timeBR= system.time(BinaryRelevance(ds, dataset_name, folders$folderDS10F, folders$folderBR))  
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nClus Global")
  timeGlobal = system.time(clusGlobal(ds, dataset_name, folders$folderResDataset, folders$folderConfigFiles, folders$folderGlobal))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCalculate correlations using jaccard.")
  timeJaccard = system.time(computeJaccard(ds, resLS, dataset_name, namesLabels, folders$folderHClust))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCalculate HClust and Cutree.")
  timeHClust = system.time(CutreeHClust(ds, resLS, dataset_name, namesLabels, folders$folderHClust))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nChoose the highest HClust coefficient")
  timeBestCoef = system.time(bestCoefficient(folders$folderHClust))
  cat("\n\n################################################################################################")
  
  Folder10F = folders$folderDS10F
  cat("\n\n################################################################################################")
  cat("\nBuilds groups for each of the selected partitions, for each of the folds and Validating in clus")
  timeHybPart = system.time(hybridPartitions(ds, dataset_name, Folder10F, folders$folderHClust, folders$folderHybPart)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather the Predicts Hybrid Partition")
  timeGPHP = system.time(gatherPredsHybPart(ds, dataset_name, folders$folderHybPart))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nEvaluate Hybrid Partition")
  timeEHP = system.time(evalHybPart(ds, dataset_name, folders$folderHybPart))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather Evaluations")
  timeGEHP = system.time(gatherEvaluations(ds, dataset_name, folders$folderHybPart))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nGather F1 Macro")
  timeGF1 = system.time(gatherF1macro(ds, dataset_name, folders$folderHybPart))
  cat("\n\n################################################################################################")
  
  Folder10F = folders$folderDS10F
  cat("\n\n################################################################################################")
  cat("\nClus Test")
  timeTest = system.time(clusHybrid(ds, dataset_name, Folder10F, folders$folderHClust, folders$folderHybPart, folders$folderHybrid))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nDelete Hyb Part Files")
  timeDelHP = system.time(deleteHybPart(ds, folders$folderHybPart))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nCompare the results")
  timeCompara = system.time(resCompara <- compara(folders$folderLocal, folders$folderGlobal, folders$folderHybrid, folders$folderBR, folders$folderResDataset, ds, dataset_name))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\nResume")
  timeResume = system.time(resResume <- resume(folders$folderHClust, folders$folderHybPart, folders$folderResDataset))  
  cat("\n\n################################################################################################")
  
  #########################################################################################  
  cat("\n\n################################################################################################")
  cat("\nRuntime")
  timesExecute = rbind(timeFolders, timeCV, timeVerify, timeLabelSpace, timeJaccard, timeHClust, timeBestCoef, 
                       timeHybPart, timeGPHP, timeEHP, timeGEHP, timeGF1, timeDelHP, timeTest, timeLocal, timeBR, 
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
  cat("\n# END OF EXECUTION - DEUS SEJA LOUVADO                                                           #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
  sink() 
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################