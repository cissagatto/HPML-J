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

cat("\nLoad Source Clus Random\n")
setwd(FolderScripts)
source("clusRandom.R")

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
  
  #n_dataset = 17
  #number_cores = 2
  #number_folds = 10
  
  cat("\n\n################################################################################################")
  cat("\n# START EXECUTE HPML-J                                                                           #")
  cat("\n##################################################################################################\n\n") 
  
  diretorios = directories()
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    # pra rodar no servidor do mauri precisa comentar estas 2 linhas
    # cl <- makeCluster(number_cores, outfile="")
    # registerDoParallel(cl)
    # inserir esta
    # registerDoParallel(number_cores)
    # print(cl)
    
    # Example registering clusters
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
  
  #namae1 = paste("output-", n_dataset, ".txt", sep="")
  setwd(diretorios$folderResults)
  #sink(namaegetwd()1, type="output")
  
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
  timeVerify = system.time(resVerify <- verifyDataset(folders$folderDSFolds, ds, dataset_name, number_folds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Separate the label space                                                                  #")
  timeLabelSpace = system.time(resLS <- labelSpace(ds, dataset_name, folders$folderDSFolds, number_folds))
  namesLabels = resLS$NamesLabels
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Clus Local                                                                               #")
  timeLocal = system.time(clusLocal(ds, dataset_name, folders$folderResDataset, 
                                    folders$folderLocal, namesLabels, number_folds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Clus Global                                                                               #")
  timeGlobal = system.time(clusGlobal(ds, dataset_name, folders$folderResDataset, folders$folderConfigFiles, 
                                      folders$folderGlobal, number_folds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: VALIDATION AND TEST RANDOM PARTITIONS                                                     #")
  cat("\n# RUN: Clus Random                                                                               #")
  timeRandom = system.time(clusRandom(ds, namesLabels, dataset_name, folders$folderDSFolds, 
                                      folders$folderResDataset, folders$folderRandom, number_folds))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: VALIDATION AND TEST HYBRID PARTITIONS                                                     #")
  cat("\n# Run: Calculate correlations using jaccard                                                      #")
  timeJaccard = system.time(computeJaccard(ds, resLS, dataset_name, namesLabels, 
                                           folders$folderHClust, number_folds))
  
  cat("\n# Run: Calculate HClust and Cutree                                                               #")
  timeHClust = system.time(CutreeHClust(ds, resLS, dataset_name, namesLabels, 
                                        folders$folderHClust, number_folds))
  
  cat("\n# Run: Choose the highest HClust coefficient                                                     #")
  timeBestCoef = system.time(bestCoefficient(folders$folderHClust, number_folds))
  
  cat("\n# Run: Builds groups for each of the selected partitions, for each of the folds and Validating in clus #")
  timeHybPart = system.time(hybridPartitions(ds, dataset_name, folders$folderDSFolds,  folders$folderHClust, 
                                             folders$folderHybPart, number_folds)) 
  
  cat("\n# Run: Clus Validation                                                                           #")
  timeValidation = system.time(clusValidation(ds, dataset_name, number_folds, folders$folderHybPart))
  
  cat("\n# Run: Clus Test                                                                                 #")
  timeHybTest = system.time(clusHybrid(ds, dataset_name, number_folds, folders$folderDSFolds, folders$folderHClust,
                                    folders$folderHybPart, folders$folderHybrid))
  
  cat("\n# Run: END VALIDATION AND TEST HYBRID PARTITIONS                                                 #")
  cat("\n\n################################################################################################\n\n")
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Compare the results                                                                       #")
  timeCompara = system.time(resCompara <- compareMethods(folders$folderRandom, folders$folderLocal, folders$folderGlobal, 
                                                         folders$folderHybrid, folders$folderResDataset))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Resume                                                                                    #")
  timeResume = system.time(resResume <- resumeResults(folders$folderHClust,  
                                                      folders$folderHybPart,  folders$folderResDataset))  
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Analyse                                                                                   #")
  timeAnalyse = system.time(resAnalyse <- analysePartitions(ds, folders$folderHClust, 
                                                            namesLabels, number_folds))  
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Runtime                                                                                   #")
  timesExecute = rbind(timeFolders, timeCV, timeVerify, timeLabelSpace, timeLocal, timeGlobal, timeRandom,
                       timeJaccard, timeHClust, timeHybPart, timeValidation, timeHybTest, 
                       timeCompara, timeResume, timeAnalyse)
  #timesExecute = rbind(timeFolders, timeCV, timeVerify, timeLabelSpace, timeTest)
  setwd(folders$folderResDataset)
  write.csv(timesExecute, "RunTime.csv")
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Delete files                                                                              #")
  FolderCF = folders$folderResDataset
  setwd(FolderCF)
  unlink("ConfigFiles", recursive = TRUE)
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Stop Parallel                                                                              #")
  # no servidor do mauri tem que comentar esta linha aqui
  on.exit(stopCluster(cl))
  #parallel::stopCluster(cl)
  cat("\n##################################################################################################\n\n") 
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF HPML-J                                                                                  #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
  #sink() 
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################