##################################################################################################
# HPML-J                                                                                         #
##################################################################################################

##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)               #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
##################################################################################################

##################################################################################################
# Script 12 - HPMLJ                                                                              #
##################################################################################################

##################################################################################################
# SET WORKING DIRECTORY                                                                          #
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


##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
args <- commandArgs(TRUE)
cat(args, sep = "\n")


##################################################################################################
# LOAD RUN.R                                                                                     #
##################################################################################################
FolderScripts = paste(FolderRoot, "/scripts/", sep="")
setwd(FolderScripts)
source("run.R") 


##################################################################################################
# GET THE DIRECTORIES                                                                            #
##################################################################################################
diretorios <- directories()


##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
setwd(diretorios$folderDatasets)
datasets <- data.frame(read.csv("datasets.csv"))


##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[args[1],]


##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- args[2]


##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- args[3]


##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name <- toString(ds$Name) 


##################################################################################################
# to avoid conflicts, before starting to execute the code, it is necessary to delete the         #
# specific folder from the dataset, if it exists                                                 #
##################################################################################################
Folder1 = paste(diretorios$folderResults, "/", dataset_name, sep="") 
if(dir.exists(Folder1)==TRUE){
  setwd(Folder1)
  str10 = paste("rm -r ", dataset_name, sep="")
  system(str10)  
}


##################################################################################################
# execute the code and get the total execution time                                              #
# n_dataset, number_cores, number_folds                                                          #
##################################################################################################
timeFinal <- system.time(results <- executeHPMLJ(args[1], number_cores, number_folds))


##################################################################################################
# save the total time in rds format in the dataset folder                                        #
##################################################################################################
str0 <- paste(dataset_name, "time-final.rds", sep="")
setwd(Folder1)
save(timeFinal, file =str0)


##################################################################################################
# save results in RDATA form in the dataset folder                                               #
##################################################################################################
str1 <- paste(dataset_name, "-results.RData", sep="")
setwd(Folder1)
save(results, file =str1)


##################################################################################################
# save results in RDS form in the dataset folder                                                 #
##################################################################################################
str2 <- paste(dataset_name, "-results.rds", sep="")
setwd(Folder1)
write_rds(results, str2)


##################################################################################################
# compress the results for later transfer to the dataset folder                                  #
##################################################################################################
str3 <- paste("tar -zcvf ", dataset_name, "-results.tar.gz " , dataset_name, sep="")
setwd(Folder1)
system(str3)


##################################################################################################
# compress 10-folds for later transfer                                                           #
##################################################################################################
Folder3 = paste(diretorios$folderFolds, "/", dataset_name, sep="") 
setwd(Folder3)
str4 <- paste("tar -zcvf ", dataset_name, "-10-folds.tar.gz " , dataset_name, sep="")
system(str4)


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################