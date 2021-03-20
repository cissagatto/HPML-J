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
# Script 13 - HPMLJ                                                                              #
##################################################################################################

##################################################################################################
# SET WORKING DIRECTORY                                                                          #
##################################################################################################
#cat("\nSet Working Directory\n")
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
}
write.table(sistema, "info_sistema.csv")
#print(FolderRoot)


##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
cat("\nArgs Command Line\n")
args <- commandArgs(TRUE)
cat(args, sep = "\n")


##################################################################################################
# LOAD RUN.R                                                                                     #
##################################################################################################
cat("\nLoad Scripts\n")
FolderScripts = paste(FolderRoot, "/scripts/", sep="")
setwd(FolderScripts)
source("run.R") 


##################################################################################################
# GET THE DIRECTORIES                                                                            #
##################################################################################################
cat("\nGet directories\n")
diretorios <- directories()


##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
cat("\nOpen datasets.csv\n")
setwd(diretorios$folderDatasets)
datasets <- data.frame(read.csv("datasets.csv"))


##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[as.numeric(args[1]),]


##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])


##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])


##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name <- toString(ds$Name) 


##################################################################################################
# to avoid conflicts, before starting to execute the code, it is necessary to delete the         #
# specific folder from the dataset, if it exists                                                 #
##################################################################################################
Folder1 = paste(diretorios$folderResults, "/", dataset_name, sep="") 
cat("\n", Folder1)


##################################################################################################
# execute the code and get the total execution time                                              #
# n_dataset, number_cores, number_folds                                                          #
##################################################################################################
cat("\nExecute HPMLJ\n")
timeFinal <- system.time(results <- executeHPMLJ(args[1], number_cores, number_folds))
print(timeFinal)

#timeFinal <- system.time(results <- executeHPMLJ(17, number_cores, number_folds))

##################################################################################################
# save the total time in rds format in the dataset folder                                        #
##################################################################################################
cat("\nSave Rds\n")
str0 <- paste(FolderRoot, "/", dataset_name, "-results.rds", sep="")
setwd(FolderRoot)
save(results, file = str0)


##################################################################################################
# save results in RDATA form in the dataset folder                                               #
##################################################################################################
cat("\nSave Rdata\n")
str1 <- paste(FolderRoot, "/", dataset_name, "-results.RData", sep="")
setwd(FolderRoot)
save(results, file = str1)


##################################################################################################
# save results in RDS form in the dataset folder                                                 #
##################################################################################################
#cat("\nSave runtime\n")
#setwd(FolderRoot)
#write(timeFinal, paste(dataset_name, "final-runtime.csv", sep=""))


##################################################################################################
# compress the results for later transfer to the dataset folder                                  #
##################################################################################################
cat("\nCompress results\n")
Folder =  paste(diretorios$folderResults, "/", dataset_name, sep="")
str3 = paste("tar -zcvf ", dataset_name, "-results.tar.gz ", Folder, sep="")
print(system(str3))


##################################################################################################
# copy file                                                                                      #
##################################################################################################
cat("\nCopy file tar \n")
str4 = paste("cp ", Folder, "/", dataset_name, "-results.tar.gz ", FolderRoot, sep="")
print(system(str4))


##################################################################################################
# del                                                                                      #
##################################################################################################
cat("\nDelete folder \n")
str5 = paste("rm -r ", Folder, sep="")
print(system(str5))


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################