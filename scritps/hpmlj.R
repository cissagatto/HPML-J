##################################################################################################
# HPML-J                                                                                         #
##################################################################################################

##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
##################################################################################################

##################################################################################################
# Script 13 - HPMLJ                                                                              #
##################################################################################################

##################################################################################################
# SET WORKING DIRECTORY                                                                          #
##################################################################################################
cat("\nSet Working Directory\n")
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
}
print(FolderRoot)

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

cat("\nCleaning the folder results\n")
if(dir.exists(Folder1)==TRUE){
  setwd(Folder1)
  str10 = paste("rm -r ", dataset_name, sep="")
  print(system(str10))
} else {
  cat("\nNao existe\n")
}

##################################################################################################
# save output                                                                                    #
##################################################################################################
#setwd(diretorios$folderResults)
#namae1 = paste("output-", dataset_name, ".txt", sep="")
#sink(namae1, type="output")


##################################################################################################
# execute the code and get the total execution time                                              #
# n_dataset, number_cores, number_folds                                                          #
##################################################################################################
cat("\nExecute HPMLJ\n")
timeFinal <- system.time(results <- executeHPMLJ(args[1], number_cores, number_folds))
#timeFinal <- system.time(results <- executeHPMLJ(9, 2, 10))


##################################################################################################
# save the total time in rds format in the dataset folder                                        #
##################################################################################################
cat("\nSave all runtime\n")
str0 <- paste(Folder1, "/", dataset_name, "time-final.rds", sep="")
str00 <- paste(dataset_name, "time-final.rds", sep="")
str000 <- paste("HPML-J/Resultados",  str00, sep="")
setwd(Folder1)
save(timeFinal, file = str0)
#drive_upload(str0, str000)


##################################################################################################
# save results in RDATA form in the dataset folder                                               #
##################################################################################################
cat("\nSave rdata\n")
str1 <- paste(Folder1, "/", dataset_name, "-results.RData", sep="")
str11 <- paste(dataset_name, "-results.RData", sep="")
str111 <- paste("HPML-J/Resultados",  str11, sep="")
setwd(Folder1)
save(results, file =str1)
save(timeFinal, file = str0)
#drive_upload(str1, str111)


##################################################################################################
# save results in RDS form in the dataset folder                                                 #
##################################################################################################
cat("\nSave rds\n")
str2 <- paste(Folder1, "/", dataset_name, "-results.rds", sep="")
str22 <- paste(dataset_name, "-results.rds", sep="")
str222 <- paste("HPML-J/Resultados",  str22, sep="")
setwd(Folder1)
write_rds(results, str2)
#drive_upload(str2, str222)


##################################################################################################
# compress the results for later transfer to the dataset folder                                  #
##################################################################################################
cat("\nCompress results\n")
str3 <- paste(Folder1, "/", dataset_name, "-results.tar.gz" , sep="")
str33 <- paste("tar -zcvf ", str3, " ", dataset_name, sep="")
str333 <- paste("HPML-J/Resultados", dataset_name, "-results.tar.gz" , sep="")
setwd(Folder1)
system(str33)
#drive_upload(str3, str333)

##################################################################################################
# compress 10-folds for later transfer                                                           #
##################################################################################################
cat("\ncompress datasets\n")
Folder3 <- paste(diretorios$folderFolds, "/", dataset_name, sep="") 
setwd(Folder3)
str4 <- paste("tar -zcvf ", dataset_name, "-10-folds.tar.gz " , dataset_name, sep="")
str44 = paste(Folder3, "/", dataset_name, "-10-folds.tar.gz", sep="")
str444 <- paste("HPML-J/Resultados", dataset_name, "-results.tar.gz" , sep="")
system(str4)
#drive_upload(str4, str444)

#sink()

##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################