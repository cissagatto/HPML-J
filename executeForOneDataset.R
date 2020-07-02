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
# LAST UPDATE: 2020-06-29                                                                        #
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

# obtem o argumento da linha de comando
args<-commandArgs(TRUE)
cat(args, sep = "\n")

# carrega o código
setwd(FolderRoot)
source("run.R") 

# obtem o diretório autal
diretorios <- directories()

# le o arquivo dataset com as informações de cada dataset
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets.csv"))

# obtem informações específicas do dataset
ds <- datasets[args[1],]

#pega o nome do dataset
dataset_name <- toString(ds$Name) 

# apaga a pasta especifica do dataset dentro da pasta do dataset
Folder1 = paste(FolderRoot, "/Results/", dataset_name, sep="") 
if(dir.exists(Folder1)==TRUE){
  setwd(Folder1)
  str10 = paste("rm -r ", dataset_name, sep="")
  system(str10)  
}

# executa o código e obtem o tempo de execução total
timeFinal <- system.time(results <- executa(args[1])) 

# salva o tempo total em formato rds na pasta do dataset
str0 <- paste(dataset_name, "time-final.rds", sep="")
setwd(Folder1)
save(timeFinal, file =str0)

# salva os resultados em forma RDATA na pasta do dataset
str1 <- paste(dataset_name, "-results.RData", sep="")
setwd(Folder1)
save(results, file =str1)

# salva os resultados em forma RDS na pasta do dataset
str2 <- paste(dataset_name, "-results.rds", sep="")
setwd(Folder1)
write_rds(results, str2)

# comprime os resultados para posterior transferencia na pasta do dataset
str3 <- paste("tar -zcvf ", dataset_name, "-results.tar.gz " , dataset_name, sep="")
setwd(Folder1)
system(str3)

# comprime os 10-folds para posterior transferencia 
Folder3 = paste(diretorios$folder10F, "/", dataset_name, sep="") 
setwd(Folder3)
str4 <- paste("tar -zcvf ", dataset_name, "-10-folds.tar.gz " , dataset_name, sep="")
system(str4)