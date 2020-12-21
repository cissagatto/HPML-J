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
# Script 1 - Libraries                                                                           #
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
# LOAD EXTERNAL LIBRARIES                                                                        #
##################################################################################################
library("readr") #
library("googledrive") #
library("foreign") #
library("stringr") #
library("mldr") #
library("plyr") #
library("dplyr") #instalado
library("reshape2") #NAO
library("AggregateR") #instalado
library("philentropy") #instalado
library("ggplot2") #NAO
library("dendextend") #NAO
library("ape") #instalado
library("pvclust") #instalado
library("GGally") #instalado
library("ggdendro") #instalado
library("cluster") #instaldo
library("lme4") #instalado 
library("parallel") #NAO
library("utiml") # não instalado
library("RWeka") #instalado
library("rJava") #instalado
library("foreach") #instalado
library("doParallel") #instalado
require("lattice") # instalado 
library("RColorBrewer") # instalado 

##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################