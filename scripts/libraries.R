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

library("readr") 
library("googledrive") 
library("foreign") 
library("stringr") 
library("mldr") 
library("plyr") 
library("dplyr") 
library("reshape2") 
library("AggregateR") 
library("philentropy") 
library("ggplot2") 
library("dendextend") 
library("ape")
library("pvclust") 
library("GGally") 
library("ggdendro")
library("cluster") 
library("lme4") 
library("parallel") 
library("utiml")
library("RWeka") 
library("rJava") 
library("foreach") 
library("doParallel") 
library("RColorBrewer") 
require("lattice")

##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################