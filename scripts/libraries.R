##################################################################################################
# HPML-J                                                                                         #
# Hybrid Partitions for Multi-label Classification version Jaccard                               #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This code is distributed in the hope       #
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
# Script 1 - Libraries                                                                           #
##################################################################################################

#################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
shm = 0
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  shm = 1
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  shm = 0
}
shm = shm
setwd(FolderRoot)

# folder SCRIPTS
FolderScripts = paste(FolderRoot, "/scripts/", sep="")

# folder shm
FolderSHM = "/dev/shm/"


##################################################################################################
# LOAD EXTERNAL LIBRARIES                                                                        #
##################################################################################################

#library("googledrive") 
library("readr", quietly = TRUE) 
library("foreign", quietly = TRUE) 
library("stringr", quietly = TRUE) 
library("mldr", quietly = TRUE) 
library("plyr", quietly = TRUE) 
library("dplyr", quietly = TRUE) 
library("reshape2", quietly = TRUE) 
library("AggregateR", quietly = TRUE) 
library("philentropy", quietly = TRUE) 
library("ggplot2", quietly = TRUE) 
library("dendextend", quietly = TRUE) 
library("ape", quietly = TRUE)
library("pvclust", quietly = TRUE) 
library("GGally", quietly = TRUE) 
library("ggdendro", quietly = TRUE)
library("cluster", quietly = TRUE) 
library("lme4", quietly = TRUE) 
library("parallel", quietly = TRUE) 
library("utiml", quietly = TRUE)
library("RWeka", quietly = TRUE) 
library("rJava", quietly = TRUE) 
library("foreach", quietly = TRUE) 
library("doParallel", quietly = TRUE) 
library("RColorBrewer", quietly = TRUE) 
library("lattice", quietly = TRUE)
library("cluster", quietly = TRUE)

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################