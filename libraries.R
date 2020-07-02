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
# Script 1 - Libraries                                                                           #
# LAST UPDATE: 2020-06-10                                                                        #
##################################################################################################

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

library("readr")
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

##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################