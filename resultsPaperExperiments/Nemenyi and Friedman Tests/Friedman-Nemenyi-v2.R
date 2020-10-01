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
# FRIEDMAN AND NEMENYI - v1                                                                      #
##################################################################################################

library("scmamp")
library("ggplot2")
library("Rgraphviz")
library("stringr")

RemoveCSV <- function(files){
  files2 = files
  j = 0
  for(j in 1:length(files2)){
    a = str_length(files2[j])
    a = a - 4
    files2[j] = str_sub(files2[j], end = a)  
    j = j + 1
    gc()
  }  
  return(files2)
}

sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
}

FolderResults = paste(FolderRoot, "/ExperimentsResults", sep="")
FolderFN = paste(FolderResults, "/Nemenyi and Friedman Tests", sep="")
FolderGraphics = paste(FolderFN, "/Graphics", sep="")
FolderGraphics2 = paste(FolderGraphics, "/Critical Distances Version 1", sep="")
FolderGraphics3 = paste(FolderGraphics, "/Density", sep="")
FolderGraphics4 = paste(FolderGraphics, "/pValues", sep="")
FolderRanking = paste(FolderFN, "/Ranking for Friedman", sep="")

files = c(dir(FolderRanking))
files2 = c(RemoveCSV(files))
total = length(files)

i = 1
while(i<=total){
  cat("\n measure:", files2[i])
  setwd(FolderRanking)
  Ranking = data.frame(read.csv(files[i]))
  Ranking2 = Ranking[,-1]
  fr = friedmanTest(Ranking2)
  ne = nemenyiTest(Ranking2, alpha=0.05)
  
  setwd(FolderGraphics2)
  pdf(paste(files2[i], "-CD.pdf", sep=""), width = 4, height = 4)
  plotCD(Ranking2, alpha=0.05, cex=1)
  dev.off()
  
  setwd(FolderGraphics3)
  pdf(paste(files2[i], "-Density.pdf", sep=""), width = 4, height = 2)
  plotDensities(Ranking2)
  dev.off()
  
  setwd(FolderGraphics4)
  pdf(paste(files2[i], "-pValues.pdf", sep=""), width = 5, height = 2.5)
  plotPvalues(ne$diff.matrix)
  dev.off()
  
  i = i + 1
  gc()
  
}