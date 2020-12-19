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

##################################################################################################
# CREATE FOLDERS                                                                                 # 
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

FolderAnalysis = paste(FolderRoot, "/Analysis", sep="")
if(dir.exists(FolderAnalysis)==FALSE){
  dir.create(FolderAnalysis)
} 

# please, put here the csvs files with the ranking for nemenyi and friedman
FolderRanking = paste(FolderAnalysis, "/CSVs", sep="")
if(dir.exists(FolderRanking)==FALSE){
  dir.create(FolderRanking)
} 

FolderGraphics = paste(FolderAnalysis, "/Graphics", sep="")
if(dir.exists(FolderGraphics)==FALSE){
  dir.create(FolderGraphics)
} 

FolderCD1 = paste(FolderGraphics, "/CriticalDistances1", sep="")
if(dir.exists(FolderCD1)==FALSE){
  dir.create(FolderCD1)
} 

FolderCD2 = paste(FolderGraphics, "/CriticalDistances2", sep="")
if(dir.exists(FolderCD2)==FALSE){
  dir.create(FolderCD2)
} 

FolderDensity = paste(FolderGraphics, "/Density", sep="")
if(dir.exists(FolderDensity)==FALSE){
  dir.create(FolderDensity)
} 

FolderNPV = paste(FolderGraphics, "/NPV", sep="")
if(dir.exists(FolderNPV)==FALSE){
  dir.create(FolderNPV)
} 

FolderIntervals = paste(FolderAnalysis, "/Intervals", sep="")
if(dir.exists(FolderIntervals)==FALSE){
  dir.create(FolderIntervals)
} 

FolderNPValue = paste(FolderAnalysis, "/NPValues", sep="")
if(dir.exists(FolderNPValue)==FALSE){
  dir.create(FolderNPValue)
} 

FolderFPValues = paste(FolderAnalysis, "/FPValues", sep="")
if(dir.exists(FolderFPValues)==FALSE){
  dir.create(FolderFPValues)
} 

FolderMeans = paste(FolderAnalysis, "/Means", sep="")
if(dir.exists(FolderMeans)==FALSE){
  dir.create(FolderMeans)
} 

FolderDiff = paste(FolderAnalysis, "/Diff", sep="")
if(dir.exists(FolderDiff)==FALSE){
  dir.create(FolderDiff)
} 

FolderHypo = paste(FolderAnalysis, "/Hypothesis", sep="")
if(dir.exists(FolderHypo)==FALSE){
  dir.create(FolderHypo)
} 

FolderCD = paste(FolderAnalysis, "/CriticalDistance", sep="")
if(dir.exists(FolderCD)==FALSE){
  dir.create(FolderCD)
} 

FolderCL = paste(FolderAnalysis, "/ConfidenceLevel", sep="")
if(dir.exists(FolderCL)==FALSE){
  dir.create(FolderCL)
} 

FolderT = paste(FolderAnalysis, "/Treat", sep="")
if(dir.exists(FolderT)==FALSE){
  dir.create(FolderT)
} 

FolderO = paste(FolderAnalysis, "/Observations", sep="")
if(dir.exists(FolderO)==FALSE){
  dir.create(FolderO)
} 

FolderChiS = paste(FolderAnalysis, "/ChiSquare", sep="")
if(dir.exists(FolderChiS)==FALSE){
  dir.create(FolderChiS)
} 

FolderDegrees = paste(FolderAnalysis, "/Degrees", sep="")
if(dir.exists(FolderDegrees)==FALSE){
  dir.create(FolderDegrees)
} 

##################################################################################################
#
##################################################################################################
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
  
  setwd(FolderCD1)
  pdf(paste(files2[i], "-CD.pdf", sep=""), width = 4, height = 2)
  print(plotCD(Ranking2, alpha=0.05, cex=1))
  dev.off()
  
  setwd(FolderDensity)
  pdf(paste(files2[i], "-Density.pdf", sep=""), width = 4, height = 2)
  print(plotDensities(Ranking2))
  dev.off()
  
  setwd(FolderNPV)
  pdf(paste(files2[i], "-pValues.pdf", sep=""), width = 5, height = 2.5)
  print(plotPvalues(ne$diff.matrix))
  dev.off()
  
  i = i + 1
  gc()
  
}