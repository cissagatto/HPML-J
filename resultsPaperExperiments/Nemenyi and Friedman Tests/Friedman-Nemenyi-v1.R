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
# FRIEDMAN AND NEMENYI - V2                                                                      #
##################################################################################################

library("tsutils")
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
FolderGraphics2 = paste(FolderGraphics, "/Critical Distances Version 2", sep="")
FolderRanking = paste(FolderFN, "/Ranking for Friedman", sep="")
FolderIntervals = paste(FolderFN, "/Intervals", sep="")
FolderNPValue = paste(FolderFN, "/NPValues", sep="")
FolderFPValues = paste(FolderFN, "/FPValues", sep="")
FolderMeans = paste(FolderFN, "/Means", sep="")
FolderDiff = paste(FolderFN, "/Diff", sep="")
FolderHypo = paste(FolderFN, "/Hypothesis", sep="")
FolderCD = paste(FolderFN, "/CriticalDistance", sep="")
FolderCL = paste(FolderFN, "/ConfidenceLevel", sep="")
FolderT = paste(FolderFN, "/Treat", sep="")
FolderO = paste(FolderFN, "/Observations", sep="")
FolderChiS = paste(FolderFN, "/ChiSquare", sep="")
FolderDegrees = paste(FolderFN, "/Degrees", sep="")

files = c(dir(FolderRanking))
files2 = c(RemoveCSV(files))
total = length(files)

means = data.frame()

Measure = c("")
NPValue = c(0)
npValue = data.frame(Measure,NPValue)

Measure = c("")
FPValues = c(0)
fpValues = data.frame(Measure, FPValues)

Measure = c("")
Hypothesis = c("")
Hypho = data.frame(Measure, Hypothesis)

Measure = c("")
CriticalDistance = c(0)
CD = data.frame(Measure, CriticalDistance)

Measure = c("")
ConfidenceLevel = c(0)
CL = data.frame(Measure, ConfidenceLevel)

Measure = c("")
Treat = c(0)
treatments = data.frame(Measure, Treat)

Measure = c("")
Obs = c(0)
Observations = data.frame(Measure, Obs)

Measure = c("")
ChiSquare = c(0)
CSq = data.frame(Measure, ChiSquare)

Measure = c("")
Degrees = c(0)
Dg = data.frame(Measure, Degrees)

measure = c(0)
difLH = c(0)
difGH = c(0)
LCD = c(0)
GCD = c(0)
different = data.frame(measure, difLH, difGH, LCD, GCD)

i = 1
while(i<=total){
  
  cat("\n measure:", files2[i])
  
  setwd(FolderRanking)
  Ranking= data.frame(read.csv(files[i]))
  Ranking = Ranking[,-1]
  Ranking = as.matrix(Ranking)
  f = friedman.test(Ranking)

  setwd(FolderGraphics2)
  pdf(paste(files2[i], "-CD-2.pdf", sep=""), width = 4, height = 4)
  n = nemenyi(Ranking, conf.level = 0.95, plottype='vline', sort = TRUE)
  dev.off()
  
  setwd(FolderIntervals)
  write.csv(n$intervals, paste(files2[i], "-Intervals.csv", sep=""))
  
  setwd(FolderNPValue)
  Measure = files2[i]
  NPValue = n$fpval
  npValue = rbind(npValue, data.frame(Measure, NPValue))
  write.csv(npValue, "NPValues.csv")
  
  #setwd(FolderMeans)
  #teste = data.frame(n$means)
  #teste2 = data.frame(t(teste))
  #rownames(teste2) = files2[i]
  #means = rbind(means, teste2)
  #write.csv(means, "Means.csv", append = TRUE)
  
  setwd(FolderDiff)
  res = round(n$means,2)
  difLH = as.numeric(res[2] - res[1])
  difGH = as.numeric(res[3] - res[1])
  LCD = toString(difLH >= n$cd)
  GCD = toString(difGH >= n$cd)
  measure = files2[i]
  different = rbind(different, data.frame(measure,difLH, difGH, LCD, GCD))
  write.csv(different, "different.csv")
  
  setwd(FolderHypo)
  Measure = files2[i]
  Hypothesis = n$fH
  Hypho = rbind(Hypho, data.frame(Measure, Hypothesis))
  write.csv(Hypho, "hypothesis.csv")

  setwd(FolderCD)
  Measure = files2[i]
  CriticalDistance = n$cd
  CD = rbind(CD, data.frame(Measure, CriticalDistance))
  write.csv(CD, "CriticalDistance.csv")
  
  setwd(FolderCL)
  Measure = files2[i]
  ConfidenceLevel = n$conf.level
  CL = rbind(CL, data.frame(Measure, ConfidenceLevel))
  write.csv(CL, "ConfidenceLevel.csv")
  
  setwd(FolderT)
  Measure = files2[i]
  Treat = n$k
  treatments = rbind(treatments, data.frame(Measure, Treat))
  write.csv(treatments, "Treatments.csv")
  
  setwd(FolderO)
  Measure = files2[i]
  Obs = n$n
  Observations = rbind(Observations, data.frame(Measure, Obs))
  write.csv(Observations, "Observations.csv")
  
  setwd(FolderChiS)
  Measure = files2[i]
  ChiSquare = as.numeric(f$statistic)
  CSq = rbind(CSq, data.frame(Measure, ChiSquare))
  write.csv(CSq, "ChiSquare.csv")
  
  setwd(FolderDegrees)
  Measure = files2[i]
  Degrees = as.numeric(f$parameter)
  Dg = rbind(Dg, data.frame(Measure, Degrees))
  write.csv(Dg, "Degrees.csv")
  
  setwd(FolderFPValues)
  Measure = files2[i]
  FPValues = as.numeric(f$p.value)
  fpValues = rbind(fpValues, data.frame(Measure, FPValues))
  write.csv(fpValues, "FPValues.csv")
  
  i = i + 1
  gc()
}