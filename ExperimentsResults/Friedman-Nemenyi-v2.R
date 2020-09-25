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
# FRIEDMAN AND NEMENYI                                                                           #
##################################################################################################

library("scmamp")
library("ggplot2")
library("Rgraphviz")

sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/HPML-J", sep="")
  setwd(FolderRoot)
}

Folder = paste(FolderRoot, "/ExperimentsResults", sep="")
setwd(Folder)

RankingF1 = data.frame(read.csv("friedman-nemenyi-f1.csv"))
RankingR = data.frame(read.csv("friedman-nemenyi-recall.csv"))
RankingP = data.frame(read.csv("friedman-nemenyi-precision.csv"))
RankingA = data.frame(read.csv("friedman-nemenyi-accuracy.csv"))
RankingHL = data.frame(read.csv("friedman-nemenyi-hamming.csv"))
RankingSA = data.frame(read.csv("friedman-nemenyi-subset.csv"))

fF1 = friedmanTest(RankingF1[,-1])
fR = friedmanTest(RankingR[,-1])
fP = friedmanTest(RankingP[,-1])
fA = friedmanTest(RankingA[,-1])
fHL = friedmanTest(RankingHL[,-1])
fSA = friedmanTest(RankingSA[,-1])

nF1 = nemenyiTest (RankingF1[,-1], alpha=0.05)
nR = nemenyiTest (RankingR[,-1], alpha=0.05)
nP = nemenyiTest (RankingP[,-1], alpha=0.05)
nA = nemenyiTest (RankingA[,-1], alpha=0.05)
nHL = nemenyiTest (RankingHL[,-1], alpha=0.05)
nSA = nemenyiTest (RankingSA[,-1], alpha=0.05)

Folder2 = paste(Folder, "/Graphics", sep="")
setwd(Folder2)

pdf("F1CD.pdf", width = 4, height = 1.5)
plotCD(RankingF1[,-1], alpha=0.05, cex=1)
dev.off()

pdf("RCD.pdf", width = 4, height = 1.5)
plotCD(RankingR[,-1], alpha=0.05, cex=1)
dev.off()

pdf("PCD.pdf", width = 4, height = 1.5)
plotCD(RankingP[,-1], alpha=0.05, cex=1)
dev.off()

pdf("ACD.pdf", width = 4, height = 1.5)
plotCD(RankingA[,-1], alpha=0.05, cex=1)
dev.off()

pdf("HLCD.pdf", width = 4, height = 1.5)
plotCD(RankingHL[,-1], alpha=0.05, cex=1)
dev.off()

pdf("SACD.pdf", width = 4, height = 1.5)
plotCD(RankingHL[,-1], alpha=0.05, cex=1)
dev.off()

pdf("F1-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingF1[,-1])
dev.off()

pdf("R-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingR[,-1])
dev.off()

pdf("P-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingP[,-1])
dev.off()

pdf("A-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingA[,-1])
dev.off()

pdf("HL-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingHL[,-1])
dev.off()

pdf("SA-Density.pdf", width = 5, height = 2.5)
plotDensities(RankingSA[,-1])
dev.off()

pdf("F1-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nF1$diff.matrix)
dev.off()

pdf("P-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nP$diff.matrix)
dev.off()

pdf("R-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nR$diff.matrix)
dev.off()

pdf("A-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nA$diff.matrix)
dev.off()

pdf("HL-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nHL$diff.matrix)
dev.off()

pdf("SA-pValues.pdf", width = 5, height = 2.5)
plotPvalues(nSA$diff.matrix)
dev.off()