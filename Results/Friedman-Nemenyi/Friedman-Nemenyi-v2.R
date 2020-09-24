# SÓ É POSSÍVEL USAR ESTE SCRITP EM VERSÕES ABAIXO DO R 4.0

library("scmamp")
library("ggplot2")
library("Rgraphviz")


setwd("C:/Users/elain/Documents/Doutorado/My Papers/SCA-2021/Results")
RankingF1 = data.frame(read.csv("ranking-f1.csv"))
RankingR = data.frame(read.csv("ranking-recall.csv"))
RankingP = data.frame(read.csv("ranking-precision.csv"))
RankingA = data.frame(read.csv("ranking-accuracy.csv"))
RankingHL = data.frame(read.csv("ranking-hamming.csv"))

fF1 = friedmanTest(RankingF1[,-1])
fR = friedmanTest(RankingR[,-1])
fP = friedmanTest(RankingP[,-1])
fA = friedmanTest(RankingA[,-1])
fHL = friedmanTest(RankingHL[,-1])

nF1 = nemenyiTest (RankingF1[,-1], alpha=0.05)
nR = nemenyiTest (RankingR[,-1], alpha=0.05)
nP = nemenyiTest (RankingP[,-1], alpha=0.05)
nA = nemenyiTest (RankingA[,-1], alpha=0.05)
nHL = nemenyiTest (RankingHL[,-1], alpha=0.05)

plotCD(RankingF1[,-1], alpha=0.05, cex=1)
plotCD(RankingR[,-1], alpha=0.01, cex=1)
plotCD(RankingP[,-1], alpha=0.01, cex=1)
plotCD(RankingA[,-1], alpha=0.01, cex=1)
plotCD(RankingHL[,-1], alpha=0.01, cex=1)


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

# GRAFICO DE DENSIDADE
plotDensities(RankingF1[,-1])
plotDensities(RankingR[,-1])
plotDensities(RankingP[,-1])
plotDensities(RankingA[,-1])
plotDensities(RankingHL[,-1])

# GRAFICO DE P VALUES
plotPvalues(nF1$diff.matrix)
plotPvalues(nP$diff.matrix)
plotPvalues(nR$diff.matrix)
plotPvalues(nA$diff.matrix)
plotPvalues(nHL$diff.matrix)


# RANKING
plotRanking()
             
Measure = c(0)
CD = c(0)
DF = c(0)
FCS = c(0)
DF_F = c(0)
p_value = c(0)
results = data.frame(Measure, CD, DF, FCS, DF_F, p_value)

Measure = "Hamming Loss"
CD = as.numeric(nHL$statistic)
DF_N = as.numeric(nR$parameter)
FCS = as.numeric(fHL$statistic)
DF_F = as.numeric(fHL$parameter)
p_value = as.numeric(fHL$p.value)
results = rbind(results, data.frame(Measure, CD, DF, FCS, DF_F, p_value))
results = results[-1,]
colnames(results) = c("Measures", "Critical Distance", 
                      "Nemenyi F Distribution", "Friedman's Chi-Squared", 
                      "Friedman F Distribution", "P Value")

library(xtable)
xtable(results)

nF1$statistic
nF1$parameter[2]
nF1$method
nF1$data.name
nF1$diff.matrix

fF1$statistic
fF1$parameter
fF1$method
fF1$p.value
fF1$data.name


# Uma lista com a classe "htest" contendo os seguintes componentes:
# estatística: o valor da estatística usada no teste;
# método: uma cadeia de caracteres indicando que tipo de teste foi realizado;
# data.name: uma cadeia de caracteres que fornece o nome dos dados e
# diff.matirx: uma matriz com todas as diferenças em pares das classificações médias

# Uma lista com a classe "htest" contendo os seguintes componentes:
# estatística: o valor da estatística usada no teste;
# parâmetro: os dois graus de liberdade da distribuição F;
# p.value: o valor p para o teste;
# método: uma cadeia de caracteres indicando que tipo de teste foi realizado e
# data.name: uma cadeia de caracteres que fornece o nome dos dados.