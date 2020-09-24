library("tsutils")

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

RankingF1 = data.frame(read.csv("ranking-f1.csv"))
RankingF1 = RankingF1[,-1]
RankingF1 = as.matrix(RankingF1)
ft1 = friedman.test(RankingF1)
nt1 = nemenyi(RankingF1, conf.level = 0.95, plottype='vline', sort = TRUE)

RankingP = data.frame(read.csv("ranking-precision.csv"))
RankingP = RankingP[,-1]
RankingP = as.matrix(RankingP)
ft2 = friedman.test(RankingP)
nt2 = nemenyi(RankingP, plottype='vline', sort = TRUE)

RankingR = data.frame(read.csv("ranking-recall.csv"))
RankingR = RankingR[,-1]
RankingR = as.matrix(RankingR)
ft3 = friedman.test(RankingR)
nt3 = nemenyi(RankingR, plottype='vline', sort = TRUE)

RankingA= data.frame(read.csv("ranking-accuracy.csv"))
RankingA = RankingA[,-1]
RankingA = as.matrix(RankingA)
ft3 = friedman.test(RankingA)
nt3 = nemenyi(RankingA, plottype='vline', sort = TRUE)

RankingHL = data.frame(read.csv("ranking-hamming.csv"))
RankingHL = RankingHL[,-1]
RankingHL = as.matrix(RankingHL)
ft3 = friedman.test(RankingHL)
nt3 = nemenyi(RankingHL, plottype='vline', sort = TRUE)


resF1 = apply(RankingF1, 2,  mean)
round(resF1,2)
difF1A = resF1[2] - resF1[1]
difF1B = resF1[3] - resF1[1]
difF1C = resF1[4] - resF1[1]
round(difF1C, 2)
aF1 = difF1A >= nt1$cd
bF1 = difF1B >= nt1$cd
cF1 = difF1C >= nt1$cd

resP = apply(RankingP, 2,  mean)
round(resP,2)
difPA = resP[2] - resP[1]
difPB = resP[3] - resP[1]
difPC = resP[4] - resP[1]
round(difPC, 2)
aP = difPA >= nt2$cd
bP = difPB >= nt2$cd
cP = difPC >= nt2$cd

resR = apply(RankingR, 2,  mean)
round(resR,2)
difRA = resR[2] - resR[1]
difRB = resR[3] - resR[1]
difRC = resR[4] - resR[1]
round(difRC, 2)
aR = difRA >= nt3$cd
bR = difRB >= nt3$cd
cR = difRC >= nt3$cd


RF1 = data.frame(read.csv("RankingF1-2.csv"))
names(RF1)[1] = "Dataset"
HPML = RF1 [order(RF1$HPML, decreasing = FALSE),]
LOCAL = RF1 [order(RF1$Local, decreasing = FALSE),]
BR = RF1 [order(RF1$BR, decreasing = FALSE),]
GLOBAL = RF1 [order(RF1$Global, decreasing = FALSE),]





# means: mean rank of each treatment.
nt1$means 

# fpavl: Friedman test p-value.
nt1$fpval

# intervals: intervals within there is no evidence of significance difference 
# according to the Nemenyi test at requested confidence level.
nt1$intervals

# fH: Friedman test hypothesis outcome.
nt1$fH

# cd: Nemenyi critical distance. Output intervals is calculate as means +/- cd.
nt1$cd

# conf.level: confidence level used for testing
nt1$conf.level

# k: number of treatments (columns).
nt1$k

# n: number of observations (rows).
nt1$n

# statistic	 the value of Friedman's chi-squared statistic.
ft1$statistic

# parameter	 the degrees of freedom of the approximate chi-squared distribution of the test statistic.
ft1$parameter

# p.value	 the p-value of the test.
ft1$p.value

# method	 the character string "Friedman rank sum test".
ft1$method

# data.name	 a character string giving the names of the data.
ft1$data.name


RankingF1 = data.frame(read.csv("Macro-F1.csv"))
names(RankingF1)[1] = "Dataset"
RankingF1 = RankingF1[,-1]
RankingF1 = as.matrix(RankingF1)
ft1 = friedman.test(RankingF1)
nt1 = nemenyi(RankingF1, conf.level = 0.95, plottype='vline', sort = TRUE)


RankingP = data.frame(read.csv("Macro-Precision.csv"))
names(RankingP)[1] = "Dataset"
RankingP = RankingP[,-1]
RankingP = as.matrix(RankingP)
ft2 = friedman.test(RankingP)
nt2 = nemenyi(RankingP, plottype='vline', sort = TRUE)

RankingR = data.frame(read.csv("Macro-Recall.csv"))
names(RankingR)[1] = "Dataset"
RankingR = RankingR[,-1]
RankingR = as.matrix(RankingR)
ft3 = friedman.test(RankingR)
nt3 = nemenyi(RankingR, plottype='vline', sort = TRUE)
