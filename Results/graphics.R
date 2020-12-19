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
# GRAPHICS                                                                    #
##################################################################################################

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

FolderGraphics = paste(FolderAnalysis, "/Graphics", sep="")
if(dir.exists(FolderGraphics)==FALSE){
  dir.create(FolderGraphics)
} 

Folder = paste(FolderGraphics, "/Others", sep="")
if(dir.exists(Folder)==FALSE){
  dir.create(Folder)
} 

# C:\Users\elain\Documents\Doutorado\My Papers\IJCNN-2021\Experiments Results\Evaluation
setwd("C:/Users/elain/Documents/Doutorado/My Papers/IJCNN-2021/Experiments Results/Evaluation")
medias = data.frame(read.csv("average-measures.csv"))  
names(medias)[1] = "medidas"
nomesLinhas = medias[,1]
medias2 = medias[,-1]
a = as.matrix(medias2)

j = 1
for(j in 1:22){
  
  b = medias[j,]
  b2 = b[,-1]
  a = as.matrix(b2)
  
  cols <- c("#ce93d8", "#ab47bc", "#8e24aa")
  
  setwd(Folder)
   
  nome = paste(nomesLinhas[j], ".pdf", sep="")
  cat("\n ", nome)
  
  pdf(nome, width = 10, height = 10)
  
  bp <- barplot(a, 
          beside = TRUE, 
          width = 0.5, 
          ylim = c(0, 1), 
          las = 1,
          cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5, cex.names = 2.5,
          args.legend = list(x = "top", bty = "n", nol = 3),
          col = cols)
  box(bty = "L")
  text(x=c(bp), y=t(a), labels=a, pos=3, cex=2.5)
  
  print(bp)
  dev.off()
  cat("\n")

  j = j + 1  
}