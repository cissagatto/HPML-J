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
# Script 3 - Preprocessing                                                                      #
##################################################################################################

##################################################################################################
# Set working directory                                                                          #
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


##################################################################################################
# Scientific notation setup                                                                      #
##################################################################################################
options(scipen=30)

##################################################################################################
# DATA FRAMES                                                                                    #
##################################################################################################
nomeDataset = c(0)
num.attributes = c(0)
num.instances = c(0)
num.inputs = c(0)
num.labels = c(0)
num.labelsets = c(0)
num.single.labelsets = c(0)
max.frequency = c(0)
cardinality = c(0)
density = c(0)
meanIR = c(0)
scumble = c(0)
scumble.cv = c(0)
tcs = c(0)
informacoesDatasets = data.frame(nomeDataset, num.attributes, num.instances, num.inputs, num.labels, 
                                 num.labelsets, num.single.labelsets, max.frequency, cardinality, 
                                 density, meanIR, scumble, scumble.cv, tcs)


##################################################################################################
# FUNCTION CROSS VALIDATION                                                                      #
#   Objective                                                                                    #
#      Creates the folds of the cross validation                                                 #  
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds to be created                                              #
#   Return                                                                                       #
#       k-folds test, train and validation                                                       #
##################################################################################################
crossValidation <- function(ds, dataset_name, number_folds){ 
  
  sf = setFolder()
  FolderRoot = sf$Folder
  
  # get the directories
  diretorios = directories()
  
  # creates the folder to store the 10-fold files
  FolderCV = paste(diretorios$folderFolds, "/", dataset_name, sep="")
  if(dir.exists(FolderCV)==TRUE){
    cat("\n")
  } else {
    dir.create(FolderCV)  
  }
  
  # creates the folder to save training folds
  FolderTr = paste(FolderCV, "/Tr", sep="")
  if(dir.exists(FolderTr)==TRUE){
    cat("\n")
  } else {
    dir.create(FolderTr)  
  }
  
  # creates the folder to save testing folds
  FolderTs = paste(FolderCV, "/Ts", sep="")
  if(dir.exists(FolderTs)==TRUE){
    cat("\n")
  } else {
    dir.create(FolderTs)  
  }
  
  # creates the folder to save validating folds
  FolderVl = paste(FolderCV, "/Vl", sep="")
  if(dir.exists(FolderVl)==TRUE){
    cat("\n")
  } else {
    dir.create(FolderVl)  
  }
  
  cat("\nOpen dataset file with mldr\n")
  setwd(diretorios$folderDO)
  arquivo = mldr(dataset_name)
  
  cat("\nGetting the names labels\n")
  x = paste(dataset_name, ".arff", sep="")
  arquivo2 = data.frame(read.arff(x))
  colnames(arquivo2)
  nomesRotulos = c(colnames(arquivo2[ds$LabelStart:ds$LabelEnd]))
  
  cat("\nCreates folds for cross-validation\n")
  set.seed(1234)
  cvdata <- create_kfold_partition(arquivo, number_folds, "iterative")
  cvDataFolds = cvdata$fold
  
  cat("\nSaves cross validation in RDS format\n")
  setwd(FolderCV)
  write_rds(cvdata, "crossvalidation.rds")
  
  # from the first fold to the last
  i = 1
  while(i<=number_folds){
    
    cat("\nFOLD ", i)
    
    # get the specific fold
    FoldSpecific = partition_fold(cvdata, i, has.validation = TRUE)
    
    #########################################################
    cat("\n\tTrain ", i, "\n")
    setwd(FolderTr)
    
    inicio = ds$LabelStart
    fim = ds$LabelEnd
    
    cat("\n\t\tTRAIN: separates the measurements and the testing FOLD\n")
    treino_rds = FoldSpecific$train
    treino_ds = FoldSpecific$train$dataset
    treino_ds$.labelcount = NULL
    treino_ds$.SCUMBLE = NULL
    treino_ds = data.frame(treino_ds)
    
    cat("\n\t\tTRAIN: Save CSV")
    str_csv_treino = paste(dataset_name, "-Split-Tr-", i, ".csv", sep="")
    write.csv(treino_ds, str_csv_treino, row.names = FALSE)
    
    cat("\n\t\tTRAIN: Convert, and save, CSV to ARFF")
    str_arff_treino = paste(dataset_name, "-Split-Tr-", i, ".arff", sep="")
    arg1Tr = str_csv_treino
    arg2Tr = str_arff_treino
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr)
    
    cat("\n\t\tTRAIN: Verify and correct {0} and {1}\n")
    arquivo = paste(FolderTr, "/", str_arff_treino, sep="")
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
    print(system(str0))
    
    cat("\n\t\tTRAIN: saves measurement information from FOLD")
    str_t3 = paste(dataset_name, "-Split-Tr-", i, sep="")
    nomeDataset = str_t3
    medidasTr = cbind(nomeDataset, data.frame(FoldSpecific$train$measures))
    
    
    #########################################################
    cat("\n\tTest ", i, "\n")
    setwd(FolderTs)
    
    cat("\n\t\tTEST: separates the measurements and the testing FOLD\n")
    teste_rds = FoldSpecific$test
    teste_ds = FoldSpecific$test$dataset
    teste_ds$.labelcount = NULL
    teste_ds$.SCUMBLE = NULL
    teste_ds = data.frame(teste_ds)       
    
    cat("\n\t\tTEST: Save CSV\n")
    str_csv_teste = paste(dataset_name, "-Split-Ts-", i, ".csv", sep="")
    write.csv(teste_ds, str_csv_teste, row.names = FALSE)
    
    cat("\n\t\tTEST: Convert, and save, CSV to ARFF\n")
    str_arff_teste = paste(dataset_name, "-Split-Ts-", i, ".arff", sep="")
    arg1Tr = str_csv_teste
    arg2Tr = str_arff_teste
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr)
    
    cat("\n\t\tTEST: Verify and correct {0} and {1}\n")
    arquivo = paste(FolderTs, "/", str_arff_teste, sep="")
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
    print(system(str0))
    
    cat("\n\t\tTEST: saves measurement information from FOLD\n")
    str_t7 = paste(dataset_name, "-Split-Ts-", i, sep="")
    nomeDataset = str_t7
    medidasTs = cbind(nomeDataset, data.frame(FoldSpecific$test$measures))
    
    #########################################################
    cat("\n\tValidation ", i, "\n")
    setwd(FolderVl)
    
    cat("\n\t\tVALIDATION: separates the measurements and the testing FOLD\n")
    val_rds = FoldSpecific$validation
    val_ds = FoldSpecific$validation$dataset
    val_ds$.labelcount = NULL
    val_ds$.SCUMBLE = NULL
    val_ds = data.frame(val_ds)
    
    cat("\n\t\tVALIDATION: Save CSV\n")
    str_csv_val = paste(dataset_name, "-Split-Vl-", i, ".csv", sep="")
    write.csv(val_ds, str_csv_val, row.names = FALSE)
    
    cat("\n\t\tVALIDATION: Convert, and save, CSV to ARFF\n")
    str_arff_val = paste(dataset_name, "-Split-Vl-", i, ".arff", sep="")
    arg1Tr = str_csv_val
    arg2Tr = str_arff_val
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr)
    
    cat("\n\t\tVALIDATION: Verify and correct {0} and {1}\n")
    arquivo = paste(FolderVl, "/", str_arff_val, sep="")
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
    print(system(str0))
    
    cat("\n\t\tVALIDATION: saves measurement information from FOLD\n")
    str_11 = paste(dataset_name, "-Split-Vl-", i, sep="")
    nomeDataset = str_11
    medidasVl = cbind(nomeDataset, data.frame(FoldSpecific$validation$measures))
    
    ###########################################################
    cat("\n\t\tSaves measurement information from ALL FOLDS\n")
    informacoesDatasets = rbind(informacoesDatasets, medidasTr, medidasTs, medidasVl)
    setwd(diretorios$folderInfoFolds)
    ab = paste("Info-Fold-", dataset_name, ".csv", sep="")
    write.csv(informacoesDatasets, ab, row.names = FALSE)
    
    i = i + 1
    gc()
  }    
  gc()
  cat("\n##################################################################################################")
  cat("\n# FUNCTION CROSS VALIDATION: END                                                                 #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION VERIFY DATASET                                                                        #
#   Objective                                                                                    #
#     Checks the number of instances per label in each fold                                      #  
#   Parameters                                                                                   #
#       Folder: folder where the folds are                                                       #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#   Return:                                                                                      #
#       dataframes with the frequencies of each label for each of the training, test and         # 
#       validation folds.                                                                        #
##################################################################################################
verifyDataset <- function(Folder, ds, dataset_name, number_folds){
  
  #Folder = folders$folderDSFolds
  retorno = list()
  
  FolderTR = paste(Folder, "/Tr", sep="")
  FolderTS = paste(Folder, "/Ts", sep="")
  FolderVL = paste(Folder, "/Vl", sep="")
  
  frequenciaTR = list()
  frequenciaTS = list()
  frequenciaVL = list()
  
  cat("\nAnalyze train folds")
  frequencia = c(0)
  somaTR = data.frame(frequencia)
  f = 1
  while(f<=number_folds){
    cat("\n\tFold: ", f)
    setwd(FolderTR)
    nome_arquivo = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    dataset = data.frame(read.csv(nome_arquivo))
    classes = dataset[,ds$LabelStart:ds$LabelEnd]
    soma = data.frame(apply(classes, 2, sum))
    names(soma) = paste("Fold-", f, sep="")
    somaTR = cbind(somaTR, soma)
    f = f + 1
    gc()
  }
  somaTR2 = somaTR
  somaTR3 = somaTR2[,-1]
  
  cat("\nAnalyze test folds")
  frequencia = c(0)
  somaTS = data.frame(frequencia)
  f = 1
  while(f<=number_folds){
    cat("\n\tFold: ", f)
    setwd(FolderTS)
    nome_arquivo = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    dataset = data.frame(read.csv(nome_arquivo))
    classes = dataset[,ds$LabelStart:ds$LabelEnd]
    soma = data.frame(apply(classes, 2, sum))
    names(soma) = paste("Fold-", f, sep="")
    somaTS = cbind(somaTS, soma)
    f = f + 1
    gc()
  }
  somaTS2 = somaTS
  somaTS3 = somaTS2[,-1]
  
  cat("\nAnalyze validation folds")
  frequencia = c(0)
  somaVL = data.frame(frequencia)
  f = 1
  while(f<=number_folds){
      cat("\n\tFold: ", f)
      setwd(FolderVL)
      nome_arquivo = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
      dataset = data.frame(read.csv(nome_arquivo))
      classes = dataset[,ds$LabelStart:ds$LabelEnd]
      soma = data.frame(apply(classes, 2, sum))
      names(soma) = paste("Fold-", f, sep="")
      somaVL = cbind(somaVL, soma)
      f = f + 1
      gc() 
  }
  somaVL2 = somaVL
  somaVL3 = somaVL2[,-1]
  
  retorno$FrequencyTR = somaTR3
  retorno$FrequencyTS = somaTS3
  retorno$FrequencyVL = somaVL3
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# FUNCTION VERIFY DATASET: END                                                                   #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION LABEL SPACE                                                                           #
#   Objective                                                                                    #
#       Separates the label space from the rest of the data to be used as input for              # 
#       calculating correlations                                                                 #                                                                                        
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Training set labels space                                                                #
##################################################################################################
labelSpace <- function(ds, dataset_name, Folder, number_folds){
  #Folder = pastas$folderDS10F
  retorno = list()
  classes = list()
  
  # from the first FOLD to the last
  k = 1
  while(k<=number_folds){
    cat("\n\tFold: ", k)
    setwd(Folder)
    FolderTR = paste(Folder, "/Tr", sep="")
    setwd(FolderTR)
    nome_arquivo = paste(dataset_name, "-Split-Tr-", k, ".csv", sep="")
    arquivo = data.frame(read.csv(nome_arquivo))
    classes[[k]] = arquivo[,ds$LabelStart:ds$LabelEnd]
    namesLabels = c(colnames(classes[[k]]))
    k = k + 1 # increment FOLD
    gc() # garbage collection
  } # End While of the 10-folds
  
  retorno$NamesLabels = namesLabels
  retorno$Classes = classes
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# FUNCTION LABEL SPACE: END                                                                      #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################