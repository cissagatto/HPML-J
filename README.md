# HPML-J

This code is a part of my doctoral research at PPG-CC/DC/UFSCar. HPML-J is the name of the first experiment carried out: Hybrid Partitions for Multi-Label Classification with index Jaccard. Other two versions will be available soon: HPML-KN and HPML-KT (K = kohonen, N = normal, T = transpose).

## Paper
Exploring Label Correlations for Partitioning the Label Space in Multi-label Classification

## Multi-Label Datasets
You can download the multi-label datasets in this link: https://cometa.ujaen.es/datasets/

## Scripts
This source code consists of a R project for R Studio and the following R scripts:

01. libraries.R
02. utils.R
03. preprocessing.R
04. miscellaneous.R
05. correlations.R
06. partitions.R
07. clusValidation.R
08. clusHybrid.R
09. clusGlobal.R
10. clusLocal.R
11. clusRandom.R
12. run.R
13. hpmlj.R

## Jars
Confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff.jar and weka.jar. Without this jars the code not runs. 

## Datasets Folder
After downloading the dataset you want to use, place it in the */HPML-J/Datasets/Originals* folder. Don't forget that the .xml and .arff files of the respective dataset are needed.

## Folder Path
Place a copy of this code in _"C:/Users/[username]/HPML-J"_ or _"/home/username/HPML-J"_. Our files are configured to obtain the paths of the folders from the root. You can change this in the code if you want.

## File "datasets.csv"
A file called "datasets.csv" must be in the *datasets* folder. This file is used to read informations about the dataset and they are used in the code. All 74 datasets available in cometa are in this file. If you want to use another dataset, please, add the following information about the dataset in the file:

_Id, Name, Domain, Labels, Instances, Attributes, Inputs, Labelsets, Single, Max freq, Card, Dens, MeanIR, Scumble, TCS, AttStart, AttEnd, LabelStart, LabelEnd_

The _"Id"_ of the dataset is a mandatory parameter (_n_dataset_) in the command line to run all code. The "LabelStart" and "LabelEnd" are used in a lot of internal functions. Please, make sure that these information are available before run the code.

## Folder Strucutre
<img src="https://raw.githubusercontent.com/cissagatto/HPML-J/master/Images/estrutura_hpmj.PNG?token=AC7DF5OGNEZDZOKK4BEBPZC72TRQC">


## Software Requirements
This code was develop in R Studio Version 1.3.959 © 2009-2020, PBC, "Middlemist Red" (3a09be39, 2020-05-18) for Windows. The R language version was 4.0.1 (2020-06-06) with x86_64-w64-mingw32 plataform.

## Hardware Requirements
This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (_number_cores_). If *number_cores = 1* the code will run sequentially. In our experiments, we used ten cores. For reproducibility, we recommend that you also use ten cores.

Important: we used the CLUS classifier in this experiment. This implies generating all physical ARFF training, validating and testing files for each of the generated hybrid partitions. Our code generates the partitions first in memory and then saves them to the HD. However, to avoid memory problems, immediately after saving to HD, the files are validated (or tested) and then deleted. Even so, make sure you have enough space on your HD and RAM for this procedure.

## RUN
To run the code, open the terminal, enter the */HPML-J/scripts/* folder and type

```
Rscript hpmlj.R [number_dataset] [number_cores] [number_folds]
```

Where:

_number_dataset_ is the dataset number in the datasets.csv file

_number_cores_ is the total cores you want to use in parallel execution.

_number_folds_ is the number of folds you want for cross-validation

All parameters are mandatory. Example:

```
Rscript hpmlj.R 17 5 10
```

## Saved Results
This code save some important results in csv files. 

1. _Random Partitions_: The random partitions are saved in "HPML-J\results\[dataset_name]\ClusRandom" in a file named "all-partitions.csv"
 
2. _Hybrid Partitions_: The hybrid partitions are saved in "\HPML-J\results\[dataset_name]\HClust\Split-X", where X is the number of the fold. The file names are "partitions-average.csv", "partitions-complete.csv" and "partitions-single.csv"
  
3. _Chosen Partitions_: The chosen hybrid and random partitions with the best macro F1 for each fold is saved in "\HPML-J\results\[dataset_name]\HybridPartition", and "\HPML-J\results\[dataset_name]\ClusRandom. The file name is "BestF1Macro.csv".
  
4. _Evaluation measures for each partitions_: The values of each measures are saved in "HPML-J\results\[dataset_name]\ClusHybrid" for hybrid partitions, "HPML-J\results\[dataset_name]\ClusRandom\Test" for random partitions, "HPML-J\results\[dataset_name]\ClusGlobal" for global partitions, and "HPML-J\results\[dataset_name]\ClusLocal" for local partitions. The file name is "SummaryFoldsEvaluated.csv"

5. _Compared partitions_: The file "ResultsFinal.csv" in "C:\Users\elain\HPML-J\results\[dataset_name]" contain the values for each evaluation measure in each partition.

Other results are saved. You can explore the folders to see or, if you are not interesting, then you can delete all.

## Paper Results
The results obtained for the paper can be found in the folder "resultsPaperExperiments". In this folder are the spreadsheets with the results for all the evaluation measures, as well as the files needed for the statistical tests of Friedman and Nemenyi. The graphics are also in this folder.



