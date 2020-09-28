# HPML-J

This code is a part of my doctoral research at PPG-CC/DC/UFSCar. HPML-J is the name of the first experiment carried out: Hybrid Partitions for Multi-Label Classification with index Jaccard. Other two versions will be available soon: HPML-KN and HPML-KT (K = kohonen, N = normal, T = transpose).

## Paper
Label Correlation for Partitioning the Label Space in MLC
(or Exploring Label Correlations for Partitioning the Label Space in Multi-label Classification)

## Multi-Label Datasets
You can download the multi-label datasets in this link: https://cometa.ujaen.es/datasets/

## Scripts
This source code consists of a R project for R Studio and the following R scripts, folders and files:

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
11. run.R
12. executeForOneDataset.R
13. analysisResults.R

## External Libraries

Please, after your download, confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff. jar and weka.jar. Without this libraries the code not runs. 

## File "datasets.csv"

A file called "datasets.csv" must be in the root folder. This file is used to read informations about the dataset and they are used in the code. All 74 datasets available in cometa are in this file. If you want to use another dataset, please, add the following information about the dataset in the file:

_Id, Name, Domain, Labels, Instances, Attributes, Inputs, Labelsets, Single, Max freq, Card, Dens, MeanIR, Scumble, TCS, AttStart, AttEnd, LabelStart, LabelEnd_

The "Id" of the dataset is a mandatory parameter (_n_dataset_) in the command line to run all code. The "LabelStart" and "LabelEnd" are used in a lot of internal functions. Please, make sure that these information are available before run the code.

## Requirements
This code was develop in R Studio Version 1.3.959 © 2009-2020, PBC, "Middlemist Red" (3a09be39, 2020-05-18) for Windows. The R language version was 4.0.1 (2020-06-06) with x86_64-w64-mingw32 plataform.

This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (_number_cores_). If number_cores = 0 the code will run sequentially. In our experiments, we used ten cores. For reproducibility, we recommend that you also use ten cores.


## Process flow

## Experimental Setup

## Results


