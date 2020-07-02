# In-between Global and Local Partitions for Multi-label Classification
This code is part of my PhD research project. 

## Multi-Label Datasets

| Id | Name                 | Instances | Attributes | Inputs | Labels | Labelsets | Single | Max freq | Card     | Dens    | Mean IR      | Scumble   | TCS      | AttStart | AttEnd | LabelStart | LabelEnd |
|----|----------------------|-----------|------------|--------|--------|-----------|--------|----------|----------|---------|--------------|-----------|----------|----------|--------|------------|----------|
| 1  | bibtex               | 7395      | 1995       | 1836   | 159    | 2856      | 2199   | 471      | 24\.019  | 0\.0151 | 124\.983     | 0\.0938   | 205\.414 | 1        | 1836   | 1837       | 1995     |
| 2  | birds                | 645       | 279        | 260    | 19     | 133       | 73     | 294      | 1\.014   | 0\.0534 | 5\.407       | 0\.033    | 133\.955 | 1        | 260    | 261        | 279      |
| 3  | bookmarks            | 87856     | 2358       | 2150   | 208    | 18716     | 14971  | 6087     | 20\.281  | 0\.0098 | 12\.308      | 0\.0597   | 228\.479 | 1        | 2150   | 2151       | 2358     |
| 4  | cal500               | 502       | 242        | 68     | 174    | 502       | 502    | 1        | 260\.438 | 0\.1497 | 205\.778     | 0\.3372   | 155\.972 | 1        | 68     | 69         | 242      |
| 5  | corel5k              | 5000      | 873        | 499    | 374    | 3175      | 2523   | 55       | 3\.522   | 0\.0094 | 1\.895\.676  | 0\.3941   | 201\.999 | 1        | 499    | 500        | 873      |
| 6  | corel16k001          | 13766     | 653        | 500    | 153    | 4803      | 3120   | 253      | 28\.587  | 0\.0187 | 341\.552     | 0\.2731   | 19\.722  | 1        | 500    | 501        | 653      |
| 7  | corel16k002          | 13761     | 664        | 500    | 164    | 4868      | 3103   | 251      | 28\.824  | 0\.0176 | 376\.781     | 0\.2883   | 198\.049 | 1        | 500    | 501        | 664      |
| 8  | corel16k003          | 13760     | 654        | 500    | 154    | 4812      | 3069   | 258      | 28\.286  | 0\.0184 | 37\.058      | 0\.285    | 197\.304 | 1        | 500    | 501        | 654      |
| 9  | corel16k004          | 13837     | 662        | 500    | 162    | 4860      | 3112   | 250      | 2\.842   | 0\.0175 | 358\.989     | 0\.2772   | 19\.791  | 1        | 500    | 501        | 662      |
| 10 | corel16k005          | 13847     | 660        | 500    | 160    | 5034      | 3293   | 252      | 28\.577  | 0\.0179 | 349\.364     | 0\.285    | 198\.138 | 1        | 500    | 501        | 660      |
| 11 | corel16k006          | 13859     | 662        | 500    | 162    | 5009      | 3239   | 176      | 28\.849  | 0\.0178 | 333\.979     | 0\.2905   | 198\.212 | 1        | 500    | 501        | 662      |
| 12 | corel16k007          | 13915     | 674        | 500    | 174    | 5158      | 3389   | 254      | 28\.859  | 0\.0166 | 377\.146     | 0\.2821   | 19\.922  | 1        | 500    | 501        | 674      |
| 13 | corel16k008          | 13864     | 668        | 500    | 168    | 4956      | 3169   | 253      | 2\.883   | 0\.0172 | 36\.2        | 0\.2894   | 198\.469 | 1        | 500    | 501        | 668      |
| 14 | corel16k009          | 13884     | 673        | 500    | 173    | 5175      | 3346   | 177      | 29\.301  | 0\.0169 | 364\.456     | 0\.2978   | 199\.195 | 1        | 500    | 501        | 673      |
| 15 | corel16k010          | 13618     | 644        | 500    | 144    | 4692      | 2999   | 350      | 28\.153  | 0\.0196 | 329\.985     | 0\.2786   | 19\.638  | 1        | 500    | 501        | 644      |
| 16 | delicious            | 16105     | 1483       | 500    | 983    | 15806     | 15642  | 19       | 19\.02   | 0\.0193 | 711\.338     | 0\.532    | 227\.734 | 1        | 500    | 501        | 1483     |
| 17 | emotions             | 593       | 78         | 72     | 6      | 27        | 4      | 81       | 18\.685  | 0\.3114 | 14\.781      | 0\.011    | 93\.643  | 1        | 72     | 73         | 78       |
| 18 | enron                | 1702      | 1054       | 1001   | 53     | 753       | 573    | 163      | 33\.784  | 0\.0637 | 739\.528     | 0\.3028   | 175\.031 | 1        | 1001   | 1002       | 1054     |
| 19 | eukaryoteGo          | 7766      | 12711      | 12689  | 22     | 112       | 37     | 1580     | 11\.456  | 0\.0521 | 450\.117     | 0\.0174   | 17\.258  | 1        | 12689  | 12690      | 12711    |
| 20 | eukaryotePseAac      | 7766      | 462        | 440    | 22     | 112       | 37     | 1580     | 11\.456  | 0\.0521 | 450\.117     | 0\.0174   | 138\.963 | 1        | 440    | 441        | 462      |
| 21 | eurlexDc             | 19348     | 5412       | 5000   | 412    | 1615      | 717    | 1633     | 12\.923  | 0\.0031 | 2\.689\.297  | 0\.048    | 219\.253 | 1        | 5000   | 5001       | 5412     |
| 22 | eurlexEv             | 19348     | 8993       | 5000   | 3993   | 16467     | 14609  | 34       | 53\.102  | 0\.0013 | 396\.636     | 0\.4201   | 265\.186 | 1        | 5000   | 5001       | 8993     |
| 23 | eurlexSm             | 19348     | 5201       | 5000   | 201    | 2504      | 1182   | 1041     | 22\.133  | 0\.011  | 5\.369\.761  | 0\.182    | 216\.461 | 1        | 5000   | 5001       | 5201     |
| 24 | flags                | 194       | 26         | 19     | 7      | 54        | 24     | 27       | 33\.918  | 0\.4845 | 22\.547      | 0\.0606   | 88\.793  | 1        | 19     | 20         | 26       |
| 25 | foodtruck            | 407       | 33         | 21     | 12     | 116       | 74     | 115      | 22\.899  | 0\.1908 | 70\.945      | 0\.1035   | 10\.283  | 1        | 21     | 22         | 33       |
| 26 | genbase              | 662       | 1213       | 1186   | 27     | 32        | 10     | 170      | 12\.523  | 0\.0464 | 373\.146     | 0\.0288   | 138\.399 | 1        | 1186   | 1187       | 1213     |
| 27 | gNnegativeGo         | 1392      | 1725       | 1717   | 8      | 19        | 5      | 533      | 1\.046   | 0\.1307 | 184\.476     | 0\.0096   | 124\.722 | 1        | 1717   | 1718       | 1725     |
| 28 | gNegativePseAac      | 1392      | 448        | 440    | 8      | 19        | 5      | 533      | 1\.046   | 0\.1307 | 184\.476     | 0\.0096   | 111\.107 | 1        | 440    | 441        | 448      |
| 29 | gPositiveGo          | 519       | 916        | 912    | 4      | 7         | 2      | 206      | 10\.077  | 0\.2519 | 38\.605      | 0\.001    | 101\.478 | 1        | 912    | 913        | 916      |
| 30 | gPositivePseAac      | 519       | 444        | 440    | 4      | 7         | 2      | 206      | 10\.077  | 0\.2519 | 38\.605      | 0\.001    | 9\.419   | 1        | 440    | 441        | 444      |
| 31 | humanGo              | 3106      | 9858       | 9844   | 14     | 85        | 28     | 718      | 11\.851  | 0\.0847 | 152\.893     | 0\.0203   | 162\.763 | 1        | 9844   | 9845       | 9858     |
| 32 | humanPseAac          | 3106      | 454        | 440    | 14     | 85        | 28     | 718      | 11\.851  | 0\.0847 | 152\.893     | 0\.0203   | 131\.685 | 1        | 440    | 441        | 454      |
| 33 | imdb                 | 120919    | 1029       | 1001   | 28     | 4503      | 2263   | 13144    | 19\.997  | 0\.0714 | 25\.124      | 0\.1082   | 186\.535 | 1        | 1001   | 1002       | 1029     |
| 34 | langlog              | 1460      | 1079       | 1004   | 75     | 304       | 189    | 207      | 11\.801  | 0\.0157 | 392\.669     | 0\.051    | 169\.463 | 1        | 1004   | 1005       | 1079     |
| 35 | mediamill            | 43907     | 221        | 120    | 101    | 6555      | 4104   | 2363     | 43\.756  | 0\.0433 | 2\.564\.047  | 0\.3547   | 181\.906 | 1        | 120    | 121        | 221      |
| 36 | medical              | 978       | 1494       | 1449   | 45     | 94        | 33     | 155      | 12\.454  | 0\.0277 | 895\.014     | 0\.0471   | 156\.286 | 1        | 1449   | 1450       | 1494     |
| 37 | ng20                 | 19300     | 1026       | 1006   | 20     | 55        | 17     | 997      | 10\.289  | 0\.0514 | 10\.073      | 3,20E\-02 | 139\.168 | 1        | 1006   | 1007       | 1026     |
| 38 | nuswideBow           | 269648    | 582        | 501    | 81     | 18430     | 11378  | 60301    | 18\.685  | 0\.0231 | 951\.187     | 0\.171    | 204\.328 | 1        | 501    | 502        | 582      |
| 39 | nuswideVlad          | 269648    | 210        | 129    | 81     | 18430     | 11378  | 60301    | 18\.685  | 0\.0231 | 951\.187     | 0\.171    | 19\.076  | 1        | 129    | 130        | 210      |
| 40 | ohsumed              | 13929     | 1025       | 1002   | 23     | 1147      | 578    | 1175     | 16\.631  | 0\.0723 | 78\.692      | 0\.0688   | 170\.902 | 1        | 1002   | 1003       | 1025     |
| 41 | plantGo              | 978       | 3103       | 3091   | 12     | 32        | 8      | 277      | 10\.787  | 0\.0899 | 66\.904      | 0\.0058   | 139\.869 | 1        | 3091   | 3092       | 3103     |
| 42 | plantPseAac          | 978       | 452        | 440    | 12     | 32        | 8      | 277      | 10\.787  | 0\.0899 | 66\.904      | 0\.0058   | 120\.374 | 1        | 440    | 441        | 452      |
| 43 | rcv1sub1             | 6000      | 47337      | 47236  | 101    | 1028      | 657    | 246      | 28\.797  | 0\.0285 | 544\.923     | 0\.2237   | 223\.134 | 1        | 47236  | 47237      | 47337    |
| 44 | rcv1sub2             | 6000      | 47337      | 47236  | 101    | 954       | 589    | 549      | 26\.342  | 0\.0261 | 455\.138     | 0\.2092   | 222\.387 | 1        | 47236  | 47237      | 47337    |
| 45 | rcv1sub3             | 6000      | 47337      | 47236  | 101    | 939       | 591    | 635      | 26\.142  | 0\.0259 | 683\.326     | 0\.2075   | 222\.228 | 1        | 47236  | 47237      | 47337    |
| 46 | rcv1sub4             | 6000      | 47330      | 47229  | 101    | 816       | 491    | 950      | 24\.837  | 0\.0246 | 893\.713     | 0\.2165   | 220\.823 | 1        | 47229  | 47230      | 47330    |
| 47 | rcv1sub5             | 6000      | 47336      | 47235  | 101    | 946       | 586    | 526      | 26\.415  | 0\.0262 | 696\.815     | 0\.2381   | 222\.303 | 1        | 47235  | 47236      | 47336    |
| 48 | reutersk500          | 6000      | 603        | 500    | 103    | 811       | 513    | 381      | 14\.622  | 0\.0142 | 519\.805     | 0\.0517   | 175\.476 | 1        | 500    | 501        | 603      |
| 49 | scene                | 2407      | 300        | 294    | 6      | 15        | 3      | 405      | 1\.074   | 0\.179  | 12\.538      | 0\.0003   | 101\.834 | 1        | 294    | 295        | 300      |
| 50 | slashdot             | 3782      | 1101       | 1079   | 22     | 156       | 56     | 525      | 11\.809  | 0\.0537 | 176\.931     | 0\.0131   | 151\.247 | 1        | 1079   | 1080       | 1101     |
| 51 | stackex\-chemistry   | 6961      | 715        | 540    | 175    | 3032      | 2331   | 318      | 21\.093  | 0\.0121 | 568\.779     | 0\.1867   | 194\.733 | 1        | 540    | 541        | 715      |
| 52 | stackex\-chess       | 1675      | 812        | 585    | 227    | 1078      | 890    | 48       | 24\.113  | 0\.0106 | 857\.898     | 0\.2625   | 187\.794 | 1        | 585    | 586        | 812      |
| 53 | stackex\-coffee      | 225       | 1886       | 1763   | 123    | 174       | 149    | 7        | 19\.867  | 0\.0162 | 272\.415     | 0\.1691   | 17\.446  | 1        | 1763   | 1764       | 1886     |
| 54 | stackex\-cooking     | 10491     | 977        | 577    | 400    | 6386      | 5276   | 134      | 22\.248  | 0\.0056 | 378\.576     | 0\.1933   | 211\.112 | 1        | 577    | 578        | 977      |
| 55 | stackex\-cs          | 9270      | 909        | 635    | 274    | 4749      | 3679   | 119      | 25\.562  | 0\.0093 | 850\.023     | 0\.2723   | 205\.324 | 1        | 635    | 636        | 909      |
| 56 | stackex\-philosophy  | 3971      | 1075       | 842    | 233    | 2249      | 1890   | 224      | 2\.272   | 0\.0098 | 687\.532     | 0\.2325   | 199\.051 | 1        | 842    | 843        | 1075     |
| 57 | tmc2007              | 28596     | 49082      | 49060  | 22     | 1341      | 662    | 2486     | 21\.579  | 0\.0981 | 151\.567     | 0\.1747   | 21\.093  | 1        | 49060  | 49061      | 49082    |
| 58 | tmc2007\-500         | 28596     | 522        | 500    | 22     | 1172      | 408    | 2484     | 22\.196  | 0\.1009 | 171\.343     | 0\.1927   | 163\.721 | 1        | 500    | 501        | 522      |
| 59 | virusGo              | 207       | 755        | 749    | 6      | 17        | 6      | 56       | 12\.174  | 0\.2029 | 40\.412      | 0\.0079   | 112\.437 | 1        | 749    | 750        | 755      |
| 60 | virusPseAac          | 207       | 446        | 440    | 6      | 17        | 6      | 56       | 12\.174  | 0\.2029 | 40\.412      | 0\.0079   | 107\.117 | 1        | 440    | 441        | 446      |
| 61 | wiki10\-31K          | 20762     | 132876     | 101938 | 30938  | 20693     | 20625  | 3        | 187\.616 | 0\.0006 | 53\.418\.088 | 0\.8387   | 318\.094 | 1        | 101938 | 101939     | 132876   |
| 62 | yahoo\-arts          | 7484      | 23172      | 23146  | 26     | 599       | 358    | 1240     | 16\.539  | 0\.0636 | 947\.379     | 0\.0595   | 197\.029 | 1        | 23146  | 23147      | 23172    |
| 63 | yahoo\-business      | 11214     | 21954      | 21924  | 30     | 233       | 115    | 5992     | 1\.599   | 0\.0533 | 8\.801\.777  | 0\.1252   | 188\.476 | 1        | 21924  | 21925      | 21954    |
| 64 | yahoo\-computers     | 12444     | 34129      | 34096  | 33     | 428       | 239    | 4122     | 15\.072  | 0\.0457 | 1\.766\.952  | 0\.0965   | 199\.926 | 1        | 34096  | 34097      | 34129    |
| 65 | yahoo\-education     | 12030     | 27567      | 27534  | 33     | 511       | 283    | 2611     | 14\.632  | 0\.0443 | 1\.681\.137  | 0\.0418   | 199\.561 | 1        | 27534  | 27535      | 27567    |
| 66 | yahoo\-entertainment | 12730     | 32022      | 32001  | 21     | 337       | 151    | 2769     | 14\.137  | 0\.0673 | 644\.169     | 0\.0387   | 192\.381 | 1        | 32001  | 32002      | 32022    |
| 67 | yahoo\-health        | 9205      | 30637      | 30605  | 32     | 335       | 169    | 2832     | 16\.441  | 0\.0514 | 6\.535\.306  | 0\.092    | 196\.088 | 1        | 30605  | 30606      | 30637    |
| 68 | yahoo\-recreation    | 12828     | 30346      | 30324  | 22     | 530       | 284    | 1652     | 14\.289  | 0\.065  | 12\.203      | 0\.0301   | 196\.836 | 1        | 30324  | 30325      | 30346    |
| 69 | yahoo\-reference     | 8027      | 39712      | 39679  | 33     | 275       | 144    | 3038     | 11\.744  | 0\.0356 | 4\.618\.628  | 0\.0486   | 197\.019 | 1        | 39679  | 39680      | 39712    |
| 70 | yahoo\-science       | 6428      | 37227      | 37187  | 40     | 457       | 252    | 1200     | 14\.498  | 0\.0362 | 526\.318     | 0\.0575   | 203\.373 | 1        | 37187  | 37188      | 37227    |
| 71 | yahoo\-social        | 12111     | 52389      | 52350  | 39     | 361       | 179    | 4062     | 12\.793  | 0\.0328 | 2\.577\.044  | 0\.049    | 204\.181 | 1        | 52350  | 52351      | 52389    |
| 72 | yahoo\-society       | 14512     | 31829      | 31802  | 27     | 1054      | 624    | 4092     | 16\.704  | 0\.0619 | 3\.020\.678  | 0\.0957   | 206\.235 | 1        | 31802  | 31803      | 31829    |
| 73 | yeast                | 2417      | 117        | 103    | 14     | 198       | 77     | 237      | 42\.371  | 0\.3026 | 71\.968      | 0\.1044   | 125\.621 | 1        | 103    | 104        | 117      |
| 74 | yelp                 | 10806     | 676        | 671    | 5      | 32        | 0      | 2120     | 16\.383  | 0\.3277 | 28\.756      | 0\.0332   | 115\.839 | 1        | 671    | 672        | 676      |


## Scripts
This source code consists of the following R scripts, folders and files:

01. libraries
02. utils
03. preprocessing
04. miscellaneous
05. correlations
06. partitions
07. clusValidation
08. clusHybrid
09. clusGlobal
10. clusLocal
11. BinRel
12. Run
13. executeForOneDataset


## Process flow

1. Adjust the datasets for the experiment. Creates 10-fold for cross-validation with train, test and validation. Separates the label space for each of the 10-fold;
2. Calculates the correlation from jaccard dissimilarity;
3. Computes Hclust/Cutree
4. Select the HClust method with the highest coefficient
5. Asssembles the groups of each partition from the settings calculated by the cutree for the chosen HClust method
6. Validates each group of each partition in the CLUS-HMC
7. Gathers the predictions of the groups and evaluates the partition as a whole
8. Select the partition with the highest F1 measurement
9. Test partition


## Pre-Processing

<p align="center">  
  <img  width="499,1" height="303,1" src="https://github.com/cissagatto/PartitionsJaccard/blob/master/images/Fase_1.png">
</p>

To calculate and find correlations between labels, we use only the label space of the specific folder being treated at that time. So, first we have to separate, in a different file, the label space from all the rest of the data. We can also separate all instances that belong to each label, so that they can be used in blocks to form each group, of each partition, afterwards. For example, supposing that in a partition the labels 1, 2 and 10 were in a group, then we take the corresponding files and put them together.

The datasets used in these experiments were downloaded from [Cometa Datasets](https://cometa.ujaen.es/). The datasets **iterative stratification** were used. For more information on these methods consult [Iterative Stratification](https://link.springer.com/chapter/10.1007/978-3-642-23808-6_10). It is also interesting to consult the [mldr](https://cran.r-project.org/web/packages/mldr/index.html) and [utiml](https://cran.r-project.org/web/packages/utiml/index.html) packages.

<p align="center">
  <img width="500" height="400" src="https://github.com/cissagatto/PartitionsJaccard/blob/master/images/cross_validation_detalhado.png">
</p>


## Correlations with Jaccard
<p align="center">
  <img width="409,5" height="324,1" src="https://github.com/cissagatto/PartitionsJaccard/blob/master/images/Fase_2_Jaccard.png">
</p>

## Find Hybrid Partitions with HClust and Cutree
<p align="center">
  <img width="235,9" height="333,9" src="https://github.com/cissagatto/PartitionsJaccard/blob/master/images/Fase_3.png">
</p>

The **hclust** function requires a method to perform the grouping. In our experiment, we chose to calculate three available methods:

a) average

b) single

c) complete

At each execution, the corresponding coefficient for each method is saved. The method with the highest coefficient is chosen and then the partitions generated by this method are mounted.


## Mount and Validate Hybrid Partitions

<p align="center">
  <img width="562,1" height="294,7" src="https://github.com/cissagatto/PartitionsJaccard/blob/master/images/Fase_4.png">
</p>

The **partitions.R** script is responsible for assembling the partitions themselves and separating them into files to be used as inputs in multi-label classifiers. 

In our experiment we chose to test the best partition directly on [CLUS](https://dtai.cs.kuleuven.be/clus/). However, you can implement the silhouette to choose one among all the generated partitions. The [silhouette](http://www.sthda.com/english/wiki/wiki.php?id_contents=7952) is basically a method of evaluating clusters.

Suppose that, for partition 4, of fold-09, four groups were found. Each of the four groups has different amounts of labels. Then, these labels are grouped into each group. These groups, with it's instances, are saved in files so that they can be used later as input from binary or multi-label classifiers.

The choice of the best partition is up to each person. In our case, we chose to take the partitions created (training / validation) and test on CLUS. The partition with the best performance at this stage is chosen as the partition that will be used with the fold test.

Therefore, we choose the performance of a classifier to determine our best partition. However, cluster evalution methods, such as silhouette, can also be used. It depends on the approach, or emphasis, that each person gives to the experiment.


# Hardware



### ILUSTRATION OF OUR METHOD
