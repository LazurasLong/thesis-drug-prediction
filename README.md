# Thesis

Predicting drug side effects and targets using machine learning approaches - a case study on antidepressants

## Introduction

The purpose of our study is to develop a computational approach to investigate potential side effects and targets of antidepressants, hoping to provide support for better strategies for the future of drug development and therapy.

## Methodology

* Experimental design
<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Experimental%20design%201.jpg" alt="Experimental design 1" width="70%" height="70%">
<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Experimental%20design%202.jpg" alt="Experimental design 2" width="70%" height="70%">

* Presented an aggregation framework to predict unknown side effects and hidden targets from 816 drugs by adopting

  * 653 chemical features
  * 984 biological features
  * 6,111 phenotypic features
  
* Implemented the computational framework using four machine learning-based algorithms:

  * Random Forest (RF)
  * k-Nearest Neighbors (kNN)
  * Support Vector Machines (SVM)
  * Sparse Canonical Correlation Analysis (SCCA)
  
* The aggregation random forest model achieved best in overall performance among these algorithms

<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Methodology%201.png" alt="Methodology 1">
<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Methodology%202.png" alt="Methodology 2">

## Results and performance evaluation

* Conducted the case study using 15 depression-related drugs, including

  * 9 first generation antidepressants
  * 5 second generation antidepressants
  * 1 muscle relaxant that has a structure similar to tricyclic antidepressant (TCA)

* The in silico model obtained promising results with

  * AUROC score of 0.9140834, AUPR score of 0.5185952 for side effects prediction
  * AUROC score of 0.9513566, AUPR score of 0.3101223 for targets prediction
  
<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Results%20and%20performance%20evaluation%201.jpg" alt="Results and performance evaluation 1" width="50%" height="50%"><img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/imgs/Results%20and%20performance%20evaluation%202.jpg" alt="Results and performance evaluation 2" width="50%" height="50%">

## Discussion and conclusion


<img src="https://raw.githubusercontent.com/rubychi/thesis-drug-prediction/master/result/visualization/barplot/antidp-target-freq%20(barplot%20visualization).png" alt="Discussion and conclusion">

To summarize the prediction results of antidepressants, totally 2662 unknown drug-side effect associations and 309 hidden drug-target associations were predicted using F-measure as the cutoff threshold.
Among 1307 side effects, 289 were predicted to have associations with more than 10 drugs. Among 599 drug-target associations, 290 are known, 309 are predicted and 54 of them were successfully validated through literature.

We demonstrated the effectiveness of proposed framework for the identification of potential side effects and targets at large scale. However, this strategy needs to perform prediction twice from different feature sets and thus requires considerable computational burden. For the case study, we could only extract 15 depression-related drug compounds from the online databases for now, and not all types of antidepressants were involved in the experiment, as a result, some information could not obtained through the prediction model. 

In future work, it is expected to gain more knowledge by enlarging the sample data sets and would be reasonable to extend from single drug prediction to drug-drug interaction (DDI) since patients may take more than one medications at the same time with a possibility of increased risk of ADRs. Personalized medicine should also be taken into consideration owing to the fact that variations in the DNA sequences can have impact on how human develop disease and respond to drugs. Some studies have suggested that genes may contribute to the susceptibility to mental health disorder.

