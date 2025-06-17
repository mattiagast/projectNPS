This is the repository for the project of the Nonparametric Statistics course, MSc Mathematical Engineering @ Politecnico di Milano, A.Y. 2024-2025.
The topic of this project is a Nonparametric Inference and Forcasting in Italian Electricity Spot Market.
The repository is structured as follow:

FOLDERS:
Outputs: contain collected images used for the final presentation and the report
PredictionData: contain the dataset ready to the use for the prediction code
Presentation: contain the PowerPoints and the final report of the project
ShortDataset: contain some summary datasets and the dataset used in the functional data analysis part

*WARNING:* two folders, namely DatasetXML and DatasetCSV are not presence

.R FILES:
0.1_csvConversion.R: conversion code from .xml to .csv
0.2_obtainPrezzoZonale.R: automatically extract from the curves the final price of each auction
0.3_obtainSmoothDataset.R: automatically extract from the curves the dataset for the smoothing
1.1_tendecyExploration.R: code for the patterns of the final prices
1.2_kde.R: kernel density estimation of the knots
2.1_smoothingChoice.R: different techinque for smoothing the dataset
2.2_outlierAnalysis.R: outluer analysis of the functional smoothed data
3.1_permutationalAov.R: permutation one-way anova
3.2_permutationalT.R: permutation t 2 paired sample test 
4.1_FAR.R: snipped code for the FAR(1) model
4.2_demandFAR.R: prediction and conformal intervals for the demand curve
4.2_supplyFAR.R: prediction and conformal intervals for the supply curve