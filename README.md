# Phil_forestLossEmissions
This repository contains the Google Earth Engine javascript (.js) and R file (correlation trend analysis test) for the paper entitled "ANALYSIS OF FOREST COVER CHANGES AND FOREST GHG EMISSIONS IN THE PHILIPPINES FROM 2011-2020 USING SATELLITE-DERIVED PRODUCTS" by Fallarcuna et al., (2021) Asian Conference in Remote Sensing 2021. 

The paper aimed to produce annual forest loss trends for all the 81 Philppine provinces, (Davao Occidental is still lumped with Davao del Sur, NCR: treated as individual province

The javascript code must be pasted and ran in google earth engine editor, imported files in earth engine asset are accessible via request.

The R code (correlation script) can be used to any bivariate data (time series or not), just edit the appropriate .csv input files, correspondingly (data are in columns, preceded by 1st row label; methods (pearson,spearman, kendall rank) and p value can be specified).
