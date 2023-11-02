
# Brain-Wide Mendelian Randomization Study of Anxiety Disorders and Symptoms

Brain-Wide Mendelian Randomization Study of Anxiety Disorders and Symptoms is a study undertaken to gain insights into the role of brain structure and function on anxiety (ANX). We conducted a genetically informed investigation leveraging information from ANX genome-wide association studies available from UK Biobank (UKB), FinnGen Program, and Million Veteran Program (MVP) together with UKB genome-wide data related to 3,935 brain imaging-derived phenotypes (IDP).


## Heritability and Genetic Correlation Analysis

Heritability (h2) and genetic correlation (rg) of ANX and brain imaging phenotypes were calculated using linkage disequilibrium score regression (LDSC). LDSC code is available at https://github.com/bulik/ldsc. 

## Mendelian Randomization 

Two-sample MR analysis: 
- TwoSampleMR R package to estimate the bidirectional relationship between ANX (assessed in MVP and FinnGen) and brain IDPs (assessed in UKB). TwoSampleMR code is available at https://github.com/MRCIEU/TwoSampleMR.
- MRlap approach was applied when testing the relationship between ANX and IDPs within UKB, to account for the possible bias introduced by the sample overlap between exposure and outcome datasets. MRlap code is available at https://github.com/n-mounier/MRlap.

Sensitivity Analysis:   
- MR-RAPS code is available at https://github.com/qingyuanzhao/mr.raps.
- MR-PRESSO code is available at https://github.com/rondolab/MR-PRESSO.

Multivariable Analysis:    
- MendelianRandomization R package was applied for Multivariable Analysis.

Organization of the content: 

- `/MR-IVW and RAPS`: Contains codes for performing Two-Sample MR for MVP, FinnGen and UKBiobank. IVW and RAPS methods were performed. 
- `/MR_Multivariable`: Contains codes for performing Multivariable MR for MVP, FinnGen, and UKBiobank.
- `/MR-PRESSO`: Contains scripts for performing MR-PRESSO as sensitivity analysis for MVP, FinnGen, and UKBiobank.

## Regression Analysis 

A generalized linear model to explore the associations of brain IDPs with ANX symptoms.
- `/GLM-brain-anx`: Contains codes for performing GLM.  
