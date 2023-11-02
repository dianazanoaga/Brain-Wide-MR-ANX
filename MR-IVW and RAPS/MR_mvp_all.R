#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(remotes)
library(readxl)
library(data.table)
library(TwoSampleMR)
library("plyr")
library(MRInstruments)
library(ieugwasr)
library("ragg")
library(Cairo)
library(mr.raps)
library(WriteXLS)
library(writexl)

# Load necessary datasets

mvp_all <- fread("/gpfs/gibbs/pi/polimanti/diana/mvp_all.txt")

batch_args <- read_excel(args[1])
batch_args <- t(batch_args)
print(seq_along(batch_args))

for (i in seq_along(batch_args)) {

 print(i)
 print(batch_args[i])
 brain_all <- fread(batch_args[i])
 brain_all <- as.data.frame(brain_all)

# Select only significant SNPs for both datasets
 mvp_sig <- mvp_all[mvp_all$P < 5*10^(-8),]
 mvp_sig_relaxed <- mvp_all[mvp_all$P < 1*10^(-5),]

 brain_sig <- brain_all[brain_all$pvalue < 5*10^(-8),]
 brain_sig_relaxed <- brain_all[brain_all$pvalue < 1*10^(-5),]

# Formatting 

 if(dim(mvp_sig)[1] != 0 ){

    mvp_for <- format_data(mvp_sig, 
                          type="exposure", 
                          phenotype_col = "mvp_anxiety",
                          snp_col = "rsid",
                          beta_col = "EFFECT",
                          se_col = "SE",
                          eaf_col = "Freq1",
                          effect_allele_col = "A1",
                          other_allele_col = "A2",
                          pval_col = "P")

 }
 
 mvp_for_relaxed <- format_data(mvp_sig_relaxed, 
                               type="exposure", 
                               phenotype_col = "mvp_anxiety",
                               snp_col = "rsid",
                               beta_col = "EFFECT",
                               se_col = "SE",
                               eaf_col = "Freq1",
                               effect_allele_col = "A1",
                               other_allele_col = "A2",
                               pval_col = "P")
 
 if(dim(brain_sig)[1] != 0 ){

    brain_for <- format_data(brain_sig, 
                          type="exposure", 
                          phenotype_col = "brain_0705",
                          snp_col = "rsid",
                          beta_col = "beta",
                          se_col = "se",
                          effect_allele_col = "a2",
                          other_allele_col = "a1",
                          pval_col = "pvalue",
                          samplesize_col = "n")
 }
 
 brain_for_relaxed <- format_data(brain_sig_relaxed, 
                                 type="exposure", 
                                 phenotype_col = "brain_0705",
                                 snp_col = "rsid",
                                 beta_col = "beta",
                                 se_col = "se",
                                 effect_allele_col = "a2",
                                 other_allele_col = "a1",
                                 pval_col = "pvalue")


# CLUMPING

# Clumping anxiety
 
 if(dim(mvp_sig)[1] != 0 ){

 mvp_clump <- mvp_for
 mvp_clump <- clump_data(mvp_clump)

 }
 
 mvp_clump_relaxed <- mvp_for_relaxed
 mvp_clump_relaxed <- clump_data(mvp_clump_relaxed)

# Clumping brain

 if(dim(brain_sig)[1] != 0 ){

 brain_clump <- brain_for
 brain_clump <- clump_data(brain_clump)

 }
 
 brain_clump_relaxed <- brain_for_relaxed
 brain_clump_relaxed <- clump_data(brain_clump_relaxed)

# ANX as exposure and Brain IDP as outcome
 
 if(dim(mvp_sig)[1] != 0 ){

 brain_out <- read_outcome_data(
  snps = mvp_clump$SNP,
  sep = ",",
  filename = batch_args[i],
  snp_col = "rsid",
  beta_col = "beta",
  se_col = "se",
  effect_allele_col = "a2",
  other_allele_col = "a1",
  pval_col = "pvalue",
  samplesize_col = "n"

)} else {

   brain_out = matrix(c(0,0,0,0), nrow=1, ncol=1)

}

 brain_out_relaxed <- read_outcome_data(

  snps = mvp_clump_relaxed$SNP,
  sep = ",",
  filename = batch_args[i],
  snp_col = "rsid",
  beta_col = "beta",
  se_col = "se",
  effect_allele_col = "a2",
  other_allele_col = "a1",
  pval_col = "pvalue",
  samplesize_col = "n"

)
 
 if(dim(brain_sig)[1] != 0 ){

  mvp_out <- read_outcome_data(
    snps = brain_clump$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/mvp_all.txt",
    snp_col = "rsid",
    beta_col = "EFFECT",
    se_col = "SE",
    effect_allele_col = "A1",
    other_allele_col = "A2",
    pval_col = "P",
    samplesize_col = "N"

  )} else{

   mvp_out = matrix(c(0,0,0,0), nrow=1, ncol=1)

 }
 
 mvp_out_relaxed <- read_outcome_data(

  snps = brain_clump_relaxed$SNP,
  sep = ",",
  filename = "/gpfs/gibbs/pi/polimanti/diana/mvp_all.txt",
  snp_col = "rsid",
  beta_col = "EFFECT",
  se_col = "SE",
  effect_allele_col = "A1",
  other_allele_col = "A2",
  pval_col = "P",
  samplesize_col = "N"

)

mr_method_list()$name

# Harmonize for ANX as exposure and Brain IDP as outcome

anx_brain_harmonize_relaxed <- harmonise_data(

    exposure_dat = mvp_clump_relaxed, 
    outcome_dat = brain_out_relaxed

  )    
  
# Perform Mendelian Randomization

mr_anx_brain_relaxed <- mr(anx_brain_harmonize_relaxed, method_list = mr_method_list()$obj)
mr_anx_brain_relaxed$"exposure" = "mvp anxiety"
mr_anx_brain_relaxed$"outcome" =  batch_args[i]
n <- dim(mr_anx_brain_relaxed)[1]
mr_anx_brain_relaxed$"threshold" = rep("relaxed threshold", n)
  
list_data_1 <- mr_anx_brain_relaxed    
  
# Harmonize for Brain IDP as exposure and ANX as outcome

brain_anx_harmonize_relaxed <- harmonise_data(

  exposure_dat = brain_clump_relaxed, 
  outcome_dat = mvp_out_relaxed

  )
  
  # Perform Mendelian Randomization

mr_brain_anx_relaxed <- mr(brain_anx_harmonize_relaxed, method_list = mr_method_list()$obj)
mr_brain_anx_relaxed$"exposure" =  batch_args[i]
mr_brain_anx_relaxed$"outcome" =  "mvp anxiety"
n <- dim(mr_brain_anx_relaxed)[1]
mr_brain_anx_relaxed$"threshold" = rep("relaxed threshold", n)
  
list_data_2 <- mr_brain_anx_relaxed

list_data <- list("anx_brain" = list_data_1 ,"brain_anx" = list_data_2)

id <- sub(".txt.*", "", batch_args[i])
id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format", "", id)
write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/mvp", id, ".xlsx"))

}