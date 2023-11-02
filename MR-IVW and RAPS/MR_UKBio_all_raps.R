
#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(remotes)
library(readxl)
library(data.table)
library(TwoSampleMR)
library("plyr")
library("dplyr")
library(MRInstruments)
library(ieugwasr)
library("ragg")
library(Cairo)
library(mr.raps)
library(WriteXLS)
library(writexl)



# load necessary datasets

uk_all<- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")
uk_all <- as.data.frame(uk_all)

batch_args <- read_excel(args[1])
batch_args <- t(batch_args)
print(seq_along(batch_args))

for (i in seq_along(batch_args)) {
  print(i)
  print(batch_args[i])
  brain_all <- fread(batch_args[i])
  brain_all <- as.data.frame(brain_all)
  
  id <- sub(".txt.*", "", batch_args[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format/", "", id)
  
  print(id)
  
  # select only significant SNPs for both datasets
  uk_sig <- uk_all[uk_all$pval < 5*10^(-8),]
  uk_sig_relaxed <- uk_all[uk_all$pval < 1*10^(-5),]
  
  brain_sig <- brain_all[brain_all$pvalue < 5*10^(-8),]
  brain_sig_relaxed <- brain_all[brain_all$pvalue < 1*10^(-5),]
  
  
   
  uk_for_relaxed <- format_data(uk_sig_relaxed, 
                                type="exposure", 
                                phenotype_col = "anxiety",
                                snp_col = "rsid",
                                beta_col = "beta",
                                se_col = "se",
                                effect_allele_col = "alt.x",
                                other_allele_col = "ref.x",
                                pval_col = "pval_EUR")
  
   
  brain_for_relaxed <- format_data(brain_sig_relaxed, 
                                   type="exposure", 
                                   phenotype_col = "brain",
                                   snp_col = "rsid",
                                   beta_col = "beta",
                                   se_col = "se",
                                   effect_allele_col = "a2",
                                   other_allele_col = "a1",
                                   pval_col = "pvalue")
  
  
  # Clumping
  
  uk_clump_relaxed <- uk_for_relaxed
  uk_clump_relaxed <- clump_data(uk_clump_relaxed)
  
  
  brain_clump_relaxed <- brain_for_relaxed
  brain_clump_relaxed <- clump_data(brain_clump_relaxed)
  
 
  
  brain_out_relaxed <- read_outcome_data(
    snps = uk_clump_relaxed$SNP,
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
  
  
  uk_out_relaxed <- read_outcome_data(
    snps = brain_clump_relaxed$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt",
    snp_col = "rsid",
    beta_col = "beta",
    se_col = "se",
    effect_allele_col = "alt.x",
    other_allele_col = "ref.x",
    pval_col = "pval_EUR"
  )
  
  
  # harmoniza for anx as exposure and brain as outcome
  
 
  
  anx_brain_harmonize_relaxed <- harmonise_data(
    exposure_dat = uk_clump_relaxed, 
    outcome_dat = brain_out_relaxed
  )
  
  #mendelian 
  
  mr_anx_brain_relaxed <- mr(anx_brain_harmonize_relaxed, method_list = c("mr_ivw_mre" , "mr_raps"))
  mr_anx_brain_relaxed$"exposure" = "ukbio anxiety"
  mr_anx_brain_relaxed$"outcome" =  batch_args[i]
  n <- dim(mr_anx_brain_relaxed)[1]
  mr_anx_brain_relaxed$"threshold" = rep("relaxed threshold", n)
  
  list_data_1 <- mr_anx_brain_relaxed
  
  
  brain_anx_harmonize_relaxed <- harmonise_data(
    exposure_dat = brain_clump_relaxed, 
    outcome_dat = uk_out_relaxed
  )
  
  #mendelian 
  
  mr_brain_anx_relaxed <- mr(brain_anx_harmonize_relaxed, method_list = c("mr_ivw_mre" , "mr_raps"))
  mr_brain_anx_relaxed$"exposure" =  batch_args[i]
  mr_brain_anx_relaxed$"outcome" =  "ukbio anxiety"
  n <- dim(mr_brain_anx_relaxed)[1]
  mr_brain_anx_relaxed$"threshold" = rep("relaxed threshold", n)
  
  list_data_2 <- mr_brain_anx_relaxed
  
  mr_report(brain_anx_harmonize_relaxed)
 
  list_data <- list("anx_brain" = list_data_1 ,"brain_anx" = list_data_2)
  
  write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/ukbio_mrraps_1/", id, ".xlsx"))
  
}


