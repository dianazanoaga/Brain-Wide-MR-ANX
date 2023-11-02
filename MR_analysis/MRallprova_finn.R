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

finn_all <- fread("/gpfs/gibbs/pi/polimanti/diana/gad_analysis/input_gad/finngen_R8_KRA_PSY_ANXIETY_EXMORE")
finn_all <- as.data.frame(finn_all)
write.csv(finn_all, file="/gpfs/gibbs/pi/polimanti/diana/finn_all.txt")


batch_args <- read_excel(args[1])
batch_args <- t(batch_args)
print(seq_along(batch_args))


for (i in seq_along(batch_args)) {
  print(i)
  print(batch_args[i])
  brain_all <- fread(batch_args[i])
  brain_all <- as.data.frame(brain_all)
  
  # select only significant SNPs for both datasets
  finn_sig <- finn_all[finn_all$pval < 5*10^(-8),]
  finn_sig_relaxed <- finn_all[finn_all$pval < 1*10^(-5),]
  
  brain_sig <- brain_all[brain_all$pvalue < 5*10^(-8),]
  brain_sig_relaxed <- brain_all[brain_all$pvalue < 1*10^(-5),]
  
  
  # formatting 
  if(dim(finn_sig)[1] != 0 ){
    finn_for <- format_data(finn_sig, 
                            type="exposure", 
                            phenotype_col = "anxiety",
                            snp_col = "rsids",
                            beta_col = "beta",
                            se_col = "sebeta",
                            eaf_col = "Freq1",
                            effect_allele_col = "alt",
                            other_allele_col = "ref",
                            pval_col = "pval")
  }
  
  
  finn_for_relaxed <- format_data(finn_sig_relaxed, 
                                 type="exposure", 
                                 phenotype_col = "anxiety",
                                 snp_col = "rsids",
                                 beta_col = "beta",
                                 se_col = "sebeta",
                                 eaf_col = "Freq1",
                                 effect_allele_col = "alt",
                                 other_allele_col = "ref",
                                 pval_col = "pval")
  
  if(dim(brain_sig)[1] != 0 ){
    brain_for <- format_data(brain_sig, 
                             type="exposure", 
                             phenotype_col = "brain",
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
                                   phenotype_col = "brain",
                                   snp_col = "rsid",
                                   beta_col = "beta",
                                   se_col = "se",
                                   effect_allele_col = "a2",
                                   other_allele_col = "a1",
                                   pval_col = "pvalue")
  
  
  # CLUMPING
  
  #clumping anxiety
  
  if(dim(finn_sig)[1] != 0 ){
    finn_clump <- finn_for
    finn_clump <- clump_data(finn_clump)
  }
  
  finn_clump_relaxed <- finn_for_relaxed
  finn_clump_relaxed <- clump_data(finn_clump_relaxed)
  
  # clumping brain
  if(dim(brain_sig)[1] != 0 ){
    brain_clump <- brain_for
    brain_clump <- clump_data(brain_clump)
  }
  
  brain_clump_relaxed <- brain_for_relaxed
  brain_clump_relaxed <- clump_data(brain_clump_relaxed)
  
  
  merge_brain_out <- merge(x=finn_clump, y=brain_all, by.x= "SNP", by.y = "rsid")
  merge_finn_out <- merge(x=brain_clump, y=finn_all, by.x= "SNP", by.y = "rsids")
  
  # anx as exposure and brain as outcome
  
  if((dim(finn_sig)[1] != 0) & (dim(merge_brain_out)[1]!= 0)){
    
    brain_out <- read_outcome_data(
      snps = finn_clump$SNP,
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
  } else {
    brain_out = matrix(c(0,0,0,0), nrow=1, ncol=1)
  }
  
  brain_out_relaxed <- read_outcome_data(
    snps = finn_clump_relaxed$SNP,
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
  
  if((dim(brain_sig)[1] != 0) & (dim(merge_finn_out)[1]!= 0)){
    
      finn_out <- read_outcome_data(
      snps = brain_clump$SNP,
      sep = ",",
      filename = "/gpfs/gibbs/pi/polimanti/diana/finn_all.txt",
      snp_col = "rsids",
      beta_col = "beta",
      se_col = "sebeta",
      effect_allele_col = "alt",
      other_allele_col = "ref",
      pval_col = "pval"
    )
  } else {
    finn_out = matrix(c(0,0,0,0), nrow=1, ncol=1)
  }
  
  finn_out_relaxed <- read_outcome_data(
    snps = brain_clump_relaxed$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/finn_all.txt",
    snp_col = "rsids",
    beta_col = "beta",
    se_col = "sebeta",
    effect_allele_col = "alt",
    other_allele_col = "ref",
    pval_col = "pval"
  )
  
  
  
  mr_method_list()$name
  
  # harmoniza for anx as exposure and brain as outcome
  
  #if(dim(brain_out)[1] <5) {
    
    anx_brain_harmonize_relaxed <- harmonise_data(
      exposure_dat = finn_clump_relaxed, 
      outcome_dat = brain_out_relaxed
    )
    
    #mendelian 
    mr_anx_brain_relaxed <- mr(anx_brain_harmonize_relaxed, method_list = mr_method_list()$obj)
    mr_anx_brain_relaxed$"exposure" = "finn anxiety"
    mr_anx_brain_relaxed$"outcome" =  batch_args[i]
    n <- dim(mr_anx_brain_relaxed)[1]
    mr_anx_brain_relaxed$"threshold" = rep("relaxed threshold", n)
    
    list_data_1 <- mr_anx_brain_relaxed
    
    
  #} else {
    
    #anx_brain_harmonize <- harmonise_data(
     # exposure_dat = finn_clump, 
      #outcome_dat = brain_out
    #)
    
    #mr_anx_brain <- mr(anx_brain_harmonize, method_list = mr_method_list()$obj)
    #mr_anx_brain$"exposure" = "finn anxiety"
    #mr_anx_brain$"outcome" =  batch_args[i]
    #n <- dim(mr_anx_brain)[1]
    #mr_anx_brain$"threshold" = rep("stringent threshold", n)
    
    #list_data_1 <- mr_anx_brain
    
  #}
  
  
  # harmonize for brain as exposure and anx as outcome
  
  #if(dim(finn_out)[1] < 5) {
    
    brain_anx_harmonize_relaxed <- harmonise_data(
      exposure_dat = brain_clump_relaxed, 
      outcome_dat = finn_out_relaxed
    )
    
    #mendelian 
    mr_brain_anx_relaxed <- mr(brain_anx_harmonize_relaxed, method_list = mr_method_list()$obj)
    mr_brain_anx_relaxed$"exposure" =  batch_args[i]
    mr_brain_anx_relaxed$"outcome" =  "finn anxiety"
    n <- dim(mr_brain_anx_relaxed)[1]
    mr_brain_anx_relaxed$"threshold" = rep("relaxed threshold", n)
    
    list_data_2 <- mr_brain_anx_relaxed
    
  #} else{
    
    #brain_anx_harmonize <- harmonise_data(
      #exposure_dat = brain_clump, 
      #outcome_dat = finn_out
    #)
    
    #mendelian 
    #mr_brain_anx <- mr(brain_anx_harmonize, method_list = mr_method_list()$obj)
    #mr_brain_anx$"exposure" = batch_args[i]
    #mr_brain_anx$"outcome" =  "finn anxiety"
    #n <- dim(mr_brain_anx)[1]
    #mr_brain_anx$"threshold" = rep("stringent threshold", n)
    
    #list_data_2 <- mr_brain_anx
    
    
  #}
  
  list_data <- list("anx_brain" = list_data_1 ,"brain_anx" = list_data_2)
  
  id <- sub(".txt.*", "", batch_args[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format", "", id)
  write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/finngen", id, ".xlsx"))
  
}