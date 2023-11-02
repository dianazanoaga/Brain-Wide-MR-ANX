#!/usr/bin/env Rscript

library(remotes)
library(readxl)
library(data.table)
library("plyr")
library("dplyr")
library(meta)
library(purrr)
library(metafor)
library(rio) 
library(WriteXLS)
library(writexl)
library(MendelianRandomization)
library(tidyverse)
library(TwoSampleMR)
library(MRPRESSO)

# Load summary statistics 

my_list = c("/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0953.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0043.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1961.txt",
            "/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1511.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1437.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/3915.txt")

ukbio_all <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")


for(i in 1:length(my_list)) {  # assign function within loop
  
  brain <- fread(my_list[i])
  assign(paste0("brain_", i), format_data(brain, 
                                          type="exposure", 
                                          phenotype_col = "brain",
                                          snp_col = "rsid",
                                          beta_col = "beta",
                                          se_col = "se",
                                          effect_allele_col = "a2",
                                          other_allele_col = "a1",
                                          pval_col = "pvalue"))
}


for(i in 1:6){
  
  assign(paste0("b_",i), get(paste0("brain_", i))[get(paste0("brain_", i))$pval.exposure < 1*10^(-5),])
  assign(paste0("b_", i, "_clump"), clump_data(get(paste0("b_",i))))
  
} 

# select snps in the outcome based on the snps in the exposure AND HARMONIZE

for(i in 1:6) {

  assign(paste0("ukbio_out_",i), read_outcome_data(

    snps = get(paste0("b_", i, "_clump"))$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt",
    snp_col = "rsid",
    beta_col = "beta",
    se_col = "se",
    effect_allele_col = "alt.x",
    other_allele_col = "ref.x",
    pval_col = "pval_EUR")
    
    )
  
  assign(paste0("brain_anx_harmonize_",i), harmonise_data(

    exposure_dat = get(paste0("b_", i, "_clump")), 
    outcome_dat = get(paste0("ukbio_out_", i)))
    
    )
   
}

mr_res_presso_ukbio <- as.data.frame(matrix(nrow=6, ncol=1))

mr_res_presso_ukbio$out_ind_ukbio <- NULL
mr_res_presso_ukbio$out_snp_ukbio <- NULL

for(i in c(1:6)){
  
  print(i)
  
  id <- sub(".txt.*", "", my_list[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/", "", id)
  
  print(id)
  
  write_xlsx(get(paste0("brain_anx_harmonize_", i)), paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_ukbio_harmonize.xlsx"))

  dataexcel <- read_excel(paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_ukbio_harmonize.xlsx"))
  dataexcel <- as.data.frame(dataexcel)
  
  presso_ukbio <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dataexcel, NbDistribution = 10000, SignifThreshold = 0.05)
  
  mr_res<- as.data.frame(presso_ukbio$`Main MR results`)
  
  print(mr_res)
  print(mr_res)
  print(presso_ukbio[["MR-PRESSO results"]][["Outlier Test"]])
  print(presso_ukbio[["MR-PRESSO results"]][["Global Test"]])
  print(presso_ukbio$`MR-PRESSO results`$`Distortion Test`)
  
  mr_res_presso_ukbio$exposure[i] <- my_list[i]
  mr_res_presso_ukbio$outcome[i] <- "ukbio"
  mr_res_presso_ukbio$n_ukbio[i] <-  dim(get(paste0("brain_anx_harmonize_", i)))[1]
  mr_res_presso_ukbio$raw_b_ukbio[i] <- mr_res$`Causal Estimate`[1]
  mr_res_presso_ukbio$raw_sd_ukbio[i] <- mr_res$Sd[1]
  mr_res_presso_ukbio$raw_t_ukbio[i] <- mr_res$`T-stat`[1]
  mr_res_presso_ukbio$raw_p_ukbio[i] <- mr_res$`P-value`[1]
  mr_res_presso_ukbio$corr_b_ukbio[i] <- mr_res$`Causal Estimate`[2]
  mr_res_presso_ukbio$corr_sd_ukbio[i] <- mr_res$Sd[2]
  mr_res_presso_ukbio$corr_t_ukbio[i] <- mr_res$`T-stat`[2]
  mr_res_presso_ukbio$corr_p_ukbio[i] <- mr_res$`P-value`[2]
  mr_res_presso_ukbio$global_test_rsobs_ukbio[i] <- presso_ukbio$`MR-PRESSO results`$`Global Test`$RSSobs
  mr_res_presso_ukbio$global_test_pval_ukbio[i] <- presso_ukbio$`MR-PRESSO results`$`Global Test`$Pvalue
  
  mr_res_presso_ukbio$dist_coef_ukbio[i] <- NA
  mr_res_presso_ukbio$dist_pval_ukbio[i] <- NA
  
  if(!is.null(presso_ukbio$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)){
    
    indici <- as.list(presso_ukbio$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)
    mr_res_presso_ukbio$out_ind_ukbio[[i]] <- indici
    
    snps <- list()
    for (k in seq_along(indici)) {
      snps[[k]] <- get(paste0("brain_anx_harmonize_", i))[indici[[k]], "SNP"]
    }
    
    print(snps)
    mr_res_presso_ukbio$out_snp_ukbio[[i]] <- snps
    
    mr_res_presso_ukbio$dist_coef_ukbio[i] <- presso_ukbio$`MR-PRESSO results`$`Distortion Test`$`Distortion Coefficient`
    mr_res_presso_ukbio$dist_pval_ukbio[i] <- presso_ukbio$`MR-PRESSO results`$`Distortion Test`$Pvalue
    
    
  }
  
}

ss <- mr_res_presso_ukbio
mr_res_presso_ukbio[,c(17,18)] <- lapply(mr_res_presso_ukbio[,c(17,18)], as.character)
write_xlsx(mr_res_presso_ukbio[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/ukbio_presso_harm_final.xlsx"))