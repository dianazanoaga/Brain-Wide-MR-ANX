#!/usr/bin/env Rscript

library(remotes)
library(readxl)
library(data.table)
library("plyr")
library("dplyr")
library(meta)
library(purrr)
library(metafor)
library(rio) # per leggere piu sheets di R
library(WriteXLS)
library(writexl)
library(MendelianRandomization)
library(tidyverse)
library(TwoSampleMR)

library(MRPRESSO)



# Load summary statistics 

my_list = c("/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0953.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0043.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1961.txt",
            "/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1511.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1437.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/3915.txt")


mvp_all <- fread("/gpfs/gibbs/pi/polimanti/diana/gad_analysis/input_gad/dbGAP_GAD2eurMVP")


for(i in 1:length(my_list)) {  # assign function within loop
  
  brain <- fread(my_list[i])
  #brain_sig_relaxed <- brain[brain$pvalue < 1*10^(-5)]
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

# all 6

mvp_pre <- mvp_all

# select only those with pvalu less than 1x10(-5) and clump data 
for(i in 1:6){
  
  assign(paste0("b_",i), get(paste0("brain_", i))[get(paste0("brain_", i))$pval.exposure < 1*10^(-5),])
  assign(paste0("b_", i, "_clump"), clump_data(get(paste0("b_",i))))
  
} 

# select snps in the outcome based on the snps in the exposure AND HARMONIZE

for(i in 1:6) {
  assign(paste0("mvp_out_",i), read_outcome_data(
    snps = get(paste0("b_", i, "_clump"))$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/mvp_all.txt",
    snp_col = "rsid",
    beta_col = "EFFECT",
    se_col = "SE",
    effect_allele_col = "A1",
    other_allele_col = "A2",
    pval_col = "P",
    samplesize_col = "N"))
  
  
  assign(paste0("brain_anx_harmonize_",i), harmonise_data(
    exposure_dat = get(paste0("b_", i, "_clump")), 
    outcome_dat = get(paste0("mvp_out_", i))
  ))
  
  
}


mr_res_presso_mvp <- as.data.frame(matrix(nrow=6, ncol=1))
mr_res_presso_mvp$out_snp_mvp <- NULL
mr_res_presso_mvp$out_ind_mvp <- NULL



for(i in c(1:6)){
  
  print(i)
  
  id <- sub(".txt.*", "", my_list[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/", "", id)

  write_xlsx(get(paste0("brain_anx_harmonize_", i)), paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_mvp_harmonize.xlsx"))
  
  #data <- get(paste0("brain_anx_harmonize_",i))
  #data <- as.data.frame(data)
  print(id)

  dataexcel <- read_excel(paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_mvp_harmonize.xlsx"))
  dataexcel <- as.data.frame(dataexcel)
  
  presso_mvp <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dataexcel, NbDistribution = 10000, SignifThreshold = 0.05)
  
  mr_res <- as.data.frame(presso_mvp$`Main MR results`)
  print(mr_res)
  print(presso_mvp[["MR-PRESSO results"]][["Outlier Test"]])
  print(presso_mvp[["MR-PRESSO results"]][["Global Test"]])
  print(presso_mvp$`MR-PRESSO results`$`Distortion Test`)
  
  mr_res_presso_mvp$exposure[i] <- my_list[i]
  mr_res_presso_mvp$outcome[i] <- "mvp"
  mr_res_presso_mvp$n_mvp[i] <-  dim(get(paste0("brain_anx_harmonize_",i)))[1]
  mr_res_presso_mvp$raw_b_mvp[i] <- mr_res$`Causal Estimate`[1]
  mr_res_presso_mvp$raw_sd_mvp[i] <- mr_res$Sd[1]
  mr_res_presso_mvp$raw_t_mvp[i] <- mr_res$`T-stat`[1]
  mr_res_presso_mvp$raw_p_mvp[i] <- mr_res$`P-value`[1]
  mr_res_presso_mvp$corr_b_mvp[i] <- mr_res$`Causal Estimate`[2]
  mr_res_presso_mvp$corr_sd_mvp[i] <- mr_res$Sd[2]
  mr_res_presso_mvp$corr_t_mvp[i] <- mr_res$`T-stat`[2]
  mr_res_presso_mvp$corr_p_mvp[i] <- mr_res$`P-value`[2]
  mr_res_presso_mvp$global_test_rsobs_mvp[i] <- presso_mvp$`MR-PRESSO results`$`Global Test`$RSSobs
  mr_res_presso_mvp$global_test_pval_mvp[i] <- presso_mvp$`MR-PRESSO results`$`Global Test`$Pvalue
  
  #mr_res_presso_mvp$out_snp_mvp[i] <- NA
  #mr_res_presso_mvp$out_ind_mvp[i] <- NA
  mr_res_presso_mvp$dist_coef_mvp[i] <- NA
  mr_res_presso_mvp$dist_pval_mvp[i] <- NA
  
  
  if(!is.null(presso_mvp$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)){
    
    indici <- as.list(presso_mvp$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)
    #mr_res_presso_mvp$out_ind_mvp[i] <- NA
    mr_res_presso_mvp$out_ind_mvp[[i]] <- indici
    
    snps <- list()
    for (k in seq_along(indici)) {
      snps[[k]] <- get(paste0("brain_anx_harmonize_", i))[indici[[k]], "SNP"]
    }
    
    print(snps)
    #mr_res_presso_mvp$out_snp_mvp[i] <- NA
    mr_res_presso_mvp$out_snp_mvp[[i]] <- snps
    
    mr_res_presso_mvp$dist_coef_mvp[i] <- presso_mvp$`MR-PRESSO results`$`Distortion Test`$`Distortion Coefficient`
    mr_res_presso_mvp$dist_pval_mvp[i] <- presso_mvp$`MR-PRESSO results`$`Distortion Test`$Pvalue
    
  }
  
}

mry <- mr_res_presso_mvp

mr_res_presso_mvp[, c(17,18)] <- lapply(mr_res_presso_mvp[, c(17,18)], as.character)

#write_xlsx(mr_res_presso_mvp[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/mvp_presso_new.xlsx"))
#write_xlsx(mr_res_presso_mvp[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/mvp_presso_new_secondotrial.xlsx"))
#write_xlsx(mr_res_presso_mvp[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/mvp_presso_new_secondotrial_harm.xlsx"))
write_xlsx(mr_res_presso_mvp[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/mvp_presso_harm_final.xlsx"))

