#!/usr/bin/env Rscript

library(remotes)
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
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

finn_all <- fread("/gpfs/gibbs/pi/polimanti/diana/gad_analysis/input_gad/finngen_R8_KRA_PSY_ANXIETY_EXMORE")

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

# select only those with pvalue less than 1x10^(-5) and clump data 

for(i in 1:6){
  
  assign(paste0("b_", i), get(paste0("brain_", i))[get(paste0("brain_", i))$pval.exposure < 1*10^(-5),])
  assign(paste0("b_", i, "_clump"), clump_data(get(paste0("b_", i))))
  
} 

# select snps in the outcome based on the snps in the exposure AND HARMONIZE

for(i in 1:length(my_list)) {

  assign(paste0("finn_out_",i), read_outcome_data(

      snps = get(paste0("b_", i, "_clump"))$SNP,
      sep = ",",
      filename = "/gpfs/gibbs/pi/polimanti/diana/finn_all.txt",
      snp_col = "rsids",
      beta_col = "beta",
      se_col = "sebeta",
      effect_allele_col = "alt",
      other_allele_col = "ref",
      pval_col = "pval")

    )
  
 
  assign(paste0("brain_anx_harmonize_", i), harmonise_data(

    exposure_dat = get(paste0("b_", i, "_clump")), 
    outcome_dat = get(paste0("finn_out_", i)))

  )

}

mr_res_presso_finn <- as.data.frame(matrix(nrow=6, ncol=1))

mr_res_presso_finn$out_ind_finn <- NULL
mr_res_presso_finn$out_snp_finn <- NULL


for(i in 1:length(my_list)){
  
  print(i)
  
  id <- sub(".txt.*", "", my_list[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/", "", id)

  print(id)
  write_xlsx(get(paste0("brain_anx_harmonize_", i)), paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_finn_harmonize.xlsx"))
  
  dataex <- read_excel(paste0("/gpfs/gibbs/pi/polimanti/diana/MR_beta_exp_combined/", id, "_finn_harmonize.xlsx"))
  dataex <- as.data.frame(dataex)
  presso_finn <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dataex, NbDistribution = 10000, SignifThreshold = 0.05)
  
  
  mr_res<- as.data.frame(presso_finn$`Main MR results`)
  print(mr_res)
  print(presso_finn[["MR-PRESSO results"]][["Outlier Test"]])
  print(presso_finn[["MR-PRESSO results"]][["Global Test"]])
  print(presso_finn$`MR-PRESSO results`$`Distortion Test`)
  
  mr_res_presso_finn$exposure[i] <- my_list[i]
  mr_res_presso_finn$outcome[i] <- "finn"
  mr_res_presso_finn$n_finn[i] <-  dim(get(paste0("brain_anx_harmonize_", i)))[1]
  mr_res_presso_finn$raw_b_finn[i] <- mr_res$`Causal Estimate`[1]
  mr_res_presso_finn$raw_sd_finn[i] <- mr_res$Sd[1]
  mr_res_presso_finn$raw_t_finn[i] <- mr_res$`T-stat`[1]
  mr_res_presso_finn$raw_p_finn[i] <- mr_res$`P-value`[1]
  mr_res_presso_finn$corr_b_finn[i] <- mr_res$`Causal Estimate`[2]
  mr_res_presso_finn$corr_sd_finn[i] <- mr_res$Sd[2]
  mr_res_presso_finn$corr_t_finn[i] <- mr_res$`T-stat`[2]
  mr_res_presso_finn$corr_p_finn[i] <- mr_res$`P-value`[2]
  mr_res_presso_finn$global_test_rsobs_finn[i] <- presso_finn$`MR-PRESSO results`$`Global Test`$RSSobs
  mr_res_presso_finn$global_test_pval_finn[i] <- presso_finn$`MR-PRESSO results`$`Global Test`$Pvalue
  
  
  mr_res_presso_finn$dist_coef_finn[i] <- NA
  mr_res_presso_finn$dist_pval_finn[i] <- NA
  
  
  if(!is.null(presso_finn$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)){
    
    indici <- as.list(presso_finn$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)
    mr_res_presso_finn$out_ind_finn[[i]] <- indici
    
    snps <- list()
    for (k in seq_along(indici)) {
      snps[[k]] <- get(paste0("brain_anx_harmonize_", i))[indici[[k]], "SNP"]
    }
    
    print(snps)
    mr_res_presso_finn$out_snp_finn[[i]] <- snps
    
    mr_res_presso_finn$dist_coef_finn[i] <- presso_finn$`MR-PRESSO results`$`Distortion Test`$`Distortion Coefficient`
    mr_res_presso_finn$dist_pval_finn[i] <- presso_finn$`MR-PRESSO results`$`Distortion Test`$Pvalue
    
    
  }
  
}
jj <- mr_res_presso_finn

mr_res_presso_finn[, c(17,18)] <- lapply(mr_res_presso_finn[, c(17,18)], as.character)
write_xlsx(mr_res_presso_finn[,-1], paste0("/gpfs/gibbs/pi/polimanti/diana/finn_presso_harm_final.xlsx"))