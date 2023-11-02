library(meta)
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



mr_res_presso_finn <- read_excel("/gpfs/gibbs/pi/polimanti/diana/finn_presso_harm_final.xlsx")
mr_res_presso_mvp <- read_excel("/gpfs/gibbs/pi/polimanti/diana/mvp_presso_harm_final.xlsx")
mr_res_presso_ukbio <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ukbio_presso_harm_final.xlsx")


tutti <- cbind(mr_res_presso_finn, mr_res_presso_mvp, mr_res_presso_ukbio)



for(i in c(1:6)){
  
  
  if(i == 1){

    beta <- c(tutti$corr_b_mvp[i], tutti$raw_b_finn[i], tutti$raw_b_ukbio[i])
    se <- c(tutti$corr_sd_mvp[i], tutti$raw_sd_finn[i], tutti$raw_sd_ukbio[i])
    data <- data.frame(r = beta, se = se)
    #meta_result_reml <- rma.uni(r, sei = se , data = data)
    meta_result_common <- rma.uni(r, sei = se , data = data, method= "EE")
    meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
    
    #dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
    tutti$"meta_b_common"[i] <- meta_result_common$beta
    tutti$"meta_se_common"[i] <- meta_result_common$se
    tutti$"meta_pval_common"[i] <- meta_result_common$pval
    tutti$"meta_het_common"[i] <- meta_result_common$QEp
    tutti$"meta_pval_dl"[i] <- meta_result_dl$pval

  } else if(i==2){
    beta <- c(tutti$raw_b_mvp[i], tutti$corr_b_finn[i], tutti$raw_b_ukbio[i])
    se <- c(tutti$raw_sd_mvp[i], tutti$corr_sd_finn[i], tutti$raw_sd_ukbio[i])
    data <- data.frame(r = beta, se = se)
    #meta_result_reml <- rma.uni(r, sei = se , data = data)
    meta_result_common <- rma.uni(r, sei = se , data = data, method= "EE")
    meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
    
    #dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
    tutti$"meta_b_common"[i] <- meta_result_common$beta
    tutti$"meta_se_common"[i] <- meta_result_common$se
    tutti$"meta_pval_common"[i] <- meta_result_common$pval
    tutti$"meta_het_common"[i] <- meta_result_common$QEp
    tutti$"meta_pval_dl"[i] <- meta_result_dl$pval
    
    
  } else if(i ==3 | i ==4 ){
    
    beta <- c(tutti$corr_b_mvp[i], tutti$corr_b_finn[i], tutti$raw_b_ukbio[i])
    se <- c(tutti$corr_sd_mvp[i], tutti$corr_sd_finn[i], tutti$raw_sd_ukbio[i])
    data <- data.frame(r = beta, se = se)
    #meta_result_reml <- rma.uni(r, sei = se , data = data)
    meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
    meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
    
    #dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
    tutti$"meta_b_common"[i] <- meta_result_common$beta
    tutti$"meta_se_common"[i] <- meta_result_common$se
    tutti$"meta_pval_common"[i] <- meta_result_common$pval
    tutti$"meta_het_common"[i] <- meta_result_common$QEp
    tutti$"meta_pval_dl"[i] <- meta_result_dl$pval
    
  } else if(i == 5){
    
    beta <- c(tutti$raw_b_mvp[i], tutti$raw_b_finn[i], tutti$raw_b_ukbio[i])
    se <- c(tutti$raw_sd_mvp[i], tutti$raw_sd_finn[i], tutti$raw_sd_ukbio[i])
    data <- data.frame(r = beta, se = se)
    #meta_result_reml <- rma.uni(r, sei = se , data = data)
    meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
    meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
    
    #dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
    tutti$"meta_b_common"[i] <- meta_result_common$beta
    tutti$"meta_se_common"[i] <- meta_result_common$se
    tutti$"meta_pval_common"[i] <- meta_result_common$pval
    tutti$"meta_het_common"[i] <- meta_result_common$QEp
    tutti$"meta_pval_dl"[i] <- meta_result_dl$pval
    
  } else {
    
    beta <- c(tutti$raw_b_mvp[i], tutti$corr_b_finn[i], tutti$raw_b_ukbio[i])
    se <- c(tutti$raw_sd_mvp[i], tutti$corr_sd_finn[i], tutti$raw_sd_ukbio[i])
    data <- data.frame(r = beta, se = se)
    #meta_result_reml <- rma.uni(r, sei = se , data = data)
    meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
    meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
    
    #dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
    tutti$"meta_b_common"[i] <- meta_result_common$beta
    tutti$"meta_se_common"[i] <- meta_result_common$se
    tutti$"meta_pval_common"[i] <- meta_result_common$pval
    tutti$"meta_het_common"[i] <- meta_result_common$QEp
    tutti$"meta_pval_dl"[i] <- meta_result_dl$pval
    
  }
}

tutti$"p_adj_common" <- p.adjust(tutti$"meta_pval_common" , method="fdr")
tutti$"p_adj_dl" <- p.adjust(tutti$"meta_pval_dl" , method="fdr")

tutti$"p_adj_het_common" <- p.adjust(tutti$"meta_het_common" , method="fdr")

#write_xlsx(tutti, "/gpfs/gibbs/pi/polimanti/diana/meta_presso_MR.xlsx")
write_xlsx(tutti, "/gpfs/gibbs/pi/polimanti/diana/meta_presso_MR_harm_FINAL.xlsx")
