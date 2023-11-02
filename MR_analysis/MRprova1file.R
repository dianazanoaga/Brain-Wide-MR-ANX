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


# load necessary datasets

mvp_all <- fread("/gpfs/gibbs/pi/polimanti/diana/gad_analysis/input_gad/dbGAP_GAD2eurMVP")
mvp_all <- as.data.frame(mvp_all)
write.csv(mvp_all, file="/gpfs/gibbs/pi/polimanti/diana/mvp_all.txt")


brain_all <- fread("/gpfs/gibbs/pi/polimanti/diana/files_format/0039.txt")
brain_all <- as.data.frame(brain_all)

i = "/gpfs/gibbs/pi/polimanti/diana/files_format/0039.txt"

# select only significant SNPs for both datasets
mvp_sig <- mvp_all[mvp_all$P < 5*10^(-8),]
mvp_sig_relaxed <- mvp_all[mvp_all$P < 1*10^(-5),]

brain_sig <- brain_all[brain_all$pvalue < 5*10^(-8),]
brain_sig_relaxed <- brain_all[brain_all$pvalue < 1*10^(-5),]


# formatting 
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

#clumping anxiety
mvp_clump <- mvp_for

mvp_clump <- clump_data(mvp_clump)

mvp_clump_relaxed <- mvp_for_relaxed
mvp_clump_relaxed <- clump_data(mvp_clump_relaxed)

# clumping brain
brain_clump <- brain_for
brain_clump <- clump_data(brain_clump)

brain_clump_relaxed <- brain_for_relaxed
brain_clump_relaxed <- clump_data(brain_clump_relaxed)



# anx as exposure and brain as outcome

brain_out <- read_outcome_data(
  snps = mvp_clump$SNP,
  sep = ",",
  filename = i,
  snp_col = "rsid",
  beta_col = "beta",
  se_col = "se",
  effect_allele_col = "a2",
  other_allele_col = "a1",
  pval_col = "pvalue",
  samplesize_col = "n"
)


brain_out_relaxed <- read_outcome_data(
  snps = mvp_clump_relaxed$SNP,
  sep = ",",
  filename = i,
  snp_col = "rsid",
  beta_col = "beta",
  se_col = "se",
  effect_allele_col = "a2",
  other_allele_col = "a1",
  pval_col = "pvalue",
  samplesize_col = "n"
)

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
)

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

# harmoniza for anx as exposure and brain as outcome

if(dim(brain_out)[1] <5) {
  
    anx_brain_harmonize_relaxed <- harmonise_data(
    exposure_dat = mvp_clump_relaxed, 
    outcome_dat = brain_out_relaxed
  )
    
    #mendelian 
    mr_anx_brain_relaxed <- mr(anx_brain_harmonize_relaxed, method_list = mr_method_list()$obj)
    mr_anx_brain_relaxed$"exposure" = "mvp anxiety"
    mr_anx_brain_relaxed$"outcome" =  i
    mr_anx_brain_relaxed$"threshold" = rep("relaxed threshold", 17)
    
    list_data_1 <- mr_anx_brain_relaxed
    
    
} else {
  
    anx_brain_harmonize <- harmonise_data(
    exposure_dat = mvp_clump, 
    outcome_dat = brain_out
    )
    
    mr_anx_brain <- mr(anx_brain_harmonize, method_list = mr_method_list()$obj)
    mr_anx_brain$"exposure" = "mvp anxiety"
    mr_anx_brain$"outcome" =  i
    mr_anx_brain$"threshold" = rep("stringent threshold", 17)
    
    list_data_1 <- mr_anx_brain

}
  

# harmonize for brain as exposure and anx as outcome

if(dim(mvp_out)[1] < 5) {
  
    brain_anx_harmonize_relaxed <- harmonise_data(
    exposure_dat = brain_clump_relaxed, 
    outcome_dat = mvp_out_relaxed
  )
  
  #mendelian 
  mr_brain_anx_relaxed <- mr(brain_anx_harmonize_relaxed, method_list = mr_method_list()$obj)
  mr_brain_anx_relaxed$"exposure" = i
  mr_brain_anx_relaxed$"outcome" =  "mvp anxiety"
  mr_brain_anx_relaxed$"threshold" = rep("relaxed threshold", 17)
  
  list_data_2 <- mr_brain_anx_relaxed
  
} else{
  
    brain_anx_harmonize <- harmonise_data(
    exposure_dat = brain_clump, 
    outcome_dat = mvp_out
  )
  
  #mendelian 
  mr_brain_anx <- mr(brain_anx_harmonize, method_list = mr_method_list()$obj)
  mr_brain_anx$"exposure" = i
  mr_brain_anx$"outcome" =  "mvp anxiety"
  mr_brain_anx$"threshold" = rep("stringent threshold", 17)
  
  list_data_2 <- mr_brain_anx
  

}
  
list_data <- list("anx_brain" = list_data_1 ,"brain_anx" = list_data_2)

write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/0039.xlsx"))

