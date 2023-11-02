#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=brain.imaging.gen1
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=15g
module load miniconda
conda activate ldsc
cd /gpfs/gibbs/pi/polimanti/diana/brain_analysis/output	
for file in {0001..1967}.proc.txt.out.sumstats.gz;
do python /gpfs/ysm/project/polimanti/mz575/ldsc/ldsc.py --rg /gpfs/gibbs/pi/polimanti/diana/gad_analysis/munged/EUR_phecode-300-both_sexes.tsv.sumstats,/gpfs/gibbs/pi/polimanti/diana/brain_analysis/output/$file --ref-ld-chr /gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr/ --w-ld-chr /gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr/ --out /gpfs/gibbs/pi/polimanti/diana/corr_analysis/corr_uk_brain/$file.gc
done
