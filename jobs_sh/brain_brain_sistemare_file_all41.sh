#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=gem_corr_file
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=10g

module load miniconda
conda activate ldsc
touch brain_brain_summaryfile_all41.txt
cd /gpfs/gibbs/pi/polimanti/diana/corr_analysis/brain_brain_usando41
for file in *.proc.txt.out.sumstats.gz.log
do tail -4  /gpfs/gibbs/pi/polimanti/diana/corr_analysis/brain_brain_usando41/$file | head -1 >> /gpfs/gibbs/pi/polimanti/diana/brain_brain_summaryfile_all41.txt
done