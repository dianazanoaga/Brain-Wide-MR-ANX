#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=gem_corr_file
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=10g

module load miniconda
conda activate ldsc
touch finngen_brain_summaryfile.txt
cd /gpfs/gibbs/pi/polimanti/diana/corr_analysis/corr_finngen_brain
for file in {0001..3935}.proc.txt.out.sumstats.gz.gc.log;
do tail -4  /gpfs/gibbs/pi/polimanti/diana/corr_analysis/corr_finngen_brain/$file | head -1 >> /gpfs/gibbs/pi/polimanti/diana/finngen_brain_summaryfile.txt
done
