setwd("Z:/UKB/g_analysis/GWAS_Results/wbnojob")
library("qqman")
results_newwell <- read.table("wbnojob_dupremoved_maf0.01_info0.8_hwe_p0.05_plot", head=TRUE)

manhattan(results_newwell,chr="CHR",bp="BP",p="P_BOLT_LMM_INF",snp="SNP",
          main = "Wellbeing index", col = c("gray40","#D4AF37"), 
          annotatePval = 5e-8, ylim = c(2,10))
