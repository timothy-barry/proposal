library(ondisc)
library(magrittr)
library(ppcor)
library(dplyr)
set.seed(10)

# compute the gene-gene and gRNA-gRNA marginal and partial correlations
odm_fp <- "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gene/gasp_scale_gene_expressions.odm"
metadata_fp <- "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gene/gasp_scale_gene_metadata.rds"
odm_gene <- read_odm(odm_fp, metadata_fp)

odm_fp <- "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gRNA_grouped/gasp_scale_gRNA_counts_grouped.odm"
metadata_fp <- "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gRNA_grouped/gasp_scale_gRNA_metadata_grouped.rds"
odm_gRNA <- read_odm(odm_fp, metadata_fp)

# compute correlations for 1000 randomly-selected pairs
f <- function(odm, covariate_m, B = 2000, thresh = NULL) {
  g1 <- sample(x = seq(1, nrow(odm)), size = B, replace = FALSE)
  g2 <-  sample(x = seq(1, nrow(odm)), size = B, replace = FALSE)
  if (any(g1 == g2)) stop()
  cor_coefs <- sapply(X = seq(1, B), FUN = function(i) {
    if (i %% 100 == 0) print(i)
    e1 <- odm[[g1[i],]] %>% as.numeric()
    e2 <- odm[[g2[i],]] %>% as.numeric()
    if (!is.null(thresh)) {
      e1 <- as.integer(e1 >= thresh)
      e2 <- as.integer(e2 >= thresh)
    }
    m_cor <- cor(e1, e2)
    p_cor_all <- pcor(data.frame(e1 = e1, e2 = e2, covariate_m))
    p_cor <- p_cor_all$estimate["e1", "e2"]
    return(c(m_cor = m_cor, p_cor = p_cor))
  })
  return(cor_coefs)
}

# Feature independence is a reasonable first-order approximation
gene_cor <- f(odm = odm_gene, covariate_m = odm_gene %>% get_cell_covariates() %>% select(-batch))
gene_m_cor <- gene_cor["m_cor",]
gene_p_cor <- gene_cor["p_cor",]

gRNA_cor <- f(odm_gRNA, odm_gRNA %>% get_cell_covariates())
gRNA_m_cor <- gRNA_cor["m_cor",]
gRNA_p_cor <- gRNA_cor["p_cor",]


par(mfrow=c(2, 2))
hist(gene_m_cor, main = "Gene marginal cor.", xlab = "")
hist(gene_p_cor, main = "Gene partial cor.", xlab = "")
hist(gRNA_m_cor, main = "gRNA marginal cor.", xlab = "")
hist(gRNA_p_cor, main = "gRNA partial cor.", xlab = "")

# fraction of correlations that are "negligible": |p| < 0.1
mean(gene_p_cor < 0.1 & gene_p_cor > -0.1) * 100
mean(gRNA_p_cor < 0.1 & gRNA_p_cor > -0.1) * 100
