# This preliminary analysis evaluate the weight of different PCs

library(dslabs)
library(tidyverse)

tissue_gene_expression <- data("tissue_gene_expression")  %>%
save(tissue_gene_expression, file = 'rdas/tissue_gene_expression.rda')

load('rdas/tissue_gene_expression.rda')
pca <- prcomp(tissue_gene_expression$x)


pc <- 1:nrow(tissue_gene_expression$x) # As the n of obser. is less than predictors, the n of PCs is equal to n of obser.
qplot(pc, pca$sdev) + # to see the importance of PCs
xlab('PCA number') + ylab('PCA standard deviation') +
  ggsave('fig/PC-sd.png')
