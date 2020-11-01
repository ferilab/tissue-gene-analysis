# We'll try to figure out the minimum required number of PCs for a good classfication of 
# tissues usin their gene expressions in experimental observations

library(tidyverse)

load('rdas/tissue_gene_expression.rda')
pca <- prcomp(tissue_gene_expression$x)

# How good only two PCs will work 
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=tissue_gene_expression$y) %>%
  ggplot(aes(PC1, PC2, fill=label))+
  geom_point(cex=3, pch=21) + 
  ggsave('fig/PC1-vs-PC2.png')

# is only PC1 and observation average enough?
data.frame(PC1 = pca$x[,1], avg = rowMeans(tissue_gene_expression$x), 
           label=tissue_gene_expression$y) %>%
  ggplot(aes(PC1, avg, fill=label)) +
  geom_point(cex=3, pch=21)

cor(pca$x[,1], rowMeans(tissue_gene_expression$x))

col_means <- colMeans(tissue_gene_expression$x)
x_test <- sweep(tissue_gene_expression$x, 2, col_means) %*% pca$rotation

data.frame(PC1 = x_test[,1], avg = rowMeans(tissue_gene_expression$x), 
           label=tissue_gene_expression$y) %>%
  ggplot(aes(PC1, avg, fill=label))+
  geom_point(cex=3, pch=21) + ylab('Observation average (over all gene expressions)')
ggsave('fig/PC-vs-avg.png')
cor(x_test[,1], rowMeans(tissue_gene_expression$x))

# and finally, how many PCs do we need
ten_PC <- as.data.frame(x_test[,1:10]) %>% mutate(labels = tissue_gene_expression$y)
ten_PC %>% gather(PCs, value, -labels) %>%
  ggplot(aes(labels, value, fill = labels)) +
  geom_boxplot() +
  facet_wrap(~PCs, scales = "free", ncol = 4) +
  theme(axis.text.x = element_blank(), legend.position="bottom") +
  ggsave('fig/PCs-lables.png')
