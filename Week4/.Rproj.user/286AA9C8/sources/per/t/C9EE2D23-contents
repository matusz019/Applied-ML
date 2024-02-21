gene_expr <- read.csv("gene_counts.csv", row.names = 1)
gene_sample_info <- read.csv("sample_gene_info.csv", row.names=1)

head(gene_expr)
dim(gene_expr)
head(gene_sample_info)
dim

gene_expr<-t(gene_expr)
dim(gene_expr)

gene_pca <- prcomp(as.matrix(gene_expr))
summary(gene_pca)

pc_scores<-as.data.frame(gene_pca$x)
dim(pc_scores)

ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point()

gene_sample_info
pc_scores_info<-merge(pc_scores, gene_sample_info, by=0)
str(pc_scores_info)

loadings<-gene_pca$rotation
#Extract the loadings for the first PC
loadings<-gene_pca$rotation[,1]
# to remove sign (direction of the transformation) but look only the magnitude

loadings<-abs(loadings)
#sort PCA in the decreasing order
loadings<-sort(loadings,decreasing = T)
loadings
#select 10 top names of the original variables based on the higest weights
top10<-names(loadings[1:10])
gene_pca$rotation[top10, 1]

#Extract the loadings for the second PC
loadings2<-gene_pca$rotation[,2]
loadings2<-abs(loadings2)
loadings2<-sort(loadings2,decreasing = T)
loadings2
top10_2<-names(loadings2[1:10])
gene_pca$rotation[top10_2, 2]

top_genes<-unique(c(top10, top10_2))
pc_loadings <-as.data.frame(gene_pca$rotation[top_genes,1:2])

pc_loadings$gene<-rownames(pc_loadings)
ggplot(pc_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               col = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

library(factoextra)
fviz_pca_var(gene_pca, col.var = "red", select.var = list(name=top_genes))
km<-kmeans(gene_expr, 6, nstart=25)
head(pc_scores)
clusters<-data.frame(Cluster=km$cluster)

cluster_scores<-merge(pc_scores, clusters, by="row.names")
rownames(cluster_scores)<-cluster_scores$Row.names
names(cluster_scores)

ggplot(cluster_scores, aes(x = PC1,y = PC2, col = Cluster,)) +
  geom_point()+
  geom_text(aes(x = PC1, y = PC2, label = rownames(cluster_scores)),
            nudge_y = 1, size = 2)+
  scale_color_gradient(low="blue", high="red")
