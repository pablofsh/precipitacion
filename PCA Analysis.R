
# Librerias 
# install.packages(c("corrr","ggcorrplot","FactoMineR","factoextra"))

library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(tidyverse)


# Leer datos
data <- read.delim("data/procesada/base_final_para_modelo.txt", stringsAsFactors=TRUE)

# NA Values 
colSums(is.na(data))

data_clean <- na.omit(data)

# 1. Normalizar los datos 
numerical_data <- data_clean[, c("edadmeses","fexp_nino","control_nino_sano","n_hijos","estrato")]
data_normalized <- scale(numerical_data)

# 2. Compute correlation matrix 
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

# 3. Apply PCA
data_pca <- princomp(corr_matrix)
summary(data_pca)

data_pca$loadings[, 1:3]

# Scree Plot 
fviz_eig(data_pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data_pca, col.var = "black")
