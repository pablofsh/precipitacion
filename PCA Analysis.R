
# Librerias 
# install.packages(c("corrr","ggcorrplot","FactoMineR","factoextra"))

library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(tidyverse)


# Leer datos
data <- read_delim("inamhi-precipitacion-2019diciembre.csv",
                   delim = ";",
                   locale = locale(decimal_mark = ","))

# Cuentas frecuencias
data %>% 
  count(Estacion)

# Los casos unicos
data %>% 
  distinct(Estacion)

# Años disintos
data %>% 
  distinct(anio)

# Cuantas estaciones tienen información completa?

estaciones <- data %>% 
  group_by(Estacion) %>% 
  summarise(anios = n_distinct(anio))


estaciones %>% 
  mutate(completo_dummy = as.numeric(anios > 50)) %>% 
  count(completo_dummy) %>% 
  rowwise() %>% 
  mutate(prop = n/sum(n))


resumen <- starwars %>% 
  count(hair_color,skin_color,eye_color) %>% 
  mutate(prop = n/sum(n))

resumen <- starwars %>% 
  count(hair_color,skin_color,eye_color) %>% 
  group_by(hair_color) %>% 
  mutate(prop = n/sum(n))
  

sum(resumen$prop)

estaciones_15 <- data %>% 
  filter(anio == 2015)

est_mat <- estaciones_15 %>% 
  select(ene:dic)

rownames(est_mat) <- estaciones_15$Estacion

class(est_mat)


data_clean <- na.omit(est_mat)


data_normalized <- scale(as.data.frame(data_clean))

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
