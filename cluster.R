#Riccardo Baratto
#librerie
library(tidyverse)
library(cluster)
library(factoextra)
library(caret)
library(reshape2)
library(data.table)
library(ggplot2)
library(GGally)
library(ggdendro)
library(dplyr)

#caricamento dati
data <- fread(file.choose())
data_gower <- data[, .(Spesa_Media = mean(Purchase, na.rm = TRUE)), 
                        by = .(User_ID, Gender, Age, Occupation, City_Category, 
                               Stay_In_Current_City_Years, Marital_Status)]

#variabili categoriche
categorical_cols <- c("Gender", "Age", "Occupation", "Marital_Status", "Stay_In_Current_City_Years", "City_Category")
#cluster
data_gower[, (categorical_cols) := lapply(.SD, as.factor), .SDcols = categorical_cols]
gower <- daisy(data_gower, metric = "gower")
hc <- hclust(gower, method = "ward.D2")
#dendogramma
ggdendrogram(hc,
             rotate = FALSE,             
             theme_dendro = FALSE,       
             size = 0.6) +               
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank())

#silhoutte
sil_width = c(NA)
for(i in 2:10){
  pam_fit = pam(gower,
                diss = TRUE,
                k = i)
  sil_width[i] = pam_fit$silinfo$avg.width
}
silhouette.df = data.frame(x = 2:10,
                           sil_width = sil_width[2:10])
silhouette.df %>%
  ggplot(aes(x,sil_width)) +
  geom_line(col = "#333333",size = 0.8) +
  geom_point(shape = 21,color="#333333",fill = "#F5F200",size = 3) +
  labs(x = "Numero di gruppi",y = "Silhouette") +
  scale_x_continuous(breaks = 2:10)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(family = "CMUSerif"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#assegno i cluster
data_gower$cluster <- cutree(hc, k = 5)
table(data_gower$cluster)

#osservo le distribuzioni
categorical_cols <- c("Gender", "Age", "Occupation", "Marital_Status", "Stay_In_Current_City_Years", "City_Category")
for (col in categorical_cols) {
  cat("\nDistribuzione per:", col, "\n")
  print(prop.table(table(data_gower[[col]], data_gower$cluster), margin = 2))
}

#preparo i dati in formato lungo
data_long <- data_gower %>%
  select(all_of(categorical_cols), cluster) %>%
  pivot_longer(cols = all_of(categorical_cols), names_to = "variabile", values_to = "valore")

#calcolo proporzioni
data_plot <- data_long %>%
  group_by(cluster, variabile, valore) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variabile) %>%
  mutate(prop = n / sum(n))

data_plot %>%
  arrange(cluster, variabile, desc(prop)) %>% filter (cluster==1) %>%
  print(n = 50)  
data_plot %>%
  arrange(cluster, variabile, desc(prop)) %>% filter (cluster==2) %>%
  print(n = 50)  
data_plot %>%
  arrange(cluster, variabile, desc(prop)) %>% filter (cluster==3) %>%
  print(n = 50)  
data_plot %>%
  arrange(cluster, variabile, desc(prop)) %>% filter (cluster==4) %>%
  print(n = 50) 
data_plot %>%
  arrange(cluster, variabile, desc(prop)) %>% filter (cluster==5) %>%
  print(n = 50)  

#grafico variabili x cluster
ggplot(data_plot, aes(x = valore, y = prop, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variabile, scales = "free_x") +
  labs(x = "", y = "Proporzione", fill = "Cluster") +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

#comportamento spesa media nei vari cluster
data_gower %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)))



