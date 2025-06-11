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
library(arules)
library(arulesViz)
library(dplyr)

#caricamento dati
data <- fread(file.choose())

#trasformazione dati
data_gower <- data[, .(Spesa_Media = mean(Purchase, na.rm = TRUE)), 
                        by = .(User_ID, Gender, Age, Occupation, City_Category, 
                               Stay_In_Current_City_Years, Marital_Status)]

#variabili categoriche
categorical_cols <- c("Gender", "Age", "Occupation", "Marital_Status", "Stay_In_Current_City_Years", "City_Category")
#cluster
data_gower[, (categorical_cols) := lapply(.SD, as.factor), .SDcols = categorical_cols]
gower <- daisy(data_gower, metric = "gower")
hc <- hclust(gower, method = "ward.D2") 
ggdendrogram(hc,
             rotate = FALSE,             
             theme_dendro = FALSE,       
             size = 0.6) +               
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank())

sil_width = c(NA)
for(i in 2:10){
  pam_fit = pam(gower,
                diss = TRUE,
                k = i)
  sil_width[i] = pam_fit$silinfo$avg.width
}
silhouette.df = data.frame(x = 2:10,
                           sil_width = sil_width[2:10])


#assegno i cluster
data_gower$cluster <- cutree(hc, k = 5)


#per ogni utente e cluster, raggruppo i prodotti acquistati
data_with_cluster <- merge(data, data_gower[, .(User_ID, cluster)], by = "User_ID", all.x = TRUE)
df_basket <- data_with_cluster %>%
  group_by(User_ID, cluster) %>%
  summarise(items = list(as.character(Product_Category)), .groups = "drop")

#MBA per ogni cluster
unique_clusters <- sort(unique(df_basket$cluster))
rules_by_cluster <- list()
for (cl in unique_clusters) {
  cat("Analisi per il Cluster:", cl, "\n")
  cluster_data <- df_basket %>% filter(cluster == cl)
  trans_list <- as(cluster_data$items, "transactions")
  rules <- apriori(trans_list,
                   parameter = list(supp = 0.2, conf = 0.5,minlen=2,maxlen=3, target = "rules"))
  rules_by_cluster[[paste0("Cluster_", cl)]] <- sort(rules, by = "lift", decreasing = TRUE)
  if (length(rules) > 0) {
    inspect(head(rules_by_cluster[[paste0("Cluster_", cl)]], 5))
  } else {
    cat("Nessuna regola trovata per il Cluster:", cl, "\n")
  }
}

