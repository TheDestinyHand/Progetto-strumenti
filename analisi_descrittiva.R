#Riccardo Baratto
#ANALISI DESCRITTIVA
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(summarytools)
library(readr)
library(corrplot)
library(plotly)
library(dplyr)
library(viridis) 
#caricamento dati
df <- read_csv(file.choose())  

#trasformazione variabili
df <- df %>%
  mutate(
    Occupation = as.factor(Occupation),
    Marital_Status = as.factor(Marital_Status),
    Product_Category = as.factor(Product_Category),
    Gender = as.factor(Gender),
    City_Category = as.factor(City_Category),
    Stay_In_Current_City_Years = as.factor(Stay_In_Current_City_Years),
    Age = as.factor(Age)
  )
#panoramica dati
summary(df)

age_list <- unique(df$Age)
years_list <- levels(df$Stay_In_Current_City_Years)
city_list <- levels(df$City_Category)
categorical_cols <- c("Gender", "Age", "Occupation", "City_Category", 
                      "Stay_In_Current_City_Years", "Marital_Status", "Product_Category")








df2 <- df %>%
  group_by(User_ID, Product_Category, Gender, Age, Occupation,
           City_Category, Stay_In_Current_City_Years, Marital_Status) %>%
  summarise(Spesa_Media = mean(Purchase, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    across(where(is.character), as.factor),
    Marital_Status = as.factor(Marital_Status),
    Occupation = as.factor(Occupation),
    Product_Category = as.factor(Product_Category),
    Stay_In_Current_City_Years = as.factor(Stay_In_Current_City_Years),
    Age = factor(Age, levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"))
  )



#istogramma spesa media x variabili categoriali
for (col in categorical_cols) {
  plot_data <- df2 %>%
    group_by(.data[[col]]) %>%
    summarise(Spesa_Media = mean(Spesa_Media, na.rm = TRUE)) 
  
  print(
    ggplot(plot_data, aes(x = (.data[[col]]), y = Spesa_Media, fill = .data[[col]])) +
      geom_col(width = 0.7, color = "white", show.legend = FALSE) +
      labs(
        title = paste("Spesa media per", col),
        x = NULL,
        y = "Spesa media"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  )
}



#boxplot spesa media x categoria prodotto
ggplot(df, aes(x = factor(Product_Category), y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(x = "Categoria prodotto", y = "Spesa", title = "Distribuzione della spesa per categoria") +
  theme_minimal()

#boxplot spesa media x fascia d'età
ggplot(df, aes(x = Age, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per fascia d'età") +
  theme_minimal()

#boxplot spesa media x categoria città
ggplot(df, aes(x = City_Category, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per categoria di città") +
  theme_minimal()

#boxplot spesa media x stato civile
ggplot(df, aes(x = Marital_Status, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per stato civile") +
  theme_minimal()

#boxplot spesa media x genere
ggplot(df, aes(x = Gender, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per genere") +
  theme_minimal()

#boxplot spesa media x occupazione
ggplot(df, aes(x = Occupation, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per occupazione") +
  theme_minimal()

#boxplot spesa media x anno di residenza
ggplot(df, aes(x = Stay_In_Current_City_Years, y = Purchase)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Spesa per anni di residenza") +
  theme_minimal()

#boxplot spesa media x categoria città A x categoria prodotto
df %>%
  filter(City_Category == "A") %>%
  ggplot(aes(x = factor(Product_Category), y = Purchase)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribuzione spesa per prodotto - Città A",
       x = "Categoria prodotto", y = "Spesa") +
  theme_minimal()
#boxplot spesa media x categoria città B x categoria prodotto
df %>%
  filter(City_Category == "B") %>%
  ggplot(aes(x = factor(Product_Category), y = Purchase)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribuzione spesa per prodotto - Città B",
       x = "Categoria prodotto", y = "Spesa") +
  theme_minimal()
#boxplot spesa media x categoria città C x categoria prodotto
df %>%
  filter(City_Category == "C") %>%
  ggplot(aes(x = factor(Product_Category), y = Purchase)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribuzione spesa per prodotto - Città C",
       x = "Categoria prodotto", y = "Spesa") +
  theme_minimal()

#boxplot spesa media x fascia d'età x genere
df %>%
  ggplot(aes(x = factor(Age), y = Purchase, fill = Gender)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    title = "Distribuzione spesa per fascia d'età e genere",
    x = "Fascia d'età",
    y = "Spesa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#creo tabelle di frequenza
freq_tables <- lapply(categorical_cols, function(col) {
  df %>%
    group_by(.data[[col]]) %>%
    summarise(Frequenza = n()) %>%
    arrange(desc(Frequenza)) %>%
    rename(Valore = all_of(col))
})

#istogramma frequenza x variabili categoriali
for (col in categorical_cols) {
  plot <- df %>%
    count(.data[[col]]) %>%
    ggplot(aes(x = reorder(.data[[col]], n), y = n, fill = .data[[col]])) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Frequenza di", col), x = col, y = "Frequenza") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))
  
  print(plot)
}

#densità purchase
ggplot(df, aes(x = Purchase)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Distribuzione della spesa",
       x = "Spesa",
       y = "Densità") +
  theme_minimal()

