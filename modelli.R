#Riccardo Baratto
#librerie
library(SHAPforxgboost)
library(tidyverse)
library(caret)
library(randomForest)
library(gbm)
library(earth)
library(ipred)
library(lme4)
library(nlme)
library(xgboost)
library(glmnet)
library(knitr)
library(data.table)

#caricamento dati
df = read.csv(file.choose())
#trasformazione dati
df_model <- df %>%
  group_by(User_ID, Product_Category, Gender, Age, Occupation,
           City_Category, Stay_In_Current_City_Years, Marital_Status) %>%
  summarise(Spesa_Media = mean(Purchase, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    across(where(is.character), as.factor),
    Marital_Status = as.factor(Marital_Status),
    Occupation = as.factor(Occupation),
    Product_Category = as.factor(Product_Category),
    Stay_In_Current_City_Years = as.factor(Stay_In_Current_City_Years),
    Age = factor(Age, levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"))
  )

set.seed(123)

#elimino ID prodotto
df_model=df_model[,-1]
#creo insieme di stima e di verifica
train_index <- createDataPartition(df_model$Spesa_Media, p = 0.8, list = FALSE)
train_data <- df_model[train_index, ]
test_data <- df_model[-train_index, ]
#funzione per l'RMSE
rmse <- function(true, pred) sqrt(mean((true - pred)^2))

#modello lineare
mod_lm <- lm(Spesa_Media ~ ., data = train_data)
#stepwise
mod_step <- step(mod_lm, direction = "both", trace = FALSE)
pred_step <- predict(mod_step, test_data)
rmse_step <- rmse(test_data$Spesa_Media, pred_step)
#riepilogo
summary(mod_step)

#regressione ridge
train_matrix <- model.matrix(Spesa_Media ~ . -1, data = train_data)
test_matrix <- model.matrix(Spesa_Media ~ . -1, data = test_data)
mod_ridge <- cv.glmnet(train_matrix, train_data$Spesa_Media, alpha = 0)
pred_ridge <- predict(mod_ridge, s = "lambda.min", newx = test_matrix)
rmse_ridge <- rmse(test_data$Spesa_Media, pred_ridge)

#modello mars
mod_mars <- earth(Spesa_Media ~ ., data = train_data)
pred_mars <- predict(mod_mars, test_data)
rmse_mars <- rmse(test_data$Spesa_Media, pred_mars)

#random forest
mod_rf <- randomForest(Spesa_Media ~ ., data = train_data, ntree = 20, importance=TRUE)
pred_rf <- predict(mod_rf, test_data)
rmse_rf <- rmse(test_data$Spesa_Media, pred_rf)
varImpPlot(mod_rf, type = 1, main = "Importanza delle variabili (Random Forest)")
#calcolo importanza 
var_imp <- importance(mod_rf, type = 1)
df_imp <- data.frame(
  Variable = rownames(var_imp),
  IncMSE = var_imp[, 1]
) %>%
  mutate(Influence_Pct = 100 * IncMSE / sum(IncMSE)) %>%
  arrange(Influence_Pct)

#grafico importanza
ggplot(df_imp, aes(x = reorder(Variable, Influence_Pct), y = Influence_Pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2f%%", Influence_Pct)), 
            hjust = -0.1, size = 3.5, color = "darkred") +
  labs(
    title = "Influenza Relativa delle Variabili (Random Forest)",
    x = "",
    y = "Contributo Percentuale (%)"
  ) +
  theme_minimal(base_size = 13) +
  expand_limits(y = max(df_imp$Influence_Pct) * 1.15)

#boosting 
mod_gbm <- gbm(Spesa_Media ~ ., data = train_data, distribution = "gaussian",
               n.trees = 15, interaction.depth = 3, shrinkage = 0.1, cv.folds = 10)
pred_gbm <- predict(mod_gbm, test_data, n.trees = 10)
rmse_gbm <- rmse(test_data$Spesa_Media, pred_gbm)

#bagging
mod_bagging <- bagging(Spesa_Media ~ ., data = train_data, nbagg = 25)
pred_bagging <- predict(mod_bagging, test_data)
rmse_bagging <- rmse(test_data$Spesa_Media, pred_bagging)

#xgboost
dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$Spesa_Media)
dtest <- xgb.DMatrix(data = test_matrix, label = test_data$Spesa_Media)
param <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)
watchlist <- list(train = dtrain, test = dtest)
mod_xgb <- xgb.train(
  params = param,
  data = dtrain,
  nrounds = 100,
  watchlist = watchlist,
  verbose = 0
)
pred_xgb <- predict(mod_xgb, dtest)
rmse_xgb <- rmse(test_data$Spesa_Media, pred_xgb)

#influenza relativa variabili
imp <- xgb.importance(
  model = mod_xgb,
  feature_names = colnames(train_matrix),
)
imp <- imp %>%
  mutate(
    Influence_Pct = Gain / sum(Gain) * 100
  ) %>%
  arrange(desc(Influence_Pct))
imp_plot <- imp %>%
  mutate(Feature = as.factor(Feature)) %>%
  mutate(Feature = fct_reorder(Feature, Influence_Pct))
setDT(imp_plot)
imp_plot[, Variable := fifelse(grepl("^Age", Feature), "Age",
                         fifelse(grepl("^Occupation", Feature), "Occupation",
                                 fifelse(grepl("^Product_Category", Feature), "Product_Category",
                                         fifelse(grepl("^City_Category", Feature), "City_Category",
                                                 fifelse(grepl("^Stay_In_Current_City_Years", Feature), "Stay_In_Current_City_Years",
                                                         fifelse(grepl("^Gender", Feature), "Gender",
                                                                 fifelse(grepl("^Marital_Status", Feature), "Marital_Status", as.character(Feature))))))))]
dt_aggregata <- imp_plot[, .(Influence_Pct = sum(Influence_Pct)), by = Variable][order(-Influence_Pct)]

ggplot(dt_aggregata, aes(x = reorder(Variable, Influence_Pct), y = Influence_Pct)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2f%%", Influence_Pct)), 
            hjust = -0.1, size = 3.5, color = "darkred") +
  labs(title = "Influenza Relativa delle Variabili",
       x = "",
       y = "Contributo Percentuale (%)") +
  theme_minimal() +
  expand_limits(y = c(0, max(dt_aggregata$Influence_Pct) * 1.15))

#RMSE train/test per numero di alberi
eval_log <- as.data.frame(mod_xgb$evaluation_log)
ggplot(eval_log, aes(x = iter)) +
  geom_line(aes(y = train_rmse, color = "Train")) +
  geom_line(aes(y = test_rmse, color = "Test")) +
  labs(title = "Errore di stima vs Numero interazioni (alberi)",
       x = "Numero alberi (interazioni)",
       y = "RMSE",
       color = "") +
  theme_minimal(base_size = 13)

#confronto rmse
results <- tibble(
  Modello = c("Stepwise", "Ridge", "MARS", "Random Forest", "Boosting", "Bagging", "XGBoost"),
  RMSE = c(rmse_step,rmse_ridge, rmse_mars, rmse_rf, rmse_gbm, rmse_bagging, rmse_xgb)
)
results %>%
  ggplot(aes(x = reorder(Modello, RMSE), y = RMSE, fill = Modello)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Confronto RMSE tra modelli predittivi",
       x = "Modello", y = "RMSE") +
  theme_minimal()

