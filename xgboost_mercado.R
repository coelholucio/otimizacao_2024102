library(xgboost)

mercado = data.table::fread("C:/Flavio/Projeto Otimizacao/Modelo Mercado/data_consolidado_final_jun24.csv",sep=';', showProgress = TRUE)

df_mercado = mercado  %>% select (c("premio_total_liq_cap","ano_mod_conv","classe_bonus",
                                    "idade","is_casco","is_dmoral","is_rcdc","is_rcdm",
                                    "RNS_Casco_calc",
                                    "Grupo_veiculo_new", "fabrica","idade_ult_RNS","md_categoria",
                                    "MD_comb","md_config","MD_franquia","md_menor26","md_sexo",
                                    "MD_Subzona","md_tipo_renov","MD_uso_veic","uf"
) )

df_mercado2 = df_mercado  


features = names(df_mercado2)

for (f in features) {
  if (class(df_mercado2[[f]])=="character") {
    levels <- unique(df_mercado2[[f]])
    df_mercado2[[f]] <- as.integer(factor(df_mercado2[[f]], levels=levels))
  }
}


set.seed(0) # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(df_mercado2$premio_total_liq_cap, p = .8, list = F)
train = df_mercado2[parts, ]
test = df_mercado2[-parts, ]

xgtrain = xgb.DMatrix(data = as.matrix(train[,-1]), 
                       label = train$premio_total_liq_cap )

xgtest = xgb.DMatrix(data = as.matrix(test[,-1]), 
                      label = test$premio_total_liq_cap )

#defining a watchlist
watchlist = list(train=xgtrain, test=xgtest)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgtrain, max.depth = 3, watchlist=watchlist, nrounds = 500)


#define final model
model_xgboost = xgboost(data = xgtrain, max.depth = 3, nrounds = 500, verbose = 0)

summary(model_xgboost)

#use model to make predictions on test data
pred_y = predict(model_xgboost, xgtest)

teste = pred_y %>%  as.data.frame()
names(teste) ="test_y"
# performance metrics on the test data
test_y = test$premio_total_liq_cap
msd= data.frame(test$premio_total_liq_cap,teste$test_y)

mean((test_y - pred_y)^2) #mse - Mean Squared Error

caret::RMSE(test_y, pred_y) #rmse - Root Mean Squared Error

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

x = 1:length(test_y)                   # visualize the model, actual and predicted data
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))



