library(readr)
library(dplyr)
library(lightgbm)
library(caret)
library(fastDummies)


df_ini = data.table::fread("C:/Flavio/Projeto Otimizacao/Modelo Renovacao/df_mod_mp_feb23may24_v2 - treino V.csv",sep = "|", showProgress = TRUE)

glimpse(df_ini)
df_mod_glm = df_ini  %>% select (c("premio_ofertado","ano_mod","classe_bonus",
                                   "MD_idade_novo","is_casco","is_dmoral","is_rcdc","is_rcdm",
                                   "RNS_Casco_calc",
                                   "Grupo_veiculo_new", "FABRICA","idade_ult_RNS","md_categoria",
                                   "MD_comb","desc_config","MD_franquia","MD_menor26","MD_sexo",
                                   "MD_Subzona","MD_tipo_renov","MD_uso_veic","UF"
) ) %>% rename(fabrica =FABRICA,
               idade = MD_idade_novo,
               md_config=desc_config,
               md_menor26=MD_menor26,
               md_sexo=MD_sexo,
               md_tipo_renov=MD_tipo_renov,
               uf=UF)

rm(df_mercado)
rm(df_ini)
gc()

df_mod_glm_dummy=df_mod_glm %>% 
  dummyVars(~.-premio_ofertado,data=., sep="_") %>% 
  predict(newdata=df_mod_glm) %>% 
  as.data.frame()%>% 
  mutate(premio_ofertado=df_mod_glm$premio_ofertado
  )




mercado = data.table::fread("C:/Flavio/Projeto Otimizacao/Modelo Mercado/data_consolidado_final_jun24.csv",sep=';', showProgress = TRUE)

df_mercado = mercado  %>% select (c("premio_total_liq_cap","ano_mod_conv","classe_bonus",
                                    "idade","is_casco","is_dmoral","is_rcdc","is_rcdm",
                                    "RNS_Casco_calc",
                                    "Grupo_veiculo_new", "fabrica","idade_ult_RNS","md_categoria",
                                    "MD_comb","md_config","MD_franquia","md_menor26","md_sexo",
                                    "MD_Subzona","md_tipo_renov","MD_uso_veic","uf"
) ) %>% rename(premio_ofertado=premio_total_liq_cap ,
               ano_mod=ano_mod_conv )



rm(mercado)
gc()
# 
# features = names(df_mercado)
# 
# for (f in features) {
#   if (class(df_mercado[[f]])=="character") {
#     levels <- unique(df_mercado[[f]])
#     df_mercado[[f]] <- as.integer(factor(df_mercado[[f]], levels=levels))
#   }
# }


df_mercado_dummy=df_mercado %>% 
  dummyVars(~.-premio_ofertado,data=., sep="_") %>% 
  predict(newdata=df_mercado) %>% 
  as.data.frame()%>% 
  mutate(premio_ofertado=df_mercado$premio_ofertado
         )


rm(df_mercado)
gc()

glimpse(df_mercado_dummy)

set.seed(0) # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(df_mercado_dummy$premio_ofertado, p = .8, list = F)
train = df_mercado_dummy[parts, ]
test = df_mercado_dummy[-parts, ]

dim(train)
colnames(train)


x_train <- train[,1:535]
x_test <- test[,1:535]

y_train <- train[,536]
y_test <- test[,536]

# lightgbm comes with an own sparse matrix format
lgbm_df = lgb.Dataset(data=as.matrix(x_train), label=y_train)

rm(lgbm_df)

gc()


lgb_mod = lightgbm(boosting_type = 'gbdt', 
                 objective = "regression", 
                 metric = 'mae', 
                 lgbm_df, 
                 nrounds = 500) 


yhat_fit_base <- predict(lgb_mod, data = as.matrix(x_train))
summary(yhat_fit_base)


 
GBM_mercado <- predict(lgb_mod, data = as.matrix(df_mod_glm))
