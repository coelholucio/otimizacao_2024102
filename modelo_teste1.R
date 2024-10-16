library(dplyr)
library(broom)
library(car)
library(tidyr)
library(caret)
library(data.table)

options(scipen=999)

#df_ini = fread("C:/Flavio/Projeto Otimizacao/Modelo Renovacao/df_mod_mp_feb23may24_v2.csv",sep = "|", showProgress = TRUE)
#df_ini = fread("C:/Flavio/Projeto Otimizacao/Modelo Renovacao/df_mod_mp_feb23may24_v2 - treino V.csv",sep = "|", showProgress = TRUE)
df_ini = fread("C:/Flavio/Projeto Otimizacao/Modelo Renovacao/df_mod_mp_feb23may24_v3.csv",sep = "|", showProgress = TRUE)



#teste = df_ini  %>%  dplyr::filter((!UF=="SP" | !Grupo_veiculo_new %in% c("Toyota Hilux CD","Toyota Hilux CS","Toyota Hilux SW4"))) %>% select(UF,Grupo_veiculo_new)

glimpse(df_ini)
colnames(df_ini2)

filtro =  df_ini  %>% 
  # dplyr::filter(teste_validacao=="Trein") %>% 
  # dplyr::filter(!anomes_dt_ter %in% c("202310","202309","202308","202302","202402")) %>% 
  dplyr::filter(!is.na(FABRICA)) %>%
  dplyr::filter(!is.na(MD_est_civil)) %>% 
  dplyr::filter(!is.na(per_com)) %>% 
  dplyr::filter(!is.na(peso)) %>% 
  dplyr::filter(!is.na(Renovacao_ant)) %>% 
  # dplyr::filter(!premio_total== 0) %>% 
  # dplyr::filter(premio_creserva>=0) %>% 
  # dplyr::filter(vigencia =="Anual") 
  
table(df_ini$MD_est_civil)
table(df_ini$MD_est_civil,useNA = 'always')

sum(is.na(df_ini$MD_est_civil))

  dplyr::filter(premio_ofertado/premio_total - 1 <=1.3 &  premio_ofertado/premio_total - 1 >=-0.3) %>% 
  dplyr::filter(premio_ofertado>100) %>% 
  dplyr::filter(is_casco_ofer>0) %>% 
  dplyr::filter(per_com > 0 &  per_com < 50) %>% 
  dplyr::filter((!UF=="SP" | !Grupo_veiculo_new %in% c("Toyota Hilux CD","Toyota Hilux CS","Toyota Hilux SW4"))) %>% 
  dplyr::filter(premio_creserva>=0) %>% 
  dplyr::filter(!premio_total== 0) %>% 
  ) 




df_ini2 =  df_ini  %>% 
    dplyr::mutate(
                # premio_ofertado=if_else(premio_ofertado>12000,12000,premio_ofertado),
                tx_risco_casco = round(risco_casco_modelo/is_casco_ofer,3),
                tx_premio_is = round(premio_ofertado/is_casco_ofer,3),
                risco_dmais_cob = 0.2627*1.4*premio_total,
                Act_cost = Risco_PP_mod+Risco_PT_mod+Risco_RF_mod+Risco_DM_mod+Risco_AS_mod+Risco_DC_mod+Risco_DMO_mod+risco_dmais_cob,
                premio_ic_100 = Act_cost/(1-per_com/100-0.2749),
                MD_tempo_casa = as.numeric(MD_tempo_casa_2)
  ) %>% 
  
  dplyr::filter(teste_validacao=="Trein") %>% 
  dplyr::filter(!anomes_dt_ter %in% c("202310","202309","202308","202302","202402")) %>% 
  dplyr::filter(premio_ofertado/premio_total - 1 <=1.3 &  premio_ofertado/premio_total - 1 >=-0.3) %>% 
  dplyr::filter(premio_ofertado>100) %>% 
  dplyr::filter(is_casco_ofer>0) %>% 
  dplyr::filter(per_com > 0 &  per_com < 50) %>% 
  dplyr::filter((!UF=="SP" | !Grupo_veiculo_new %in% c("Toyota Hilux CD","Toyota Hilux CS","Toyota Hilux SW4"))) %>% 
  dplyr::filter(premio_creserva>=0) %>% 
  dplyr::filter(!premio_total==  0) %>% 
  dplyr::filter(!is.na(FABRICA)) %>% 
  dplyr::filter(!is.na(MD_est_civil)) %>% 
  dplyr::filter(!is.na(per_com)) %>% 
  dplyr::filter(!is.na(peso)) %>% 
  dplyr::filter(!is.na(Renovacao_ant)) 

  
 
 


summary(df_ini2$premio_ofertado)

df_ini_select = df_ini2 %>% 
    dplyr::select(MD_sexo,cod_corr,Cod_End,MD_sin_5a_cas,MD_tempo_casa_2,
                  MD_idade_veic,MD_faixa_IScas,
                premio_ofertado, premio_total, anomes_dt_ter ,MD_idade ,
                per_com, RNS_Casco_calc ,MD_menor26 ,UF,ind_renovacao,
                Renovacao_ant,fx_ir_12m,is_casco_ofer,
                classe_bonus,MD_fator_ajuste, MD_tipo_renov,MD_capac,
                md_categoria,MD_comb,MD_qtd_assis_cpf,MD_tipo_renov, 
                idade_ult_RNS,canal,Act_cost,premio_ic_100,
                teste_validacao,tx_premio_is,MD_tempo_casa,
                MD_tipo_renov)


glimpse(df_ini)


table(df_ini$MD_tipo_renov)
# 
# 
# df_mod$fx_var_prem_prop_ant <- cut(df_mod$relative_core_premium,seq(-2,23,0.2))
# df_mod$fx_is_casco_ofer <- cut(df_mod$is_casco_ofer_num, seq(5000,80000,15900))
# df_mod$fx_per_com <- cut(df_mod$per_com_num, seq(0,35,5))
# df_mod$fx_tx_premio_is <- cut(df_mod$tx_premio_is, seq(0,1,0.1))
# df_mod$fx_tx_risco_casco <- cut(df_mod$tx_risco_casco, seq(0,1,0.1))
# 
# #separando em treino e teste
# set.seed(2024)
# split <- sort(sample(nrow(df_mod), nrow(df_mod)*0.7))
# 
# training <- df_mod[split,]
# testing <- df_mod[-split,]

training = df_ini_select %>% dplyr::filter(teste_validacao=="Trein")
testing  = df_ini_select %>% dplyr::filter(teste_validacao =="Test")


#categorizando variaveis---
dfmodel <- function(df){
  df = dplyr::mutate(df,
                     fx_ir_12m_bin = dplyr::case_when(
                       fx_ir_12m  == "01 - 00-<=020"  ~ "01 - 00-<=020" ,
                       fx_ir_12m  == "02 - 20-<=040"  ~ "02 - 20-<=040" ,
                       fx_ir_12m  == "03 - 40-<=060"  ~ "03 - 40-<=060" ,
                       fx_ir_12m  == "04 - 60-<=080"  ~ "04 - 60-<=080" ,
                       fx_ir_12m  == "05 - 80-<=100"  ~ "05 - 80-<=100" ,
                       is.na(fx_ir_12m )              ~ "03 - 40-<=060",
                       fx_ir_12m  == "NI"             ~ "02 - 20-<=040" ,
                       
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     fx_ir_12m_bin = relevel(factor(fx_ir_12m_bin),ref="02 - 20-<=040" ),
                     
                     
                     per_com_bin = dplyr::case_when(
                       per_com < 10    ~ "<10" ,
                       is.na(per_com)  ~ ">=10",
                       TRUE            ~ '>=10'                   
                     ),
                     
                     per_com_bin = relevel(factor(per_com_bin),ref=">=10"),
                     
                     
                     RNS_Casco_calc_bin = dplyr::case_when(
                       RNS_Casco_calc > 0   ~ ">0" ,
                       is.na(RNS_Casco_calc)  ~ "Demais",
                       TRUE ~ 'Demais'                   
                     ),
                     
                     RNS_Casco_calc_bin = relevel(factor(RNS_Casco_calc_bin),ref="Demais"),
                     
                     
                     
                     
                     MD_comb_bin = dplyr::case_when(
                       MD_comb == "Gasolina"    ~ "Gasolina" ,
                       is.na(MD_comb)  ~ "Demais",
                       TRUE ~ 'Demais'                   
                     ),
                     
                     MD_comb_bin = relevel(factor(MD_comb_bin),ref="Demais"),
                     
                     
                     canal_bin = dplyr::case_when(
                       canal == "Corretor Mais"   ~ "Corretor Mais" ,
                       is.na(canal)  ~ "Demais",
                       TRUE ~ 'Demais'                   
                     ),
                     
                     canal_bin = relevel(factor(canal_bin),ref="Demais"),
                     
                     
                     
                     idade_ult_RNS_bin = dplyr::case_when(
                       idade_ult_RNS == "Sem Sinistro"   ~ "Sem Sinistro" ,
                       idade_ult_RNS == "01 ano"   ~ "01 ano" ,
                       idade_ult_RNS == "02 ano"   ~ "02 ano +" ,
                       is.na(idade_ult_RNS)  ~ "Sem Sinistro",
                       TRUE ~ '02 ano +'                   
                     ),
                     
                     idade_ult_RNS_bin = relevel(factor(idade_ult_RNS_bin),ref="Sem Sinistro"),
                     
                     
                     
                     MD_qtd_assis_cpf_bin = dplyr::case_when(
                       MD_qtd_assis_cpf == 0    ~ "00" ,
                       MD_qtd_assis_cpf == 1    ~ "00" ,
                       MD_qtd_assis_cpf >= 2    ~ ">=2" ,
                       is.na(MD_qtd_assis_cpf)  ~ "0",
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     MD_qtd_assis_cpf_bin = relevel(factor(MD_qtd_assis_cpf_bin),ref="00"),
                     
                     
                     
                     MD_capac_bin = dplyr::case_when(
                       MD_capac == 5   ~ "5" ,
                       !MD_capac == 5   ~ "Demais" ,
                       is.na(MD_capac)  ~ "5",
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     MD_capac_bin = relevel(factor(MD_capac_bin),ref="5"),
                     
                     
                     
                     fator_ajuste_bin = dplyr::case_when(
                       MD_fator_ajuste < 101   ~ "<101" ,
                       MD_fator_ajuste >=101   ~ ">=101",
                       is.na(MD_fator_ajuste)  ~ "<101",
                       TRUE ~ 'ERROR'
                     ),
                     
                     fator_ajuste_bin = relevel(factor(fator_ajuste_bin),ref="<101"),
                     
                     
                     
                     
                     UF_bin = dplyr::case_when(
                       UF=="AL"  ~ "AL,CE,RS", 
                       UF=="CE"  ~ "AL,CE,RS", 
                       UF=="RS"  ~ "AL,CE,RS",
                       UF=="AP"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="DF"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="ES"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="PA"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="PB"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="RN"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="SC"  ~ "AP,DF,ES,PA,PB,RN,SC", 
                       UF=="GO"  ~ "GO,MA,MG", 
                       UF=="MA"  ~ "GO,MA,MG", 
                       UF=="MG"  ~ "GO,MA,MG",	
                       UF=="MT"  ~ "MT,PR",
                       UF=="PR"  ~ "MT,PR",	
                       UF=="RJ"  ~ "RJ",	
                       is.na(UF)   ~ "Demais",
                       TRUE ~ 'Demais'                   
                     ),
                     
                     UF_bin = relevel(factor(UF_bin),ref="Demais"),
                     
                                  
                     
                     Cod_End_bin = dplyr::case_when(
                       Cod_End == 13509   ~ "13509" ,
                       Cod_End == 50007   ~ "13509" ,
                       Cod_End == 50100   ~ "13509" ,
                       is.na(Cod_End) ~ "Demais",
                       TRUE ~ 'Demais'                   
                     ),
                     
                     Cod_End_bin = relevel(factor(Cod_End_bin),ref="Demais"),
                     
                     
                     classe_bonus_bin = dplyr::case_when(
                       classe_bonus ==0    ~ "00" ,
                       is.na(classe_bonus) ~ "01-10",
                       TRUE ~ '01-10'                   
                     ),
                     
                     classe_bonus_bin = relevel(factor(classe_bonus_bin),ref="01-10"),
                     
                     
                     # 
                     # classe_bonus_bin = dplyr::case_when(
                     #   classe_bonus ==0    ~ "00" ,
                     #   classe_bonus ==1    ~ "01" ,
                     #   classe_bonus ==2    ~ "02" ,
                     #   classe_bonus ==3    ~ "03" ,
                     #   classe_bonus ==4    ~ "04" ,
                     #   classe_bonus ==5    ~ "05" ,
                     #   classe_bonus ==6    ~ "06" ,
                     #   classe_bonus ==7    ~ "07" ,
                     #   classe_bonus ==8    ~ "08" ,
                     #   classe_bonus ==9    ~ "09" ,
                     #   classe_bonus ==10   ~ "10" ,
                     #   is.na(classe_bonus) ~ "10",
                     #   TRUE ~ 'ERROR'                   
                     # ),
                     # 
                     # classe_bonus_bin = relevel(factor(classe_bonus_bin),ref="10"),
                     
                     
                     classe_bonus2_bin = dplyr::case_when(
                       classe_bonus ==0    ~ "00" ,
                       classe_bonus ==1    ~ "01-08" ,
                       classe_bonus ==2    ~ "01-08" ,
                       classe_bonus ==3    ~ "01-08" ,
                       classe_bonus ==4    ~ "01-08" ,
                       classe_bonus ==5    ~ "01-08" ,
                       classe_bonus ==6    ~ "01-08" ,
                       classe_bonus ==7    ~ "01-08" ,
                       classe_bonus ==8    ~ "01-08" ,
                       classe_bonus ==9    ~ "09-10" ,
                       classe_bonus ==10   ~ "09-10" ,
                       is.na(classe_bonus) ~ "09-10",
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     classe_bonus2_bin = relevel(factor(classe_bonus2_bin),ref="09-10"),
                     
                     
                     tempo_casa_bin = dplyr::case_when(
                       MD_tempo_casa_2 ==1    ~ "01" ,
                       MD_tempo_casa_2 ==2    ~ "02" ,
                       MD_tempo_casa_2 ==3    ~ "03" ,
                       MD_tempo_casa_2 ==4    ~ "04" ,
                       MD_tempo_casa_2 >=5    ~ "05+" ,
                       
                       is.na(MD_tempo_casa_2) ~ "01",
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     tempo_casa_bin = relevel(factor(tempo_casa_bin),ref="01"),
                     
                     
                     
                     
                     # tempo_casa_bin = dplyr::case_when(
                     #   MD_tempo_casa_2 ==1    ~ "01" ,
                     #   MD_tempo_casa_2 ==2    ~ "02" ,
                     #   MD_tempo_casa_2 ==3    ~ "03" ,
                     #   MD_tempo_casa_2 ==4    ~ "04" ,
                     #   MD_tempo_casa_2 ==5    ~ "05" ,
                     #   MD_tempo_casa_2 ==6    ~ "06" ,
                     #   MD_tempo_casa_2 ==7    ~ "07" ,
                     #   MD_tempo_casa_2 ==8    ~ "08" ,
                     #   MD_tempo_casa_2 ==9    ~ "09" ,
                     #   MD_tempo_casa_2 >=10   ~ ">=10" ,
                     #   is.na(MD_tempo_casa_2) ~ "01",
                     #   TRUE ~ 'ERROR'                   
                     # ),
                     # 
                     # tempo_casa_bin = relevel(factor(tempo_casa_bin),ref="01"),
                     # 
                     
                     tempo_casa2_bin = dplyr::case_when(
                       MD_tempo_casa_2 ==1    ~ "01" ,
                       MD_tempo_casa_2 ==2    ~ "02-03" ,
                       MD_tempo_casa_2 ==3    ~ "02-03" ,
                       MD_tempo_casa_2 ==4    ~ "04-08" ,
                       MD_tempo_casa_2 ==5    ~ "04-08" ,
                       MD_tempo_casa_2 ==6    ~ "04-08" ,
                       MD_tempo_casa_2 ==7    ~ "04-08" ,
                       MD_tempo_casa_2 ==8    ~ "04-08" ,
                       MD_tempo_casa_2 ==9    ~ "09" ,
                       MD_tempo_casa_2 >=10   ~ ">=10" ,
                       is.na(MD_tempo_casa_2) ~ "10",
                       TRUE ~ 'ERROR'                   
                     ),
                     
                     tempo_casa2_bin = relevel(factor(tempo_casa2_bin),ref="01"),
                     
                     
                     
                     tipo_renov_bin = dplyr::case_when(
                       Renovacao_ant =="1 - Seguro Novo"                     ~ "1-SeguroNovo" ,
                       Renovacao_ant =="2 - Renov MAPFRE sem sinistro"       ~ "2-RenovPropria" ,
                       Renovacao_ant =="3 - Renov MAPFRE com sinistro"       ~ "2-RenovPropria" ,
                       Renovacao_ant =="4 - Renov Congenere sem sinistro"    ~ "3-RenovCongenere" ,
                       Renovacao_ant =="5 - Renov Congenere com sinistro"    ~ "3-RenovCongenere" ,
                       Renovacao_ant =="6 - Renov BB sem sinistro"           ~ "3-RenovCongenere" ,
                       Renovacao_ant =="7 - Renov BB com sinistro"           ~ "3-RenovCongenere" ,
                       Renovacao_ant ==" "                                   ~ "2-RenovPropria" ,
                       is.na(Renovacao_ant)                                  ~ "2-RenovPropria" ,
                       TRUE                                                  ~ '2-RenovPropria'                   
                     ),
                     
                     tipo_renov_bin = relevel(factor(tipo_renov_bin),ref="2-RenovPropria"),
                     
                     
                     tipo_renov2_bin = dplyr::case_when(
                       Renovacao_ant =="1 - Seguro Novo"                     ~ "1-SeguroNovo",
                       is.na(Renovacao_ant)                                   ~ "Demais" ,
                       TRUE ~ 'Demais'                   
                     ),
                     
                     tipo_renov2_bin = relevel(factor(tipo_renov2_bin),ref="Demais"),
                     
  )
}

df_training = dfmodel(training)
df_testing = dfmodel(testing)

table(df_ini_select$teste_validacao)



#model----
summary (model0 <- glm(ind_renovacao ~ 1
                         # log(premio_ofertado)
                       , 
                       data = df_training, 
                       family = binomial) )



summary (model0 <- glm(ind_renovacao ~ 
                         log(premio_ofertado) +
                         tx_premio_is +
                         log((premio_ofertado/premio_ic_100)) +
                         log((premio_ofertado/premio_total-1)+1) +
                         canal_bin+
                         Cod_End_bin+
                         fator_ajuste_bin+
                         per_com +
                         per_com_bin +
                         classe_bonus_bin+
                         log(MD_tempo_casa_2+1) +
                         idade_ult_RNS_bin+
                         UF_bin+
                         MD_comb_bin+
                         tipo_renov_bin+
                         fx_ir_12m_bin +
                         # RNS_Casco_calc_bin+
                         MD_qtd_assis_cpf_bin
                       
                       , 
                       data = df_training, 
                       family = binomial) )

#escorar----
df_training <- df_training %>% 
  dplyr::mutate(prob_est= predict(model0, type = "response" ,newdata=df_training),
                ind_renovacao_est=ifelse(prob_est>0.5,1,0))

df_testing <- df_testing %>% 
  dplyr::mutate(prob_est = predict(model0, type = "response" ,newdata=df_testing),
                ind_renovacao_est=ifelse(prob_est>0.5,1,0))


summary(df_training$ind_renovacao)
summary(df_training$prob_est)


#ler lote----
df_lote = fread("C:/Flavio/Projeto Otimizacao/Otimizacao/base_case_otim_082024.csv",sep = ";", showProgress = TRUE)
df_lote = df_lote %>% mutate(MD_capac=as.numeric(MD_capacidade),
                             Cod_End =cod_end,
                             classe_bonus=Classe_bonus,
                             premio_ofertado=Premio_oferecido_com_batente,
                             TARIFA=Premio_oferecido_com_batente,
                             tx_premio_is = round(premio_ofertado/is_casco_ofer,3),
                             premio_total=Pr_anterior_TI,
                             premio_ic_100=Premio_IC_100 )


df_lote = dfmodel(df_lote)


df_lote <- df_lote %>% 
  dplyr::mutate(prob_est= predict(model0, type = "response" ,newdata=df_lote))

summary(df_lote$prob_est)
summary(df_lote$`Renewal Demand_bc`)



data_teste = df_lote %>% filter(chave=="1002542863100001")

fatores = seq(1.5, 0.6, by=-0.1)
lista_df=list()

data2 <- df_lote %>% select(chave,premio_ofertado)

for(i in seq_along(fatores)) {   
  fator=fatores[i]
  
  data2_mult = data2*fator
  lista_df[[i]]=data2_mult
}

df <- do.call("rbind",lista_df)
colnames(df)<-"premio_cart"




ver <- df_lote %>% select(prob_est,`Renewal Demand_bc`)

summary(ver)


logistic = function(p, c, alpha, p0) c/(1+exp(-alpha*(p-p0)))

# Objective functions for optimization
demand_objective = function(par, p, d) sum((d - logistic(p, par[1], par[2], par[3]))^2)
price_objective = function(p, alpha, c, p0) (exp(-alpha*(p-p0))*(alpha*(p-c)+1) + 1)^2 

# A cleaner alternative for pricing optimization is to min:
price_objective2 = function(p, c, alpha, C, p0) -logistic(p, C, alpha, p0)*(p-c)

# synthetic data
p = seq(80,130)
c = 75
d = logistic(p, 120, -.15, 115) + rnorm(sd = 10, length(p))
profit = d*(p-c)

# Demand fitting, we can't use lm anymore
par.start = c(max(d), 0, mean(d)) # initial guess

demand_fit = optim(par = par.start, 
                   fn = demand_objective, 
                   method = 'BFGS',
                   p = p, 
                   d = d)

par = demand_fit$par # estimated parameters for demand function
demand.fitted = logistic(p, c = par[1], alpha = par[2], p0 = par[3])
profit.fitted = demand.fitted*(p - c)

# Pricing Optimization, we don't have a closed expression anymore
price_fit = optim(mean(p), price_objective, method = 'BFGS',
                  alpha = par[2], c = c, p0 = par[3])

# or

price_fit2 = optim(mean(p), price_objective2, method = 'BFGS',
                   c = c, C = par[1], alpha = par[2], p0 = par[3]) 

# both results are almost identical
p.max.profit = price_fit$par
p.max.profit
p.max.profit2 = price_fit2$par
p.max.profit2

# Graphics
df.logistic = data.frame('Prices' = p, 'Demand' = d, 'Demand.fitted' = demand.fitted,
                         'Profit.fitted' = profit.fitted, 'Profit' = profit)

ggplot(select(df.logistic, Prices, Demand)) + aes(x = Prices, y = Demand) +
  geom_point() +
  geom_line(data = df.logistic, aes(x = Prices, y = Demand.fitted), color = 'blue')

ggplot(select(df.logistic, Prices, Profit)) + aes(x = Prices, y = Profit) +
  geom_line() + geom_vline(xintercept = p.max.profit, lty = 2) +
  geom_line(data = df.logistic, aes(x = Prices, y = Profit.fitted), color = 'blue')



