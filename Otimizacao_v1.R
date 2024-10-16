

# library 

library(dplyr)
library(nloptr)

# Criando um DataFrame de exemplo
dados <- data.frame(
  Cliente = c(1, 2, 3, 4, 5),
  Preco_Base = c(2000, 2200, 1800, 2100, 2300),
  Custo_Base = c(1200, 1300, 1100, 1250, 1350),
  Comprou = c(1, 0, 1, 0, 1)
)

# Definindo parâmetros
elasticidade_preco = -0.5  
# Ajuste conforme necessário 
probabilidade_target = 0.8 
npv_target = 10000

# Função de elasticidade preço e demanda 
elasticidade_demanda <- function(preco, elasticidade) {
  return(exp(elasticidade * log(preco))) }

# Função objetivo para otimizar os preços 
funcao_objetivo <- function(precos, custos, elasticidade, probabilidade_target, npv_target) {
  demanda_esperada <- elasticidade_demanda(precos, elasticidade)
  probabilidade_esperada <- pmin(1, demanda_esperada)  # Garante que a probabilidade não ultrapasse 1
  npv <- probabilidade_esperada * (precos - custos)
  penalizacao_probabilidade <- (mean(probabilidade_esperada) - probabilidade_target)^2
  penalizacao_npv <- (mean(npv) - npv_target)^2
  return(-mean(npv) - 10 * penalizacao_probabilidade - 10 * penalizacao_npv)
}

# Função de restrição para garantir que os preços otimizados estejam dentro dos limites 
restricao_limites <- function(precos, dados) {
  c(0.7 * dados$Preco_Base - precos, precos - 2 * dados$Preco_Base) }

# Chute inicial para os preços (preço base) 
chute_inicial <- dados$Preco_Base

# Otimizando os preços individualmente
resultado_otimizacao <- nloptr::nloptr(
  x0 = chute_inicial,
  eval_f = function(precos) funcao_objetivo(precos, 
                                            dados$Custo_Base,
                                            elasticidade_preco, 
                                            probabilidade_target, 
                                            npv_target),
  lb = 0.7 * dados$Preco_Base,
  ub = 2 * dados$Preco_Base,
  eval_g_ineq = function(precos) restricao_limites(precos, dados),
  opts = list("algorithm" = "NLOPT_LN_COBYLA")
)

# Adicionando resultados ao DataFrame
dados$Preco_Otimizado <- resultado_otimizacao$solution 
dados$Demanda_Otimizada <- elasticidade_demanda(dados$Preco_Otimizado, elasticidade_preco)
dados$Probabilidade_Otimizada <- pmin(1, dados$Demanda_Otimizada)
dados$NPV_Otimizado <- dados$Probabilidade_Otimizada * (dados$Preco_Otimizado - dados$Custo_Base)

# Exibindo os resultados finais
print("\nResultados Finais:")
print(dados)