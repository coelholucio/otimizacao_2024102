#############################################
#Função para gerar o relatório de renovação 
#Autor: Flavio Coelho
#############################################

def relatorio(df):
    import pandas as pd
    import numpy as np 
    df["per_com_pct"] = df['per_com']/100
    df["premio_prob"] = df["Renewal_Demand"]*df["premio"]
    df["premio_anterior_prob"] = df["Renewal_Demand"]*df["premio_anterior"]
    df["Act_cost_prob"] = df["Renewal_Demand"]*df["Act_cost"]                                                                
    df["comissao_valor"] = df["per_com_pct"] *df["premio"]
    df["comissao_valor_prob"] = df["per_com_pct"] *df["premio_prob"]



    apolice_ofertada    = np.round(df.shape[0],0)
    apolice_estimada    = np.round(np.sum(df["Renewal_Demand"]),0)
    premio_ofertado     = np.mean(df["premio"])
    retencao_estimada   = np.mean(df["Renewal_Demand"])
    premio_esperado     = np.sum(df["premio_prob"])/np.sum(df["Renewal_Demand"])
    risco_ofertado      = np.mean(df["Act_cost"])
    risco_esperado      = np.sum(df["Act_cost_prob"])/np.sum(df["Renewal_Demand"])
    sp_ofertado         = np.sum(df["Act_cost"])/np.sum(df["premio"])
    sp_esperado         = np.sum(df["Act_cost_prob"])/np.sum(df["premio_prob"])
    comis_ofertado      = np.sum(df["comissao_valor"])/np.sum(df["premio"])
    comis_esperado      = np.sum(df["comissao_valor_prob"])/np.sum(df["premio_prob"])
    premio_anterior     = np.mean(df["premio_anterior"])
    premio_anterior_w   = np.sum(df["premio_anterior_prob"])/np.sum(df["Renewal_Demand"])
    ic_ofertado       =  sp_ofertado  +  comis_ofertado + 0.2749
    ic_esperado       =  sp_esperado  +  comis_esperado + 0.2749
    premio_anterior   = np.mean(df_lote["premio_anterior"])
    premio_anterior_w   = np.sum(df_lote["premio_anterior_prob"])/np.sum(df_lote["Renewal_Demand"])


    tabela = pd.DataFrame([apolice_ofertada,apolice_estimada,
                            retencao_estimada,
                            premio_ofertado,premio_esperado,
                            premio_anterior,premio_anterior_w,
                            risco_ofertado,risco_esperado,
                            comis_ofertado, comis_esperado,
                            sp_ofertado,sp_esperado,
                            ic_ofertado,ic_esperado      
                            ])


    tabela = tabela.rename(columns = {0 : 'Total'})

    tabela.index = ['Apolices Ofertadas:', 
                    'Apolices Renovadas:',
                    '%Retenção:',
                    'Premio Ofertado:',
                    'Premio Renovado:',  
                    'Premio Anterior Ofertado:',
                    'Premio Anterior Renovado:',
                    'Risco Ofertado', 
                    'Risco Renovado', 
                    '%Comissão Ofertada:',
                    '%Comissão Renovada:',
                    '%SP Ofertada:', 
                    '%SP Renovada:',
                    '%IC Ofertado:', 
                    '%IC Renovado:']

    return tabela