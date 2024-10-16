#############################
#função do modelo de demanda
#Autor Flavio Coelho
#############################
def demand_model(df):
    import pandas as pd
    import numpy as np 
    P   = df['premio']
    X2  = np.array(df['GBM_mercado'])
    X3  = np.array(df['Premio_IC_100'])
    X4  = np.array(df['premio_anterior'])
    X5  = np.array(df['MD_tempo_casa_2'])
    X6  = np.array(df['per_com'])
    X7  = np.array(df['per_com_bin_p_1_10'])
    X1  = np.array(df['is_casco_ofer'])
    X8  = np.array(df['canal_bin_Corretor_Mais'])
    X9  = np.array(df['Cod_End_bin_13509'])
    X10 = np.array(df['fator_ajuste_bin_f_101_120'])
    X11 = np.array(df['classe_bonus_bin_00'])
    X12 = np.array(df['UF_bin_AL_CE_RS'])
    X13 = np.array(df['UF_bin_AP_DF_ES_PA_PB_RN_SC'])
    X14 = np.array(df['UF_bin_GO_MA_MG'])
    X15 = np.array(df['UF_bin_MT_PR'])
    X16 = np.array(df['UF_bin_RJ'])
    X17 = np.array(df['MD_comb_bin_Gasolina'])
    X18 = np.array(df['md_tipo_renov_ant_bin_1_SeguroNovo'])
    X19 = np.array(df['md_tipo_renov_ant_bin_3_RenovCongenere'])
    X20 = np.array(df['fx_ir_12m_bin_01_00_20'])
    X21 = np.array(df['fx_ir_12m_bin_03_40_60'])
    X22 = np.array(df['fx_ir_12m_bin_04_60_80'])
    X23 = np.array(df['fx_ir_12m_bin_05_80_100'])
    X24 = np.array(df['MD_tipo_renov_bin_3_RenovPropria_com_sin'])
    X25 = np.array(df['idade_ult_RNS_bin_01_05_anos'])
    X26 = np.array(df['idade_ult_RNS_bin_06_inf'])

    def demanda(P,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26):
        D=np.exp(1.3356 -0.2576 * np.log(P)  + 1.3647 * np.divide(P, X1)-0.8782 * np.log(np.divide(P, X2))-0.795 * np.log(np.divide(P, X3))-1.6731 * np.log((np.divide(P, X4)))  + 0.3797 * np.log(X5 + 1)  + 0.0011 * X6  + 0.1253 * X7  + 0.1334 * X8-0.1382 * X9  + 0.3919 * X10-0.1397 * X11-0.2305 * X12-0.2832 * X13  + 0.0024 * X14-0.1476 * X15  + 0.2817 * X16  + 0.3336 * X17-0.4115 * X18  + 0.0127 * X19-0.73 * X20  + 0.4343 * X21  + 1.1066 * X22  + 1.9561 * X23-0.0742 * X24 -0.84590*X25- 0.7094 * X26)
        return D/(1+D)


    df['Renewal_Demand'] = demanda(P, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26)
    
    return df