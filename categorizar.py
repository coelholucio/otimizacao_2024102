#função para categorizar as variaveis conforme necessidade nos modelos
#Autor Flavio Coelho

def categorizar(df):
    
    df['classe_bonus_bin'] = df['classe_bonus'].apply(lambda x: '00'  if x == 0 else 'Demais')   
    
    df['MD_sexo_bin'] = df['MD_sexo'].map({'Masculino': 'Masculino', 
                                 'Feminino': 'Feminino', 
                                  'ERRO': 'Masculino'}).astype(object)
    
    df['MD_sexo_2bin'] = df['MD_sexo'].map({'Masculino': 1, 
                                 'Feminino': 0, 
                                  'ERRO': 1}).astype(object)


    df['fx_ir_12m_bin'] = df['fx_ir_12m'].apply(lambda x: '01_00_20' if x == '01 - 00-<=020' else
                                                          '02_20_40' if x == '02 - 20-<=040' else
                                                          '03_40_60' if x == '03 - 40-<=060' else
                                                          '04_60_80' if x == '04 - 60-<=080' else
                                                          '05_80_100' if x == '05 - 80-<=100' else
                                                          '02_20_40' if x == 'NI'           else 'ERROR')

  
    df['MD_comb_bin'] = df['MD_comb'].apply(lambda x: 'Gasolina' if x == 'Gasolina'  else 'Demais')

    df['canal_bin'] =  df['canal'].apply(lambda x: 'Corretor_Mais' if x == 'Corretor Mais' else 
                                                   'Corretor_Mais' if x == 'CorretorMais'
                                                                   else    'Demais')
   
    df['per_com_bin'] = df['per_com'].apply(lambda x: 'p_1_10' if x < 10 else
                                                      'p_10_35' if x >= 10 else 'ERROR')    



    df['idade_ult_RNS_bin'] = df['idade_ult_RNS'].apply(lambda x:     'Sem_Sinistro' if x == 'Sem Sinistro' else
                                                                      '01_05_anos' if x == '01 ano' else
                                                                      '01_05_anos' if x == '02 anos' else
                                                                      '01_05_anos' if x == '03 anos' else
                                                                      '01_05_anos' if x == '04 anos' else
                                                                      '01_05_anos' if x == '05 anos' else '06_inf')


    df['UF_bin'] = df['UF'].apply(lambda x:     'AL_CE_RS'  if x == 'AL' else
                                                'AL_CE_RS'  if x == 'CE' else
                                                'AL_CE_RS'  if x == 'RS' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'AP' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'DF' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'ES' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'PA' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'PB' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'RN' else 
                                                'AP_DF_ES_PA_PB_RN_SC' if x == 'SC' else 
                                                'GO_MA_MG'             if x == 'GO' else 
                                                'GO_MA_MG'             if x == 'MA' else 
                                                'GO_MA_MG'             if x == 'MG' else 
                                                'MT_PR'                if x == 'MT' else
                                                'MT_PR'                if x == 'PR' else 
                                                'RJ'                   if x == 'RJ' else  
                                                'Demais')

    df['md_tipo_renov_ant_bin'] = df['Renovacao_ant'].apply(lambda x:     '1_SeguroNovo'       if x == '1 - Seguro Novo'  else
                                                                          '2_RenovPropria'     if x == '2 - Renov MAPFRE sem sinistro'   else
                                                                          '2_RenovPropria'     if x == '3 - Renov MAPFRE com sinistro'   else
                                                                          '3_RenovCongenere'   if x == '4 - Renov Congenere sem sinistro' else
                                                                          '3_RenovCongenere'   if x == '5 - Renov Congenere com sinistro' else
                                                                          '3_RenovCongenere'   if x == '6 - Renov BB sem sinistro' else
                                                                          '3_RenovCongenere'   if x == '7 - Renov BB com sinistro' else  
                                                                          '2_RenovPropria')
  
    df['MD_tipo_renov_bin'] = df['MD_tipo_renov'].apply(lambda x:          '2_RenovPropria_sem_sin'     if x == '2 - Renov MAPFRE sem sinistro'   else
                                                                           '3_RenovPropria_com_sin'     if x == '3 - Renov MAPFRE com sinistro'   else
                                                                           'Error')
   
     
    df['Cod_End_bin'] = df['Cod_End'].apply(lambda x: '13509'  if x == 13509 else
                                                      '13509'  if x == 50007 else
                                                      '13509'  if x == 50100 else 'Demais')   

    df['MD_capac_bin'] = df['MD_capac'].apply(lambda x: '5'  if x == 5  else 'Demais')  



    df['fator_ajuste_bin'] = df['MD_fator_ajuste'].apply(lambda x: 'f_80_101'    if x < 101 else
                                                                   'f_101_120'   if x >= 101  else 
                                                                   'f_80_101')   
    
    return df
