#teste

import pandas as pd
import numpy as np
import streamlit as st
import matplotlib.pyplot as plt

data = {
        "Ano": [2018, 2019, 2020, 2021, 2022],
        "Vendas": [100, 150, 200, 250, 300]}
df = pd.DataFrame(data)
print(df)

fig, ax = plt.subplots()
ax.plot(df['Ano'], df['Vendas'], marker='o')
ax.set_title('Vendas Anuais')
ax.set_xlabel('Ano')
ax.set_ylabel('Vendas')
st.pyplot(fig)

ano_min, ano_max = st.slider('Selecione o intervalo de anos', min_value=2018, max_value=2022, value=(2018, 2022))
df_filtered = df[(df['Ano'] >= ano_min) & (df['Ano'] <= ano_max)]

fig, ax = plt.subplots()
ax.plot(df_filtered['Ano'], df_filtered['Vendas'], marker='o')
ax.set_title('Vendas Anuais')
ax.set_xlabel('Ano')
ax.set_ylabel('Vendas')

#streamlit run dashboard.py

import streamlit as st
 
st.title('My First Streamlit App')