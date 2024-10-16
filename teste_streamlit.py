import streamlit as st
import pandas as pd

#import statsmodels.formula.api as smf

# Função para carregar arquivos
@st.cache_data
def load_file(file):
    if file.name.endswith('.csv'):
        return pd.read_csv(file)
    elif file.name.endswith('.xlsx'):
        return pd.read_excel(file)
    else:
        st.error("Formato de arquivo não suportado")
        return None

# Página Data
def page_data():
    st.title("Página Data")
    file = st.file_uploader("Importar arquivo CSV ou XLSX", type=['csv', 'xlsx'])
    if file:
        df = load_file(file)
        st.write("Dados importados:")
        st.write(df)
        code = st.text_area("Digite o código para manipular os dados")
        if st.button("Executar código"):
            exec(code, {'df': df, 'pd': pd, 'st': st})
            st.write("Dados manipulados:")
            st.write(df)

# Página Modeling
def page_modeling():
    st.title("Página Modeling")
    file = st.file_uploader("Selecionar arquivo CSV ou XLSX", type=['csv', 'xlsx'])
    if file:
        df = load_file(file)
        st.write("Dados importados:")
        st.write(df)
        formula = st.text_input("Digite a fórmula para o modelo")
        if st.button("Aplicar modelo"):
            model_glm = smf.logit(formula, data=df).fit()
            st.write(model_glm.summary())

# Página Price
def page_price():
    st.title("Página Price")
    file = st.file_uploader("Selecionar arquivo CSV ou XLSX", type=['csv', 'xlsx'])
    if file:
        df = load_file(file)
        st.write("Dados importados:")
        st.write(df)
        if st.button("Aplicar função"):
            import my_function
            result = my_function.apply_function(df)
            st.write("Resultado da função:")
            st.write(result)

# Configuração das páginas
page = st.sidebar.selectbox("Selecione a página", ["Data", "Modeling", "Price"])

if page == "Data":
    page_data()
elif page == "Modeling":
    page_modeling()
elif page == "Price":
    page_price()