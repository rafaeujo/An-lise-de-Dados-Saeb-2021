# /***************************************************************************************/
# /*  INEP/Daeb-Diretoria de Avalia??o da Educa??o B?sica                                */ 
# /*                                   			                                             */
# /*  Coordena??o-Geral de Instrumentos de Medidas                                       */
# /*-------------------------------------------------------------------------------------*/
# /*  PROGRAMA:                                                                          */
# /*               INPUT_R_TS_ESCOLA                                                     */
# /*-------------------------------------------------------------------------------------*/
# /*  DESCRICAO:   PROGRAMA PARA LEITURA DOS RESULTADOS DAS ESCOLAS DO SAEB 2021         */
# /*                                                                                     */
# /***************************************************************************************/
# /* Obs:                                                                                */
# /* 		                                                                                 */
# /* Para abrir os microdados, ? necess?rio salvar este programa e o arquivo             */
# /* TS_ESCOLA.CSV no diret?rio C:\ do computador.	                                     */
# /*							                                                                       */ 
# /* Ao terminar esses procedimentos, execute o programa salvo utilizando                */
# /* as vari?veis de interesse.                                                          */
# /***************************************************************************************/
# /*                                  ATEN??O                                            */ 
# /***************************************************************************************/
# /* Este programa abre a base de dados com os r?tulos das vari?veis de	                 */
# /* acordo com o dicion?rio de dados que comp?e os microdados. Para abrir               */
# /* os dados sem os r?tulos, basta importar diretamente no R, executando                */
# /* o programa apenas at? a carga dos microdados.                                       */
# /* 							                                                                       */                                                         
# /***************************************************************************************/;
# 
# --------------------
# Intala??o do pacote Data.Table
# (Se n?o estiver instalado 
# --------------------
 if(!require(data.table)){install.packages('data.table')}

#--------------------
# Caso deseje trocar o local do arquivo, edite a fun??o setwd() a seguir
# informando o local do arquivo.
# Ex. Windows setwd("C:/temp")
#     Linux   setwd("/home")
#--------------------
setwd('C:/') 

#---------------
# Aloca??o de mem?ria
#---------------
memory.limit(24576)

#------------------
# Carga dos microdados

TS_ESCOLA <- data.table::fread(input='TS_ESCOLA.csv',integer64='character')

# A script a seguir formata os r?tulos das vari?veis
# Para formatar um item retire o caracter de coment?rio (#) no in?cio na linha desejada 
# (Para retirar o caracter de coment?rio de v?rias linhas de uma s? vez, selecione as linhas desejadas e tecle ctrl+shift+c)
#---------------------------

TS_ESCOLA$ID_REGIAO <- factor(TS_ESCOLA$ID_REGIAO, levels = c(1,2,3,4,5),
                               labels = c( 'Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste'))
 
 TS_ESCOLA$ID_UF <- factor(TS_ESCOLA$ID_UF, levels = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                           labels = c( 'RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO', 'DF'))
 
 TS_ESCOLA$ID_AREA <- factor(TS_ESCOLA$ID_AREA, levels = c(1,2),
                             labels = c( 'Capital', 'Interior'))
 
 TS_ESCOLA$IN_PUBLICA <- factor(TS_ESCOLA$IN_PUBLICA, levels = c(0,1),
                                        labels = c('Privada','P?blica'))
 
 TS_ESCOLA$ID_LOCALIZACAO <- factor(TS_ESCOLA$ID_LOCALIZACAO, levels = c(1,2),
                                    labels = c('Urbana', 'Rural'))
########################################################################
 #Analisando a profissionalidade dos funcionários
install.packages("ggplot2")
 library(ggplot2)
 library(tidyverse)

##Médias Quinto ano 
 
MEDIA_5EF <- TS_ESCOLA %>%
  pivot_longer(cols = c(MEDIA_5EF_LP, MEDIA_5EF_MT),
               names_to = "Disciplina",
               values_to = "Media") %>%
  mutate(Disciplina = ifelse(Disciplina == "MEDIA_5EF_LP", "Língua Portuguesa", "Matemática"))

ggplot(MEDIA_5EF, aes(x = ID_REGIAO, y = Media, fill = Disciplina)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Médias em Língua Portuguesa e Matemática no 5º ano",
       x = "Região",
       y = "Média",
       fill = "Disciplina") +
  scale_fill_manual(values = c("#112446","#2a9d8f"))

##Médias Nono ano 

MEDIA_9EF <- TS_ESCOLA %>%
  pivot_longer(cols = c(MEDIA_9EF_LP, MEDIA_9EF_MT),
               names_to = "Disciplina",
               values_to = "Media") %>%
  mutate(Disciplina = ifelse(Disciplina == "MEDIA_9EF_LP", "Língua Portuguesa", "Matemática"))

ggplot(MEDIA_9EF, aes(x = ID_REGIAO, y = Media, fill = Disciplina)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Médias em Língua Portuguesa e Matemática no 9º ano",
       x = "Região",
       y = "Média",
       fill = "Disciplina") +
  scale_fill_manual(values = c("#FF5733", "#FFC300"))

##Médias Ensino Médio Tradicional

MEDIA_EMT <- TS_ESCOLA %>%
  pivot_longer(cols = c(MEDIA_EMT_LP, MEDIA_EMT_MT),
               names_to = "Disciplina",
               values_to = "Media") %>%
  mutate(Disciplina = ifelse(Disciplina == "MEDIA_EMT_LP", "Língua Portuguesa", "Matemática"))

ggplot(MEDIA_EMT, aes(x = ID_REGIAO, y = Media, fill = Disciplina)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Médias em Língua Portuguesa e Matemática no Ensino Médio Tradicional",
       x = "Região",
       y = "Média",
       fill = "Disciplina") +
  scale_fill_manual(values = c("#6A0DAD", "#C77FF2"))

##Médias Ensino Médio Integrado

MEDIA_EMI <- TS_ESCOLA %>%
  pivot_longer(cols = c(MEDIA_EMI_LP, MEDIA_EMI_MT),
               names_to = "Disciplina",
               values_to = "Media") %>%
  mutate(Disciplina = ifelse(Disciplina == "MEDIA_EMI_LP", "Língua Portuguesa", "Matemática"))

ggplot(MEDIA_EMI, aes(x = ID_REGIAO, y = Media, fill = Disciplina)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Médias em Língua Portuguesa e Matemática no Ensino Médio Integrado",
       x = "Região",
       y = "Média",
       fill = "Disciplina") +
  scale_fill_manual(values = c("#1B9E77", "#A1D99B"))

##Médias Ensino Médio Integrado

MEDIA_EM <- TS_ESCOLA %>%
  pivot_longer(cols = c(MEDIA_EM_LP, MEDIA_EM_MT),
               names_to = "Disciplina",
               values_to = "Media") %>%
  mutate(Disciplina = ifelse(Disciplina == "MEDIA_EM_LP", "Língua Portuguesa", "Matemática"))

ggplot(MEDIA_EM, aes(x = ID_REGIAO, y = Media, fill = Disciplina)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Médias em Língua Portuguesa e Matemática no Ensino Médio",
       x = "Região",
       y = "Média",
       fill = "Disciplina") +
  scale_fill_manual(values = c("#E75480", "#F4A7B9"))

# Criando gráfico para a taxa de participação

 mediatxp5 <- mean(TS_ESCOLA$TAXA_PARTICIPACAO_5EF, na.rm = T)
 mediatxp9 <- mean(TS_ESCOLA$TAXA_PARTICIPACAO_9EF, na.rm = T)
 mediatxpEMT <- mean(TS_ESCOLA$TAXA_PARTICIPACAO_EMT, na.rm = T)
 mediatxpEMI <- mean(TS_ESCOLA$TAXA_PARTICIPACAO_EMI, na.rm = T)
 mediatxpEM <- mean(TS_ESCOLA$TAXA_PARTICIPACAO_EM, na.rm = T)
 
 datatx <-  data.frame(Ano_escolar = c("5º Ano EF", "9º Ano EF", "EM Tradicional","EM Integrado", "Ensino Medio"),
                       TaxaP_Saeb = c(mediatxp5, mediatxp9, mediatxpEMT, mediatxpEMI, mediatxpEM))
 
 datatx$TaxaP_Saeb_perc <- datatx$TaxaP_Saeb / max(datatx$TaxaP_Saeb) * 100
   
 ggplot(datatx, aes(x = Ano_escolar, y = TaxaP_Saeb_perc)) +
   geom_col(position = position_dodge(width = 0.3), width = 0.5, fill = "#1f78b4", color = "white") +  # geom_col para gráfico de barras com cor de preenchimento e borda
   theme_minimal() +  # Estilo do tema
   labs(title = "Média das Taxas de Participação da pesquisa por Ano Escolar",
        x = "Ano Escolar",
        y = "Taxa de participação (%)")  # Títulos dos eixos


                                              