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

 #Plotando a relação de escolas por região do Brasil
 
 contagem <- TS_ESCOLA %>%
   count(ID_REGIAO) %>%
   mutate(Porcentagem = n / sum(n) * 100) %>%
   arrange(desc(ID_REGIAO))  
 
 contagem
 
 ggplot(contagem, aes(x = "", y = Porcentagem, fill = factor(ID_REGIAO))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   theme_void() +
   geom_text(aes(label = paste0(round(Porcentagem, 1), "%")), position = position_stack(vjust = 0.5)) +
   scale_fill_manual(values = c("#BFD3E6", "#FFD700", "#FFA07A", "#CFCFC4", "#90EE90")) +  # Definir cores para cada ID_AREA
   labs(title = "Distribuição de localidade das Escolas por Região do País",
        fill = "Regiões",
        caption = "Baseado nos dados de TS_ESCOLA")
 
 #Plotando a relação escolas e area
 contagem <- TS_ESCOLA %>%
   count(ID_AREA) %>%
   mutate(Porcentagem = n / sum(n) * 100) %>%
   arrange(desc(ID_AREA)) 
 
 contagem
 
 ggplot(contagem, aes(x = "", y = Porcentagem, fill = factor(ID_AREA))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   theme_void() +
   geom_text(aes(label = paste0(round(Porcentagem, 1), "%")), position = position_stack(vjust = 0.5)) +
   scale_fill_manual(values = c("#e31a1c", "#ff7f00")) +  # Definir cores para cada ID_AREA
   labs(title = "Distribuição de localidade das Escolas por área",
        fill = "Local",
        caption = "Baseado nos dados de TS_ESCOLA")
 
 #Plotando a relação escola pública e privada
 
 contagem <- TS_ESCOLA %>%
   count(IN_PUBLICA) %>%
   mutate(IN_PUBLICA = ifelse(IN_PUBLICA == "P?blica", "Publica", "Matemática")) %>%
   mutate(Porcentagem = n / sum(n) * 100) %>%
   arrange(desc(IN_PUBLICA))  # Ordenar por ID_AREA, se necessário
 
 contagem
 
 ggplot(contagem, aes(x = "", y = Porcentagem, fill = factor(IN_PUBLICA))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   theme_void() +
   geom_text(aes(label = paste0(round(Porcentagem, 1), "%")), position = position_stack(vjust = 0.5)) +
   scale_fill_manual(values = c("#E75480", "#F4A7B9")) +  # Definir cores para cada ID_AREA
   labs(title = "Distribuição dos tipos de Escolas",
        fill = "Tipo",
        caption = "Baseado nos dados de TS_ESCOLA")
 
 #Plotando a relação de Localização
 
 contagem <- TS_ESCOLA %>%
   count(ID_LOCALIZACAO) %>%
   mutate(Porcentagem = n / sum(n) * 100) %>%
   arrange(desc(ID_LOCALIZACAO))
 
 contagem
 
 ggplot(contagem, aes(x = "", y = Porcentagem, fill = factor(ID_LOCALIZACAO))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   theme_void() +
   geom_text(aes(label = paste0(round(Porcentagem, 1), "%")), position = position_stack(vjust = 0.5)) +
   scale_fill_manual(values = c("#1B9E77", "#A1D99B")) +  
   labs(title = "Distribuição das Escolas Por LOcalização",
        fill = "Localização",
        caption = "Baseado nos dados de TS_ESCOLA")
 
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

##Médias Ensino Médio 

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

#Fazendo um boxplot do nível de Adequação da formação dos professores
 
 ggplot() +
   geom_boxplot(data = TS_ESCOLA, aes(x = "Formação docente EF Iniciais", y = PC_FORMACAO_DOCENTE_INICIAL), width = 0.5, fill = "#1f78b4", color = "black") +
   geom_boxplot(data = TS_ESCOLA, aes(x = "Formação docente EF Finais", y = PC_FORMACAO_DOCENTE_FINAL), width = 0.5, fill = "#112446", color = "black") +
   geom_boxplot(data = TS_ESCOLA, aes(x = "Formação docente EM", y = PC_FORMACAO_DOCENTE_MEDIO), width = 0.5, fill = "#6A0DAD", color = "black") +
   theme_minimal() +
   labs(title = "Boxplots Adequação da Formação dos professores",
        subtitle = "Baseado no íncice de adequação da Forfação.",
        x = "Anos Escolares",
        y = "Valor indice de adequação") +
   scale_x_discrete(labels = c("Formação docente EF Iniciais" = "Anos Iniciais do EF", "Formação docente EF Finais" = "Anos finais do EF", "Formação docente EM" = "Ensino Medio"))
           
 quartis <- data.frame(
   Variavel = c("Formação docente nos anos Iniciais", "Formação docente anos finais", "Formação docentes EM"),
   Q25 = c(quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_INICIAL, probs = 0.25, na.rm = TRUE),
           quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_FINAL, probs = 0.25, na.rm = TRUE),
           quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_MEDIO, probs = 0.25, na.rm = TRUE)),
   Mediana = c(quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_INICIAL, probs = 0.5, na.rm = TRUE),
               quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_FINAL, probs = 0.5, na.rm = TRUE),
               quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_MEDIO, probs = 0.5, na.rm = TRUE)),
   Q75 = c(quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_INICIAL, probs = 0.75, na.rm = TRUE),
           quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_FINAL, probs = 0.75, na.rm = TRUE),
           quantile(TS_ESCOLA$PC_FORMACAO_DOCENTE_MEDIO, probs = 0.75, na.rm = TRUE))
 )
 
 quartis
 
 ## Nível Socioeconomico
 
 conts <- TS_ESCOLA %>%
   count(NIVEL_SOCIO_ECONOMICO) %>%
   mutate(Porcentagem = n / sum(n) * 100) %>%
   arrange(desc(NIVEL_SOCIO_ECONOMICO))  
 
 ggplot(conts, aes(x = "", y = Porcentagem, fill = factor(NIVEL_SOCIO_ECONOMICO))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   theme_void() +
   geom_text(aes(label = paste0(round(Porcentagem, 1), "%")), position = position_stack(vjust = 0.5)) +
   scale_fill_manual(values = c("#BFD3E6", "#FFD700", "#FFA07A", "#CFCFC4", "#90EE90", "#E75480", "#F4A7B9", "#A1D99B")) +
   labs(title = "Distribuição de Localidade das Escolas por Região do País",
        fill = "Regiões",
        caption = "Baseado nos dados de TS_ESCOLA")
