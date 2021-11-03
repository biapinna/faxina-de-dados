
# Pacotes -----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(devtools)

# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("janitor")
# install.packages("devtools")
# devtools::install_github("abjur/brcrimR")


# Ler dados ---------------------------------------------------------------

df <- NULL
vec <- as.character(1:12)
for (i in vec) {
  print(i)
  getdata <- brcrimR::get_detailed_table_sp(folder = 'btnFeminicidio', year = '2020', month = i, department = '0')
  df <- rbind(df,getdata)
}

saveRDS(df, "trab_final/sspfeminicidio2020.Rds")

base_bruta <- readRDS("trab_final/sspfeminicidio2020.Rds")

# Experimentos ------------------------------------------------------------

base_bruta %>%
  tibble::view()

base_bruta %>% 
  dplyr::glimpse()

# qual unidade que vamos considerar?

# a unidade é a ocorrência ou a vítima?

ocorr <- base_bruta %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME) 

aut_vit <- base_bruta %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, 
               TIPOPESSOA) 


teste <- base_bruta %>% 
  count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO,
        STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO, NATUREZAVINCULADA) %>% 
  filter(n > 1)


# Arrumar a base de dados ----------------------------------------------

#bo_emitido = data registro
#dataocorrencia = data fato

base_clean <- base_bruta %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  dplyr::mutate(
    dataocorrencia = lubridate::dmy(dataocorrencia),
    datacomunicacao = lubridate::dmy(datacomunicacao),
    dataelaboracao = lubridate::dmy_hms(dataelaboracao),
    bo_emitido = lubridate::dmy_hms(bo_emitido)) %>%
  dplyr::filter(lubridate::year(bo_emitido) > 2019)

teste <- base_clean %>%
  filter(month(bo_emitido)==1) %>%
  count(numero_boletim, delegacia_nome, dataocorrencia, bo_emitido)

names(base_clean)

teste2 <- base_clean %>%
  count(mes = month(bo_emitido), numero_boletim, delegacia_nome, dataocorrencia, bo_emitido, flagrante)

teste3 <- base_clean %>%
  count(mes_bo_emitido = month(bo_emitido), numero_boletim, bo_emitido, dataocorrencia, peridoocorrencia, cidade, descricaolocal, solucao, delegacia_nome, delegacia_circunscricao)

ty <- as.data.frame(table(teste3$numero_boletim))
table(teste2$mes)

# Separando a base: vítimas e autores ----------------------------------------------

autor_perfil <- base_clean %>% 
  dplyr::filter(tipopessoa == 'Vítima' | tipovinculo == 'Vítima') %>%
  dplyr::select(num_bo, ano_bo, numero_boletim, bo_emitido, dataocorrencia, delegacia_nome, delegacia_circunscricao, cidade, descricaolocal, naturalidade, sexo, idade, estadocivil, profissao, grauinstrucao, corcutis, relacionamento) %>%
  dplyr::distinct()
  
table(base_bruta$TIPOVINCULO)

vitimas_passo1 <- base_clean %>% 
  dplyr::filter(tipopessoa == 'Vítima' | tipovinculo == 'Vítima') %>%
  dplyr::select(num_bo, ano_bo, numero_boletim, bo_emitido, dataocorrencia, delegacia_nome, delegacia_circunscricao, cidade, descricaolocal, naturalidade, sexo, idade, estadocivil, profissao, grauinstrucao, corcutis, relacionamento,
                especie:desdobramento) %>% 
  dplyr::distinct() %>% 
  tidyr::unite(crime_completo, especie, rubrica, desdobramento, sep = " @@@ ")

# Opção C - nest: transformar as linhas todas da tabela de crimes em uma list-column

vitimas_passo2 <- vitimas_passo1 %>% 
  dplyr::group_by(num_bo, ano_bo, numero_boletim, bo_emitido, dataocorrencia, delegacia_nome, delegacia_circunscricao, cidade, descricaolocal, naturalidade, sexo, idade, estadocivil, profissao, grauinstrucao, corcutis, relacionamento) %>% 
  tidyr::nest()
