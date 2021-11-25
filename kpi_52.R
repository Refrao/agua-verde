library(readr)
library(tidyverse)

# Importar arquivos

kpi_52_1 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_1.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                          VKONT = col_integer()), trim_ws = TRUE)

kpi_52_2 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_2.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                            VKONT = col_integer()), trim_ws = TRUE)

kpi_52_3 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_3.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                            VKONT = col_integer()), trim_ws = TRUE)

kpi_52_4 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_4.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                            VKONT = col_integer()), trim_ws = TRUE)

kpi_52_5 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_5.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                            VKONT = col_integer()), trim_ws = TRUE)

kpi_52_6 <- read_delim("C:/Users/BR0083166156/Downloads/kpi/KPI_52_BLOQUEIOS_6.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(COD_PROCESSO = col_integer(), 
                                                                            VKONT = col_integer()), trim_ws = TRUE)

kpi <- bind_rows(kpi_52_1,kpi_52_2,kpi_52_3,kpi_52_4,kpi_52_5,kpi_52_6)

# Filtrar arquivos e excluir final da tabela

kpi <- kpi %>% filter(BUKRS == "2005")
kpi$BUKRS  <- as.integer(kpi$BUKRS)

# Criar tabela clientes por conta contrato e join

kpi_cc <- kpi %>% filter(is.na(XBLNR))

crclientes <- crclientes %>% rename("BUKRS" = "numero_cliente")

# Saber o que não está no SAP/Synergia

result_cc <- anti_join(kpi_cc,crclientes, by = c("VKONT" = "numero_cliente")) 

tipo_cc <- result_cc %>% group_by(LOCKR) %>% summarise(qtd = n())
            ungroup() %>% droplevels(.)

#Cria tabela de faturas e analise
            
kpi_fat <- kpi %>% filter(!is.na(XBLNR))

tipo_fat <- kpi_fat %>% group_by(LOCKR) %>% 
                        summarise(qtd = n()) %>% 
                        ungroup() %>% droplevels(.)

