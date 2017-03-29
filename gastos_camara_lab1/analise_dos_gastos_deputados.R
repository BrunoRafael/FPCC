library(dplyr, warn.conflicts = F)
library(readr)
library(ggplot2)

gastos <- read_csv("dados//gastos-cota_atividade_parlamentar.csv")
View(gastos)

gastos_filter <- gastos %>% select(sgPartido, 
                                  txtCNPJCPF,
                                  sgUF,
                                  txNomeParlamentar,
                                  txtDescricao,
                                  vlrLiquido)

gastos_filter2 <- gastos %>% select(sgPartido, 
                                    txtCNPJCPF,
                                    sgUF,
                                    txNomeParlamentar,
                                    numMes, numAno,
                                    txtDescricao,
                                    vlrLiquido)

res <- gastosFilter %>% group_by(txNomeParlamentar) %>%
  summarise(total = sum(vlrLiquido)) %>% arrange(-total)
View(res)

glimpse(res)

res %>% ggplot(aes(x = "Deputado", y = total)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2, alpha = 0.5)

test <- count(gastos_filter$txNomeParlamentar)

View(test)
#res %>% mutate(total = total/2) #%>% 

res2 <- gastos_filter %>% group_by(txNomeParlamentar)

#get total of the occurrences without repeat
#length(unique(gastos_filter$txNomeParlamentar))

# Replace value Emissão Bilhete Aéreo to PASSAGENS AÉREAS
gastos_filter %>% select(txtDescricao) %>% 
  mutate(txtDescricao = ifelse(txtDescricao == "Emissão Bilhete Aéreo", "PASSAGENS AÉREAS", txtDescricao)) %>% View()


gastos_filter %>% 
  filter(!is.na(txtDescricao) && !is.na(vlrLiquido)) %>%
  group_by(txtDescricao) %>% 
  summarise(total = sum(vlrLiquido)) %>% 
  ggplot(aes(x = "Despesa", y = total)) + geom_point(stat = "identity")
  
#Quais deputados possuem gastos com valores abaixo de zero?
