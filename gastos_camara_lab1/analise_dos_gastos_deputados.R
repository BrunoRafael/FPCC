---
title: "Dicas de EDA para gastos da câmara"
---
  
library(dplyr, warn.conflicts = F)
library(readr)
library(ggplot2)

gastos <- read_csv("dados//gastos-cota_atividade_parlamentar.csv")

gastos_nordeste <- gastos %>% 
  filter(sgUF %in% c("PB", "CE", "MA", "AL", "BA", "PE", "RN", "SE", "PI"),
         !is.na(txtDescricao),
         !is.na(vlrLiquido))%>%
  mutate(txtDescricao = ifelse(txtDescricao == "Emissão Bilhete Aéreo",
                               "PASSAGENS AÉREAS", 
                               txtDescricao)) %>%
  filter(txtDescricao!="PASSAGENS AÉREAS"|| txtDescricao=="PASSAGENS AÉREAS" && vlrDocumento > 0)

gastos %>% filter(numAno %%1!= 0) %>% View()
gastos_nordeste <- gastos_nordeste %>% select(sgPartido, 
                                   txtCNPJCPF,
                                   sgUF,
                                   nuLegislatura,
                                   txNomeParlamentar,
                                   txtDescricao,
                                   numAno,
                                   numMes,
                                   vlrLiquido) %>% 
  filter(!txNomeParlamentar %in% unique(sgPartido))

#quantas notas fiscais foram emitidas por cada estado
gastos_nordeste %>% select(txNomeParlamentar, sgUF) %>% 
  unique() %>%
  ggplot(mapping=aes(x = sgUF)) + 
  geom_histogram(stat = "count") +
  geom_rug(alpha = 0.7)

# Total gasto por cada parlamentar em todos os seus mandatos
gastos_filter %>% 
  group_by(txNomeParlamentar) %>%
  summarise(total = sum(vlrLiquido)) %>% 
  arrange(-total) %>%
  ggplot(aes(x = "Deputado", y = total)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2, alpha = 0.5)

glimpse(res)

#Deputados que mais gastam
gastos_filter %>% group_by(txNomeParlamentar) %>%
  summarise(total = sum(vlrLiquido)) %>% arrange(-total) %>% 
  filter() %>% View()

#test <- count(gastos_filter$txNomeParlamentar)
#res %>% mutate(total = total/2) #%>% 

#res2 <- gastos_filter %>% group_by(txNomeParlamentar)

#get total of the occurrences without repeat
#length(unique(gastos_filter$txNomeParlamentar))

# Replace value Emissão Bilhete Aéreo to PASSAGENS AÉREAS
#gastos_filter <- gastos_filter %>%
#  filter(!is.na(txtDescricao) && !is.na(vlrLiquido)) %>% 
#  mutate(txtDescricao = 
#           ifelse(txtDescricao == "Emissão Bilhete Aéreo", 
#                  "PASSAGENS AÉREAS", 
#                  txtDescricao)) %>% 

gastos_nordeste %>% 
  group_by(txtDescricao) %>% 
  summarise(total = sum(vlrLiquido)) %>% 
  ggplot(aes(x = "Despesa", y = total)) + geom_point(stat = "identity")

# mostra todos os deputados com gastos negativos
gastos_nordeste %>% 
  group_by(txNomeParlamentar, txtDescricao) %>%
  select(txNomeParlamentar,txtDescricao, vlrLiquido) %>%
  filter(vlrLiquido <= 0) %>%
  arrange(vlrLiquido) %>% View()

# Com o que mais os deputados gastam?
gastos_nordeste %>%
  group_by(txNomeParlamentar, txtDescricao) %>%
  summarise(total = sum(vlrLiquido)) %>%
  filter(total==max(total))%>%
  arrange(desc(total)) %>%
  ggplot(aes(x = txtDescricao, y = total)) + 
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2, alpha = 0.5)+
  labs(x = "despesa", y = "Total gasto pelos deputados (R$)") +
  coord_flip()

#verificar viés das 5 maiores coisas que os deputados gastam
#dropar os nomes das despesas

# Com o que mais os deputados gastam? (Resultado em tabela)
gastos_nordeste %>% 
  group_by(txNomeParlamentar, txtDescricao) %>%
  summarise(total = sum(vlrLiquido)) %>%
  filter(total==max(total))%>%
  arrange(desc(total)) %>% View()
  
  #ggplot(aes(x = txtDescricao, y = total)) + geom_point()+
  #labs(x = "despesa", y = "Total gasto pelos deputados (R$)") +
  #coord_flip()

#gastos_nordeste %>% 
#  group_by(txtDescricao) %>%
#  summarise(total = sum(vlrLiquido)) %>%
#  filter(total==max(total))%>%
#  View()

#ggplot(aes(x = txtDescricao, y = total)) + geom_point()+
  #labs(x = "despesa", y = "Total gasto pelos deputados (R$)") +
  #coord_flip()

gastos_nordeste %>% 
  group_by(txtDescricao) %>%
  summarise(total = sum(vlrLiquido)) %>%
  arrange(-total) %>%
  ggplot(aes(x = txtDescricao, y = total)) + 
  geom_histogram(stat = "identity") +
  ggtitle("Histograma Preços de Imóveis") + # adiciona título
  scale_x_discrete()+
  coord_flip()

gastos_nordeste %>% filter(txtDescricao == "PARTICIPAÇÃO EM CURSO, PALESTRA OU EVENTO SIMILAR") %>% View()

gastos_nordeste %>% group_by(txNomeParlamentar, txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido))
ggplot(mapping=aes(x = sgUF)) + 
  geom_histogram(stat = "count") +
  geom_rug(alpha = 0.7)

gastos_nordeste %>% group_by(txNomeParlamentar, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)) %>%
  ggplot(aes(numMes, total)) + 
  geom_point()

gastos_nordeste %>% group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)/1000000) %>%
  ggplot(aes(numMes, total)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2, alpha = 0.5)+
  labs(y = "Total gasto (Em milhões R$)")

gastos_nordeste %>% group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)/1000000) %>%
  ggplot(aes(numMes, total)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 2, alpha = 0.5)+
  labs(y = "Total gasto (Em milhões R$)")
# gastos mensais de cada tipo de despesa
gastos_nordeste %>% 
  select(txtDescricao, numMes, numAno, vlrLiquido)%>%
  group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)/1000000) %>%
  filter(total == max(total), numAno == 2016) %>% 
  arrange(-numMes, numAno) %>%
  ggplot(aes(numMes, total)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.5)+
  labs(y = "Total gasto (Em milhões R$)")

gastos_nordeste %>% 
  select(txtDescricao, numMes, numAno, vlrLiquido)%>%
  group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)/1000000) %>%
  filter(total == max(total), numAno == 2017) %>% 
  arrange(-numMes, numAno) %>%
  ggplot(aes(numMes, total)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.5)+
  labs(y = "Total gasto (Em milhões R$)")

g <- gastos_nordeste %>% 
  select(txtDescricao, numMes, numAno, vlrLiquido)%>%
  group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)) %>%
  filter(numAno == 2015) 




g2 <- g %>% group_by(txtDescricao, numMes) %>%
  filter(total == max(total)) %>% View()

gastos_nordeste %>% 
  select(txtDescricao, numMes, numAno, vlrLiquido)%>%
  group_by(txtDescricao, numMes, numAno) %>%
  summarise(total = sum(vlrLiquido)) %>%
  filter(numAno == 2015) %>%
  arrange(-total) %>% 
  ggplot() +
  scale_x_continuous(breaks=1:12) +
  geom_point(aes(numMes, total), colour="blue", position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.5)+
  labs(x = "Mês", y = "Total gasto (Em milhões R$)")
#Quais deputados possuem gastos com valores abaixo de zero?
gastos_nordeste %>%
  group_by(txtDescricao, numMes, numAno) %>% 
  filter(numMes == 1, numAno == 2016) %>% View()