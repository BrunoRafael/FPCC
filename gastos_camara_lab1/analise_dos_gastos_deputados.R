---
title: "Dicas de EDA para gastos da câmara"
---
  
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
                                  nuLegislatura,
                                  vlrLiquido) %>% 
  filter(!txNomeParlamentar %in% unique(sgPartido))

gastos_nordeste <- gastos_filter %>% 
  filter(sgUF %in% c("PB", "CE", "MA", "AL", "BA", "PE", "RN", "SE", "PI"),
         !is.na(txtDescricao),
         !is.na(vlrLiquido))%>%
  mutate(txtDescricao = ifelse(txtDescricao == "Emissão Bilhete Aéreo",
                               "PASSAGENS AÉREAS", 
                               txtDescricao))


gastos_nordeste %>% select(txNomeParlamentar, sgUF) %>% 
  unique() %>%
  ggplot(mapping=aes(x = sgUF)) + 
  geom_histogram(stat = "count") +
  geom_rug(alpha = 0.7)



# Total gasto por cada parlamentar em todos os seus mandatos
gastos_filter %>% group_by(txNomeParlamentar) %>%
  summarise(total = sum(vlrLiquido)) %>% arrange(-total) %>% View()
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

gastos_nordeste %>% 
  group_by(txNomeParlamentar, txtDescricao) %>%
  summarise(total = sum(vlrLiquido)) %>%
  filter(total==max(total))%>%
  arrange(desc(total)) %>%
  top_n(n=10)%>%
  
  #ggplot(aes(x = txtDescricao, y = total)) + geom_point()+
  #labs(x = "despesa", y = "Total gasto pelos deputados (R$)") +
  #coord_flip()

View()

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
  summarise(total = sum(vlrLiquido)/1000000) %>%
  arrange(-total) %>%
  ggplot(aes(x = txtDescricao, y = total)) + 
  geom_histogram(stat = "identity") +
  ggtitle("Histograma Preços de Imóveis") + # adiciona título
  theme_bw() +
  coord_flip()
  
#Quais deputados possuem gastos com valores abaixo de zero?
