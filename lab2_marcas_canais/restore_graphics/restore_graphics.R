library(dplyr, warn.conflicts = F)
library(readr)
library(ggplot2)
library(knitr)

formata_label = function(l){
  # existem funções para formatar com , sendo milhar e . sendo decimal (scales::comma)
  # mas em pt_br é o contrário. essa função serve para isso
  format(l, big.mark = ".", scientific = FALSE, trim = TRUE, decimal.mark = ",")
} 

gastos_2009 <- read_csv2("dados//Ano-2009.csv") %>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2010 <- read_csv2("dados//Ano-2010.csv") %>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2011 <- read_csv2("dados//Ano-2011.csv") %>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2012 <- read_csv2("dados//Ano-2012.csv") %>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2013 <- read_csv2("dados//Ano-2013.csv")%>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2014 <- read_csv2("dados//Ano-2014.csv")%>% 
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos <- read_csv("dados//gastos-cota_atividade_parlamentar.csv") %>%
  select(sgPartido, txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)
cotas_por_estado <- read_csv2("dados//valor-cota-por-estado.csv")
deputados <- read_csv("dados//deputados-detalhes.csv")
ideologia <- read_csv("dados//partido-ideologias-observatorio_das_elites.csv")

gastos <- gastos %>% rbind(gastos_2009)
gastos <- gastos %>% rbind(gastos_2010)
gastos <- gastos %>% rbind(gastos_2011)
gastos <- gastos %>% rbind(gastos_2012)
gastos <- gastos %>% rbind(gastos_2013)
gastos <- gastos %>% rbind(gastos_2014)


gastos <- gastos %>% right_join(ideologia, c("sgPartido" = "sigla_partido"))
gastos <- gastos %>% select(-ideologia_nuancada, -ideol_tamanho_partido, -nome_partido)

Problemas <- c("Observações sem nenhum valor", "Valores com mesmo significado \"Emissão Bilhete\" Aéreo e \"PASSAGENS AÉREAS\"",
               "Valor dos documentos fiscais relacionados a passagens aéreas negativos")
Tratamentos <- c("Ocorrência ignorada.", "Os valores de \"Emissão Bilhete\" foram substituídas por \"PASSAGENS AÉREAS\".",
                 "Todas as ocorrências em que o TOTAL de gastos do parlamentar é negativo significa que o valor indica um ressarcimento em que o parlamentar fez a compra mas não usufruiu dela e o valor no fim das contas não foi gasto. Portanto todas as ocorrências em que o total ficou abaixo de zero, foram descartadas.")
frame <- data.frame(Problemas, Tratamentos)
kable(frame)

gastos_filter <- gastos %>% 
  filter(!is.na(vlrLiquido)) %>%
  filter(!is.na(numAno)) %>%
  mutate(txtDescricao = ifelse(txtDescricao == "Emissão Bilhete Aéreo", "PASSAGENS AÉREAS", txtDescricao))

gastos_filter <- gastos_filter %>% 
  filter(complete.cases(gastos_filter))

gastos_parlamentares <- gastos_filter %>% filter(!txNomeParlamentar %in% unique(sgPartido))
gastos_partido <- gastos_filter %>% filter(txNomeParlamentar %in% unique(sgPartido))

nomes_titulares <- deputados %>% filter(condicao=="Titular") %>% select(nomeParlamentar)
nomes_suplentes <- deputados %>% filter(condicao=="Suplente") %>% select(nomeParlamentar)

gastos_titulares <- gastos_parlamentares %>% filter(txNomeParlamentar %in% nomes_titulares$nomeParlamentar)
gastos_suplentes <- gastos_parlamentares %>% filter(txNomeParlamentar %in% nomes_suplentes$nomeParlamentar)

rm(gastos_2009)
rm(gastos_2010)
rm(gastos_2011)
rm(gastos_2012)
rm(gastos_2013)
rm(gastos_2014)

g <- gastos_titulares %>% 
  group_by(txNomeParlamentar, ideologia_simplificada, numAno) %>% 
  summarise(Total = sum(vlrLiquido))

g %>% group_by(ideologia_simplificada, numAno) %>% 
  filter(numAno < 2017 & numAno >= 2009) %>%
  ggplot(aes(x = as.factor(ideologia_simplificada), fill=as.factor(ideologia_simplificada))) + 
  geom_histogram(stat = "count", alpha=0.8) +
  facet_grid(.~numAno)+
  scale_y_continuous(labels=formata_label)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="Ideologia", y = "Deputados por ano", fill="Ideologia")