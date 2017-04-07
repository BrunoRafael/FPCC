
library(dplyr)
library(ggplot2)
library(readr)

gastos_2010 <- read_csv2("dados//Ano-2010.csv") %>% 
  select(txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2011 <- read_csv2("dados//Ano-2011.csv") %>% 
  select(txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2012 <- read_csv2("dados//Ano-2012.csv") %>% 
  select(txNomeParlamentar, txtDescricao, 
           vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2013 <- read_csv2("dados//Ano-2013.csv")%>% 
  select(txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos_2014 <- read_csv2("dados//Ano-2014.csv")%>% 
  select(txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

gastos <- read_csv("dados//gastos-cota_atividade_parlamentar.csv") %>%
  select(txNomeParlamentar, txtDescricao, 
         vlrLiquido, nuLegislatura, codLegislatura, 
         sgUF, numMes, numAno)

cotas_por_estado <- read_csv2("dados//valor-cota-por-estado.csv")

gastos <- gastos %>% rbind(gastos_2010)
gastos <- gastos %>% rbind(gastos_2011)
gastos <- gastos %>% rbind(gastos_2012)
gastos <- gastos %>% rbind(gastos_2013)
gastos <- gastos %>% rbind(gastos_2014)

gastos <- gastos %>% 
  filter(!is.na(txtDescricao), !is.na(vlrLiquido))%>%
  mutate(txtDescricao = ifelse(txtDescricao == "Emissão Bilhete Aéreo",
                               "PASSAGENS AÉREAS", 
                               txtDescricao))

View(gastos)
#legislaturas <- list("53" = 2009:2010, "54"=2011:2014, "55"=2015:2017)
#calc_year <- function(nuLegislatura)
#{
#  u <- 0
#  for(leg in legislaturas)
#  {
#    if(nuLegislatura == leg)
#    {
#      u <- sample(legislaturas[leg], 1, replace=FALSE)
#    }
#  }
#  return(u)
#}
#gastos <- gastos %>% mutate(ifelse(numAno > 2017 | numAno < 2009, calc_year(nuLegislatura), numAno))
