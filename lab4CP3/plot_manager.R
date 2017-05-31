plot_ic = function(b1, percentiles){
  median.weekend = b1 %>% 
    CI.percentile(probs = c(.025, .975))
  
  df.median = data.frame(median.weekend)
  data.frame(b1$replicates)%>% 
    ggplot(aes(x=median.users.)) +
    geom_histogram(binwidth = 10, colour = "#6ba82f", fill = "darkgreen") + 
    geom_vline(aes(xintercept = df.median$X2.5.), colour = "blue") + 
    geom_text(aes(x = df.median$X2.5., y = 500, label = "Início do intervalo"))+
    geom_vline(aes(xintercept = df.median$X97.5.), colour = "blue") + 
    geom_text(aes(x = df.median$X97.5., y = 1500,  label = "Fim do intervalo"))+
    geom_vline(aes(xintercept = b1$observed), colour = "darkorange")+
    geom_text(aes(x = b1$observed, y = 1000, label = "Média da amostra"))+
    labs(x = "distribuição das medianas das amostras", y = "Frequência de medianas")
}