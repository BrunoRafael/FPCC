plot_median_ic = function(data){
  median.monday = data %>% 
    CI.percentile(probs = c(.025, .975))
  
  df.median = data.frame(median.monday)

  data.frame(data$replicates)%>% 
    ggplot(aes(x=median.users.)) +
    geom_histogram(binwidth = 10, colour = "#6ba82f", fill = "darkgreen") + 
    geom_vline(aes(xintercept = df.median$X2.5.), colour = "blue") + 
    geom_text(aes(x = df.median$X2.5., y = 500, label = "Início do intervalo"))+
    geom_vline(aes(xintercept = df.median$X97.5.), colour = "blue") + 
    geom_text(aes(x = df.median$X97.5., y = 1500,  label = "Fim do intervalo"))+
    geom_vline(aes(xintercept = data$observed), colour = "darkorange")+
    geom_text(aes(x = data$observed, y = 1000, label = "Mediana da amostra"))+
    labs(x = "Medianas das amostras", y = "Frequência de medianas")
}


plot_sd_ic = function(data){
  sd.monday = data %>% 
    CI.percentile(probs = c(.025, .975))
  
  df.sd = data.frame(sd.monday)
  data.frame(data$replicates)%>% 
    ggplot(aes(x=sd.users.)) +
    geom_histogram(binwidth = 10, colour = "#6ba82f", fill = "darkgreen") + 
    geom_vline(aes(xintercept = df.sd$X2.5.), colour = "blue") + 
    geom_text(aes(x = df.sd$X2.5., y = 500, label = "Início do intervalo"))+
    geom_vline(aes(xintercept = df.sd$X97.5.), colour = "blue") + 
    geom_text(aes(x = df.sd$X97.5., y = 1500,  label = "Fim do intervalo"))+
    geom_vline(aes(xintercept = data$observed), colour = "darkorange")+
    geom_text(aes(x = data$observed, y = 1000, label = "Mediana da amostra"))+
    labs(x = "Medianas das amostras", y = "Frequência de medianas")
}