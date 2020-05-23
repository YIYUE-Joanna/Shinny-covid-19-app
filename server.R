library(shiny)
library(ggplot2)
library(tidyverse)
require(nCov2019)
library(dplyr)
library(forcats)
# Define server logic for random distribution application
shinyServer(function(input, output) {

  
  output$countries <- renderPlot({
    daily_data <- get_nCov2019()
    p <- plot(daily_data)
    print(p)
  })
  
  # bar
  output$barchart <- renderPlot({
    library(ggplot2)
    library(dplyr)
    library(forcats)
    data <- global_data[1:20,]
    data <-data[order(data[,2],decreasing=TRUE),]
    data%>%
      mutate(name = fct_reorder(data$name, data$confirm)) %>%
      ggplot(aes(x= confirm, y= name)) + geom_bar(stat="identity", fill = "#4682B4", alpha=2, width=0.5)+xlab("total cases")+ ylab("top 20 countries") + ggtitle("total covid-19 cases")+theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label = confirm),hjust=0,vjust=0.5,colour = "red", size =3.9)
  })
  # linechart
  output$linechart <- renderPlot({
    require(ggplot2)
    require(ggrepel)
    require(dplyr)
    x = load_nCov2019()
    global_data_1 <- x['global']
    global_data_1
    n <- global_data_1 %>% filter(time == time(x)) %>%
      top_n(7, cum_confirm) %>%
      arrange(desc(cum_confirm)) %>%
      pull(country)
    n
    ggplot(filter(global_data_1, country %in% n),
           aes(time, cum_confirm, color=country)) +
      geom_line() +
      geom_text_repel(aes(label=country),
                      function(global_data) global_data[global_data$time == time(x),]) +
      theme_minimal(base_size=14) +
      theme(legend.position = "none") +
      xlab("Month") + ylab("total cases") +ggtitle("top 7 countries") + theme(plot.title = element_text(hjust = 0.5))
  })
  ####SEIR
  output$SEIR <- renderPlot({
    N = 10000 #population
    
    E = 0
    
    I = 1
    
    S = N-I
    
    R = 0
    
    r = 20
    
    B = 0.03 #Basic reproduction number
    
    a = 0.07#1/latent period 
    
    y = 0.01 #1/days to recover
    T = 150
    for (i in 1:(T-1)){
      
      S[i+1] = S[i] - r*B*S[i]*I[i]/N
      
      E[i+1] = E[i] + r*B*S[i]*I[i]/N - a*E[i]
      
      I[i+1] = I[i] + a*E[i] - y*I[i]
      
      R[i+1] = R[i] + y*I[i]}
    result <- data.frame(S, E, I, R)
    
    #View(result)
    
    X_lim <- seq(1,T,by=1)
    
    plot(S~X_lim, pch=15, col="DarkTurquoise", main = "NZ SEIR Model", type = "l", xlab = "TIME", ylab = "total number", xlim = c(0,T), ylim = c(0,10000))
    
    lines(S, col="corn flower blue", lty=1)
    
    lines(E, col="firebrick", lty=1)
    
    lines(I, col="gold", lty=1)
    
    lines(R, col="olive drab", lty=1)
    
    legend(100,8000,c("Susceptible","Exposed","Infectious","Recovered"),col=c("corn flower blue","firebrick","gold","olive drab"),text.col=c("corn flower blue","firebrick","gold","olive drab"),lty=c(1,1,1,1))
    
})
})
