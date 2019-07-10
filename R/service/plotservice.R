


library(gridExtra)


growth.function.list <-
  list(
    haehnel = 'Haehnel',
    soneja = 'Soneja',
    combo = 'Combo',
    skupin = 'Skupin'
  )

plotTimeLine <- function(df) {
  aqi.good.rgba <- 'rgba(122,217,105,.8)'
  aqi.soso.rgba <- 'rgba(249,158,56,.8)'
  aqi.bad.rgba <- 'rgba(227,111,72,0.8)'
  
  df$aqi.good <- 13
  df$aqi.bad <- 200
  df$aqi.soso <- 50
  
  
  df <-
    df %>% mutate(rh.ge.70 = as.numeric(humidity.rm >= 70) * 100)
  
  
  
  #browser()
  p1 <-    plot_ly(
    df,
    x = ~ datetime,
    y = ~ PM10.rm,
    name = 'PM10 DWD',
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(45,75,155)', width = 1)
  ) %>%
    add_trace(
      y = ~ P1.rm,
      name = 'P1 Luftdaten',
      line = list(color = 'rgb(115,207,201)', width = 1)
    ) %>%
    add_trace(
      y = ~ adjusted,
      name = 'P1 w/ growth function',
      line = list(color = 'rgb(218,93,120)', width = 1)
    )   %>%
    add_trace(
      y = ~ rh.ge.70,
      name = 'Humidity>70%',
      fill = 'tozeroy',
      fillcolor = 'rgba(181,227,181,.4)',
      line = list(color = 'rgba(181,227,181)', width = 0)
    ) %>%
    layout(yaxis = list(range = c(0, 100), title = ''),
           xaxis = list(title = ''),legend = list(x = 0.1, y = 0.9))
  
  return(p1)
  
  
}


plotScatterDWD_vs_Luftdaten <- function(df, selected.function) {
  model.1 <- lm(data = df, P1.rm ~ PM10.rm)
  rmse.1 <- sqrt(mean(model.1$residuals ^ 2))
  adjusted_r2.1 <-
    paste0('Adj.R2 : ',
           round(summary(model.1)$adj.r.squared, 2) ,
           '- rmse: ',
           rmse.1)
  
  
  model.2 <- lm(data = df, adjusted ~ PM10.rm)
  rmse.2 <- sqrt(mean(model.2$residuals ^ 2))
  adjusted_r2.2 <-
    paste0('Adj.R2 : ',
           round(summary(model.2)$adj.r.squared, 2),
           '- rmse: ',
           round(rmse.2, 2))
  
  
  gato <-
    df %>% select(datetime, PM10.rm, P1.rm, humidity.rm, adjusted)
  gato <-
    gato %>% gather(key, value, -datetime, -PM10.rm, -humidity.rm)
  
  dat_text <- data.frame(
    label = c(adjusted_r2.1, adjusted_r2.2),
    key   = c('P1.rm', 'adjusted')
  )
  
  key.labs <-
    c(paste0('Luftdaten ', growth.function.list[selected.function][[1]], ' PM10'),
      "Luftdaten original PM10")
  names(key.labs) <- c("adjusted", "P1.rm")
  
  
  p <-
    ggplot(gato, aes(PM10.rm, value)) + geom_point(aes(color = humidity.rm), size =
                                                     .5, alpha = .2) +
    scale_color_viridis(direction = -1) + xlim(0, 100) + ylim(0, 100)
  
  p <- p + geom_text(
    data    = dat_text,
    mapping = aes(x = 20, y = 90, label = label),
    hjust   = -0.1,
    vjust   = -1
  ) + facet_grid(. ~ key, labeller = labeller(key = key.labs)) + xlab('DWD PM10') + ylab('Luftdaten PM10')
  
  return(p)
}

plotScatterRatio_vs_Humidity <- function(df, selected.function) {
  df$ratio.2 <- df$PM10.rm / df$P1.rm
  df$ratio.1 <- df$PM10.rm / df$adjusted
  
  df <- df %>% na.omit(.)
  
  gato <-
    df %>% select(datetime, ratio.1, ratio.2, humidity.rm, temperature.rm)
  
  gato <-
    gato %>% gather(key, value, -datetime, -humidity.rm, -temperature.rm)
  
  key.labs <-
    c(paste0('Luftdaten ', growth.function.list[selected.function][[1]], ' PM10'),
      "Luftdaten original PM10")
  names(key.labs) <- c("ratio.1", "ratio.2")
  
  p <-
    ggplot(gato, aes(humidity.rm, value)) + geom_point(aes(color =
                                                             temperature.rm),
                                                       alpha = .2,
                                                       size = .5) + ylim(0, 10) + scale_color_viridis(option =
                                                                                 "magma")
  
  p <-
    p + facet_grid(. ~ key, labeller = labeller(key = key.labs)) + ylab('Ratio DWD/Luftdaten on PM10') +
    xlab('Humidity (%)')
  return (p)
}


plotScatter <- function(df, selected.function) {
  p1 <- plotScatterDWD_vs_Luftdaten(df, selected.function)
  p2 <- plotScatterRatio_vs_Humidity(df, selected.function)
  
  p <- grid.arrange(p1, p2, nrow = 2)
  
  return(p)
}