
plotTimeLine <- function(df){
 
  aqi.good.rgba <- 'rgba(122,217,105,.8)'
  aqi.soso.rgba <- 'rgba(249,158,56,.8)'
  aqi.bad.rgba <- 'rgba(227,111,72,0.8)'
  
  df$aqi.good <- 13
  df$aqi.bad <- 200
  df$aqi.soso <- 50
  
  
  df <- df %>% mutate(rh.ge.70=as.numeric(humidity.rm>=70)*100)
  

  
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
      y = ~ haehnel,
      name = 'P1 w/ growth function',
      line = list(color = 'rgb(218,93,120)', width = 1))   %>%
  add_trace(
    y = ~ rh.ge.70,
    name = 'Humidity>70%',
    fill = 'tozeroy',
    fillcolor = 'rgba(181,227,181,.4)',
    line = list(color = 'rgba(181,227,181)', width = 0)
  ) %>%
    layout( yaxis = list(range = c(0, 100),title=''))
    # )  %>%
    # add_trace(
    #   y = ~ aqi.good,
    #   name = 'AQI Good',
    #   # fill = 'tozeroy',
    #   # fillcolor = aqi.good.rgba,
    #   line = list(color = aqi.good.rgba, width = 3,dash='dot')
    # ) %>%
    # add_trace(
    #   y = ~ aqi.soso,
    #   type = 'scatter',
    #   mode = 'lines',
    #   # fill = 'tonexty',
    #   # fillcolor = aqi.soso.rgba,
    #   line = list(color = aqi.soso.rgba,width=3,dash='dot'),
    #   showlegend = TRUE,
    #   name = 'AQI SOSO'
    # ) %>%
    # add_trace(
    #   y = ~ aqi.bad,
    #   type = 'scatter',
    #   mode = 'lines',
    #   # fill = 'tonexty',
    #   # fillcolor = aqi.bad.rgba,
    #   line = list(color = aqi.bad.rgba),
    #   showlegend = TRUE,
    #   name = 'AQI BAD'
    # ) 
  
  p2 <-
    plot_ly(
      df,
      x = ~ datetime,
      y = ~ humidity.rm,
      type = 'scatter',
      mode = 'lines',
      name = 'humidity',
      line = list(color = 'rgb(122,217,105)', width = 1.5)
    ) %>% layout(xaxis=list(title=''))
  
  p3 <-
    plot_ly(
      df,
      x = ~ datetime,
      y = ~ temperature.rm,
      
      type = 'scatter',
      mode = 'lines',
      name = 'temperature',
      line = list(color = 'rgb(255,162,10)', width = 1)
    )
  
  p <-
    subplot(
      p1,
      p2,
      nrows = 2,
      shareX = TRUE,
      heights = c(0.7, 0.3)
    )
  
  return(p1)
  return(p)
  
}
