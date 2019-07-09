
plotTimeLine <- function(df){
 
  aqi.good.rgba <- 'rgba(166,221,171,0.4)'
  aqi.soso.rgba <- 'rgba(254,255,160,0.4)'
  aqi.bad.rgba <- 'rgba(227,111,72,0.4)'
  
  df$aqi.good <- 13
  
  df$aqi.bad <- 200
  
  
  df$aqi.soso <- 50
  
  
  
  
  p1 <-
    plot_ly(
      df,
      x = ~ datetime,
      y = ~ PM10.rm,
      name = 'PM10',
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'rgb(245, 176, 66)', width = 1)
    ) %>%
    add_trace(
      y = ~ P1.rm,
      name = 'P1',
      line = list(color = 'rgb(0,66,37)', width = 1)
    ) %>%
    add_trace(
      y = ~ haehnel,
      name = 'P1.haehnel',
      line = list(color = 'rgb(94,182,104)', width = 1)
    )  %>%
    add_trace(
      y = ~ aqi.good,
      name = 'AQI Good',
      fill = 'tozeroy',
      fillcolor = aqi.good.rgba,
      line = list(color = aqi.good.rgba, width = 0)
    ) %>%
    add_trace(
      y = ~ aqi.soso,
      type = 'scatter',
      mode = 'lines',
      fill = 'tonexty',
      fillcolor = aqi.soso.rgba,
      line = list(color = aqi.soso.rgba),
      showlegend = FALSE,
      name = 'AQI SOSO'
    ) %>%
    add_trace(
      y = ~ aqi.bad,
      type = 'scatter',
      mode = 'lines',
      fill = 'tonexty',
      fillcolor = aqi.bad.rgba,
      line = list(color = aqi.bad.rgba),
      showlegend = FALSE,
      name = 'AQI SOSO'
    )
  
  p2 <-
    plot_ly(
      df,
      x = ~ datetime,
      y = ~ humidity.rm,
      type = 'scatter',
      mode = 'lines',
      name = 'humidity',
      line = list(color = 'rgb(92,184,178)', width = 1)
    )
  
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
  return(p)
  
}
