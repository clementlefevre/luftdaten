plotGrafana <- function(data){
  df<- as.data.frame(data$DT.grafana)
df$name<- factor(df$name,levels=c('clement','thibaut','mathilde'))

internet_1 <- list(
  xref = 'paper',
  yref = 'y',
  x = (max(df$datetime)-min(df$datetime))/10,
  y = 18,
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('Good Air Quality Threshold' ),
  font = list(family = 'Arial',
              size = 12,
              color = '#8590aa'),
  showarrow = FALSE)

df1<- df %>% filter(variable=='sds011_p2' )
df2<- df %>% filter(variable=='bme280_humidity' )

pal <- c("#2c7fb8", "#2ca25f", "#dd1c77")
p1 <- plot_ly(df1, x=~datetime, y=~value, color=~name, colors =pal,
              type = 'scatter', mode = "lines",line=list(width=1),legendgroup=~name,showlegend=TRUE) %>% 
  layout(yaxis = list(title = "Particles Matters (µg/m3)")) %>% 
  add_trace(data=df %>% filter(name=='clement'),y=~aqi.good, name='Air Quality OK (WHO)', showlegend=FALSE, 
            type='scatter', mode='lines',
            line = list(color = '#8590aa', width = 1, dash = 'dash'))%>% layout(annotations = internet_1) 

p2<- plot_ly(df2, x=~datetime, y=~value, color=~name, colors =pal,
             type = 'scatter', mode = "lines",line=list(width=1),legendgroup=~name,showlegend=FALSE) %>% 
  layout(yaxis = list(title = "Humidity (%)")) 

p <- subplot(p1, p2, nrows = 2,shareX = TRUE, titleY = TRUE,titleX = FALSE)
return(p)
}

plotMadavi <- function(data){  df<- as.data.frame(data$DT.madavi)

df$name<- factor(df$name,levels=c('clement','thibaut','mathilde'))

internet_1 <- list(
  xref = 'datetime',
  yref = 'value',
  x = max(df$datetime),
  y = 18,
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('Good Air Quality Threshold' ),
  font = list(family = 'Arial',
              size = 12,
              color = '#8590aa'),
  showarrow = FALSE)


pal <- c("#2c7fb8", "#2ca25f", "#dd1c77")
p1 <- plot_ly(df, x=~datetime, y=~SDS_P2, color=~name, colors =pal,
              type = 'scatter', mode = "lines",line=list(width=1),legendgroup=~name,showlegend=TRUE) %>% 
  layout(yaxis = list(title = "Particles Matters (µg/m3)")) %>% add_trace(data=df %>% filter(name=='clement'),y=~aqi.good, name='Air Quality OK (WHO)', showlegend=FALSE, 
                                                                          type='scatter', mode='lines',
                                                                          line = list(color = '#8590aa', width = 1, dash = 'dash'))%>% layout(annotations = internet_1) 

p2<- plot_ly(df, x=~datetime, y=~BME280_humidity, color=~name, colors =pal,
             type = 'scatter', mode = "lines",line=list(width=1),legendgroup=~name,showlegend=FALSE) %>% 
  layout(yaxis = list(title = "Humidity (%)")) 

p <- subplot(p1, p2, nrows = 2,shareX = TRUE, titleY = TRUE,titleX = FALSE)
return(p)
}
