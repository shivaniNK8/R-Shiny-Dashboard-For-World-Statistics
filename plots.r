

#Add color arguments
plot_choropleth <- function(df, attr, plot_title, legend_title) {
  fig <- plot_ly(
    df,
    type = 'choropleth',
    locations = df$CODE,
    z =  attr,
    text = df$CODE,
    #check
    frame = ~ Year#,
    #colorscale = list(c(22, 83), c("#f9ca8a", '#bd5868'))
  ) %>%
    layout(
      title = plot_title,
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      #fig_bgcolor = "rgb(255, 255, 255)", 
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      geo = list(
        scope = "world",
        showocean = TRUE,
        oceancolor = "#abd3df",
        showland = TRUE,
        landcolor = "#f1efe8"
      )
    ) %>%
    colorbar(title = legend_title) 
  
  return(fig)
}

plot_line <- 
  function ( df, attr1, attr2, plot_title,legend_title,xtitle, ytitle){
    fig <- plot_ly(df()) %>%
      add_lines(
        x=~attr1,
        y=~attr2,
        frame=~year,
        color =~factor(region),
        line=list(simplify=F)) %>%
      layout(
        title=plot_title,
        xaxis=list(title=xtitle),
        yaxis=list(title=ytitle),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
      )
    return(fig)
  }

plot_lineYear <- 
  function ( df, attr1, attr2, plot_title,legend_title,xtitle, ytitle){
    fig <- plot_ly(df) %>%
      add_lines(
        x=~year,
        y=~attr2,
        frame=NULL,
        color =~factor(continent),
        line=list(simplify=F)) %>%
      layout(
        title=plot_title,
        xaxis=list(title=xtitle),
        yaxis=list(title=ytitle),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
      )
    return(fig)
  }

plot_bubbleChart<- 
  function(df,x,y,color,country,frame, plot_title,xtitle, ytitle,legend_title){
    fig <- plot_ly(df,
                   x = ~x, 
                   y = ~y, 
                   size = ~x, 
                   color = ~color, 
                   frame = ~frame ,
                   text = ~country, 
                   hoverinfo = "text",
                   type = 'scatter',
                   mode = 'markers',
                   fill=~''
                   
    )
    
    fig <- fig %>% layout(
      title = plot_title,
      xaxis = list(title=xtitle,type = "log" ),
      yaxis=list(title=ytitle),
      legend = list(title=list(text=legend_title)),
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
    return(fig)
  }

plot_bar<-function(df,region,y1,y2, plot_title, legend_title,xtitle, ytitle){
  fig <- plot_ly(df)
  fig <- fig %>% add_bars(
    x = ~region,
    y = ~y1,
    base=0,
    marker = list(
      color = '#bcbd22'
    ),
    name = 'Import'
  )
  fig <- fig %>% add_bars(
    x = ~region,
    y = ~y2,
    base = 0,
    marker = list(
      color = '#e377c2'
    ),
    name = 'Export'
  )
  return(fig)
}

plot_globe<- function(df, attr1, attr2)
{
  line <- list(color = toRGB("#d1d1d1"), width = 0.2)
  geo <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic'),
    resolution = '100',
    showcountries = TRUE,
    countrycolor = 'black',
    showocean = TRUE,
    oceancolor = '#429ca6',
    bgcolor = '#30353b',
    landcolor= 'grey',
    showland=T)
  
  plot_geo(text = ~attr2, 
           hoverinfo = "text") %>%
    layout(geo = geo,
           title = "World Regions",
           paper_bgcolor='rgba(0,0,0,0)',
           plot_bgcolor='rgba(0,0,0,0)',
           font = list(color = 'white')) %>%
    add_trace(data = df,
              z = attr1,
              color = ~attr1,
              #colors = 'grey',
              #text = attr3,
              locations = attr2#,
              #marker = list(line = line)
    )
}   


plot_densityChart<- function(df,x,y,z,plot_title,xtitle,legend_title, ytitle){
  fig<- ggplot(df, aes(x = x, fill = factor(y))) + 
    geom_density(alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_log10()+
    transition_time(z) +
    labs(title = plot_title,
         x = xtitle,
         y = ytitle,
         fill = NULL)+
    guides(fill=guide_legend(title=legend_title))+
    theme(panel.grid.major.x = element_blank())+
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background =element_blank())
  
  anim_save("outfile.gif", animate(fig,width=400,height=400,duration = 5, 
                                   fps = 30,bg = 'transparent', renderer = gifski_renderer()))
  list(src = "outfile.gif", contentType = "image/gif")
}



Plot_BoxChart<-function(df,y,color,plot_title,xtitle, ytitle,legend_title){
  fig<- plot_ly(df, y =y, color =color, type = "box")
  fig <- fig %>% layout(
    title = plot_title,
    xaxis = list(title=xtitle),
    yaxis=list(title=ytitle),
    legend = list(title=list(text=legend_title)),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)'
  )
  return(fig)
}

TimeSeriesPlot<-function(df){
  df11 <- df[, c('Year', 'Trade (% of GDP)')]
  names(df11)[2] <- 'Trade_GDP'
  df11 <- df11 %>% drop_na(Trade_GDP)
  df11 <- ddply(df11,"Year",numcolwise(mean))
  
  ################################################
  #Declaring Y as time series data 
  ################################################
  Y <- ts(df11[,2], start=c(1960), end=c(2018), frequency=1)
  
  ################################################
  #Preliminary analysis
  ################################################
  # Time plot 
  autoplot(Y)+ ggtitle("Time Plot :Trade(% of GDP)")+ ylab("Trade")
  
  ################################################
  # Data has trend. Investigate transformation
  ################################################
  DY <- diff(Y)
  
  # Time plot of differenced data
  autoplot(DY)+ ggtitle("Time Plot :Trade(% of GDP)")+ ylab("Trade")
  
  #Fit on ETS model #Residual - 30.9406
  fit_ets <- ets(Y)
  print(summary(fit_ets))
  checkresiduals(fit_ets)
  
  #Fit on Arima model #Residual - 30.05661
  fit_arima <- auto.arima(Y, d=1, stepwise = FALSE,approximation = FALSE, trace = TRUE)
  print(summary(fit_arima))
  ModelPlot<-checkresiduals(fit_arima)
  ModelPlot
  
  #################
  #Forecast with arima model
  ###################
  forst <- forecast(fit_arima, h= 15)
  Modelplot <- autoplot(forst, include = 60) +
    labs(y = "World Trade",
         x = "Years",
         title = "15 Year Forecast of World Trade Using ARIMA Model") +
    theme_bw()
  return(Modelplot)
}

#TimeSeriesPlot(df)
#plot_choropleth()
