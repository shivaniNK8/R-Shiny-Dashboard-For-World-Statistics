server <- function(input, output, session) {
  # DYNAMIC RENDER RULES ----------------------------------------------------
  df<- read_csv("www/final_csv.csv")

  observeEvent("", {
    show("social_panel")
    output$pop1<-renderImage({
      return(list(
        src = "www/stats.png",
        contentType = "image/png",
        height=180, width = 250
      ))
    }, deleteFile = TRUE)
    
    globe_data <- df %>% filter(Year == 2007)
    globe_data$region_code<- ifelse(globe_data$region=='Asia',1,
                                    ifelse(globe_data$region=='Europe',
                                           5,
                                                                       
                                           ifelse(globe_data$region=='Americas',3,ifelse(globe_data$region=='Ocenia',4,2))))
    
    output$globe <-renderPlotly({
      plot_globe(globe_data , globe_data$region_code,globe_data$CODE)
    })
    output$stat1 <- renderText("Stat 1")
    
    df1 <- df %>% 
      group_by(region, Year) %>% 
      summarise(mean_pop = mean(`Unemployment, total (% of total labor force) (national estimate)`, na.rm = TRUE)) 
    pop_df <- df%>% 
      filter(!is.na(region))
    
    output$box_pat2 <- renderPlotly({
      plot_bubbleChart(
        pop_df,log(pop_df$`GDP per capita (current US$)`),
        pop_df$lifeExp,
        pop_df$region,
        pop_df$Country,
        pop_df$Year,
      "GDP per Capita vs Life Expectancy",
      "GDP",
      "Life Expectancy",
      "Region")
      })
    
    output$box_pat <- renderImage({
      df$Year <- as.integer(df$Year) 
      
      df2 <- df %>%  
        select(Country, `pop`, Year, region) %>%  
        group_by(Year)  %>% arrange(Year, -`pop`)  %>%  
        dplyr::mutate(rank = 1:n()) %>%  
        filter(rank <= 10) -> ranked_by_year
      
      my_theme <- theme_classic(base_family = "Times") +
        theme(axis.text.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.line.y = element_blank()) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.background =element_blank())
      
      
      ranked_by_year %>%  
        ggplot() +  
        aes(xmin = 0 ,
            xmax = `pop` / 1000000)+  
        aes(ymin = rank - .45,
            ymax = rank + .45,
            y = rank)+  
        facet_wrap(~Year)+geom_rect(alpha = .7) +  
        aes(fill = region) +  
        scale_fill_viridis_d(option = "magma",  
                             direction = -1) +  
        
        geom_text(col = "gray13",
                  hjust = "right",
                  aes(label = Country),
                  x = -50)+ scale_y_reverse() +  
        labs(fill = NULL) +  
        labs(x = 'Population (millions)') +  
        labs(y = "") +  
        my_theme -> my_plot
      
      my_plot<-my_plot + facet_null() +  
        geom_text(x = 1000 , y = -10,  
                  family = "Times",  
                  aes(label = as.character(Year)),  
                  size = 30, col = "grey18") +  
        aes(group = Country) +  
        transition_time(Year)
      
      anim_save("outfileLine.gif", animate(my_plot,duration = 5, width = 400, height = 400,
                                           fps = 25,bg = 'transparent', renderer = gifski_renderer()))
      list(src = "outfileLine.gif", contentType = "image/gif")
      
    },deleteFile = TRUE)
    

    output$box_year <- renderPlotly({
      plot_ly(df1)%>%
        add_lines(
          x=df1$Year,
          y=df1$mean_pop,
          color =factor(df1$region),
          line=list(simplify=F)) %>%
        layout(
          title="Percentage Unemployment of Total Labor Force Over Years",
          xaxis=list(title="Year"),
          yaxis=list(title="Unemployment Percentage"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          legend = list(title=list(text="Region"))
        )
      })
    
    #########
    
    ##########
    output$box1 <- renderPlotly({plot_choropleth(df,
                                                 log(df$`GDP per capita (current US$)`),
                                                 "GDP Per Capita in US$","Log GDP")})
    output$box2 <- renderImage({
      plot_densityChart(df,df$`Mortality rate, infant (per 1,000 live births)`,
                        df$region,df$Year,
                         "Infant Mortality Rate Over Years","Mortality Rate","","")
    },deleteFile = TRUE)
      
        
    output$box3 <- renderPlotly({

      imp_df <- df %>% 
                group_by(region) %>% 
                summarise(imp_mean = mean(imports, na.rm = TRUE),
                          exp_mean = mean(exports, na.rm = TRUE))
      plot_ly(imp_df) %>%
        add_trace(x= ~region,y= ~imp_mean,  name = 'Imports',type= 'bar', color = "#a6e2d1", opacity = 0.7) %>%
        add_trace(x= ~region,y = ~exp_mean, name = 'Exports',type= 'bar', color = "#ffc4ac", opacity = 0.7) %>%
        layout(
          title="Imports and Exports of Countries",
          xaxis=list(title="Region"),
          yaxis=list(title="Percentage of GDP"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
      })
    
    #########
    Green_house<-df %>% 
      filter(Year %in% (2018)) %>%
      group_by(Country) %>%
      summarise(Green_house=mean(`Total greenhouse gas emissions (kt of CO2 equivalent)`,na.rm=T))%>%
      arrange(desc(Green_house))%>%
      slice(1:10)
    co2_df <- df %>%
      filter(Year %in% (2000:2020) & Country %in% (Green_house$Country)) %>%
      select(Country,Year,
             `CO2 emissions from gaseous fuel consumption (% of total)`,
             `CO2 emissions from liquid fuel consumption (% of total)`,
             `CO2 emissions from solid fuel consumption (% of total)`,
             `Renewable energy consumption (% of total final energy consumption)`) %>%
      group_by(Country) %>%
      summarise(co2_gas = mean(`CO2 emissions from gaseous fuel consumption (% of total)`,na.rm=T),
                co2_liquid=mean(`CO2 emissions from liquid fuel consumption (% of total)`,na.rm=T),
                co2_solid=mean(`CO2 emissions from solid fuel consumption (% of total)`,na.rm=T),
                Renewable_Energy=mean(`Renewable energy consumption (% of total final energy consumption)`,na.rm=T)) 
    
    tob_alc <- df %>% 
      filter(Year>=2010) %>% 
      select(Country, region, alcohol_consumption, tobacco_consumption) %>% 
      group_by(Country) %>%
      summarise(mean_alc = mean(alcohol_consumption, na.rm = TRUE))
    countries <- c("United Kingdom", "United States", "France", "India", "Germany",
                   "China", "Ireland", "United Arab Emirates","Costa Rica", 
                   "Korea, Dem. People's Rep.", "Kuwait", "Saudi Arabia")
    tob_alc$alc_z <- round((tob_alc$mean_alc - 
                              mean(tob_alc$mean_alc,  na.rm = TRUE))/sd(tob_alc$mean_alc, na.rm = TRUE), 2) # compute normalized mpg 
    tob_alc$alc_type <- ifelse(tob_alc$alc_z < 0, "below", "above") # above / below avg flag
    tob_alc <- tob_alc[order(tob_alc$alc_z), ] # sort
    tob_alc$Country <- factor(tob_alc$Country , levels = tob_alc$Country ) # convert to factor to retain sor
    
    tob_alc <- tob_alc %>% 
      filter(Country %in% countries)
    colors_3 <- c("#ffc4ac", "#cbeda1", "#fcc1e3")
    #plots
    output$box5 <- renderPlotly({ 
      plot_ly(co2_df) %>%
        add_trace(x= ~Country,y= ~co2_gas,  name = 'Gas',type= 'bar', color = "#fcc1e3", opacity = 0.7) %>%
        add_trace(x= ~Country,y = ~co2_liquid, name = 'Liquid',type= 'bar', color = "#ffc4ac", opacity = 0.7) %>%
        add_trace(x= ~Country,y = ~co2_solid, name = 'Solid',type= 'bar', color = "#cbeda1", opacity = 0.7) %>% 
        add_trace(x= ~Country,y=~Renewable_Energy,name='Renewable_Energy',type='scatter',mode="lines") %>%
        layout(
          title="CO2 Emission of Countries",
          xaxis=list(title="Country"),
          yaxis=list(title="Emission Percentage"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
      })
    cols <- c("#e76682","#7b98ee")
    output$box7 <- renderPlotly({
      ggplotly(
        ggplot(tob_alc, aes(x=Country, y=alc_z, label=alc_z)) +
        geom_bar(stat='identity', aes(fill=alc_type), width=.5)  +
        scale_fill_manual(name="Consumption",
                          labels = c("Above Average", "Below Average"),
                          values = cols) +
        labs(y="Normalised Alcohol Consumption",
             x = "Country",
             title = "Alcohol Consumption of Countries") +
        coord_flip()+
        theme_bw()
      )
    })
    output$box6 <- renderPlotly({
      box_df <- df %>% filter(!is.na(region))
      Plot_BoxChart(box_df,
                    box_df$`Poverty headcount ratio at national poverty lines (% of population)`,
                    box_df$ region,"Poverty Ratio",
                  "Continents","Poverty Headcount","Continents")
      })
    
    ##########
    output$box_los1 <- renderPlot({TimeSeriesPlot(df)})
    # output$box_los2 <- renderPlotly({})
    # output$box_los3 <- renderPlotly({})
    # output$box_los4 <- renderPlotly({})
    ##########
    hide("economic_panel")
    hide("environmental_panel")
    hide("modeling_panel")
  }, once = TRUE)

  observeEvent(input$social, {
    show("social_panel")
    hide("environmental_panel")
    hide("economic_panel")
    hide("modeling_panel")
  })
  observeEvent(input$economic, {
    show("economic_panel")
    hide("environmental_panel")
    hide("modeling_panel")
    hide("social_panel")
  })
  observeEvent(input$environmental, {
    show("environmental_panel")
    hide("economic_panel")
    hide("modeling_panel")
    hide("social_panel")
  })
  observeEvent(input$modeling, {
    show("modeling_panel")
    hide("environmental_panel")
    hide("economic_panel")
    hide("social_panel")
  })

  observeEvent(input$Range,{
    #print(input$Range[1], input$Range[2])
  })

  # show active button with color

  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Patients") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "antimicrobials", style = {
      if (x == "Antimicrobial consumption") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })

  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }

  observeEvent(input$patients, {
    update_all("Patients")
  })
  observeEvent(input$antimicrobials, {
    update_all("Antimicrobial consumption")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })

}