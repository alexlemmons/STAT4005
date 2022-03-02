## “All work presented is my own, and I have followed all rules for collaboration.” - Alex Emmons

library(tidyverse)
library(plotly)



pokemon_df <- read_csv("data/pokemon_full.csv")

features <- c('Attack', 'Defense', 'SpAtk', 'SpDef', 'HP', 'Speed')
names(features) <- c('Attack', 'Defense', 'Special Attack', 'Special Defense', 'Health Points', 'Speed')
feature_df <- data.frame(feature_name=names(features), stringsAsFactors = F)
rownames(feature_df) <- features

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "poke_gen",
                      'Generation',
                      choices=unique(pokemon_df$Generation),
                      selected=1),
      uiOutput('pokemon_ui')
      ),
      mainPanel(
          div(plotlyOutput('bar_comp'))
          )))

server <- function(input, output, session) {
  

  output$pokemon_ui <- renderUI({
    choices <- pokemon_df[which(pokemon_df$Generation == input$poke_gen),'Name']
    selectInput("pokemon_name",
                "Choose a Pokemon:",
                choices = choices)
  })
  

  selected_pokemon <- reactive({
    poke_selection <- pokemon_df[which(pokemon_df$Name == input$pokemon_name),]
  })
  
  output$bar_comp <- renderPlotly({
    req(input$pokemon_name)
    stat_names <- names(features)[1:6]
    
    df <- selected_pokemon() %>%
      select(Attack, Defense, SpAtk, SpDef, HP, Speed) %>%
      `colnames<-` (stat_names) %>%
      gather("Stat", "Value") %>%
      mutate(side = 'Pokemon')
    
   is_generation <- selected_pokemon()$Generation
   poke_filter <- filter(pokemon_df, Generation== is_generation)
    
   as.numeric(poke_filter$Attack)
   as.numeric(poke_filter$Defense)
   as.numeric(poke_filter$SpAtk)
   as.numeric(poke_filter$SpDef)
   as.numeric(poke_filter$HP)
   as.numeric(poke_filter$Speed)
    
    df_avg <- poke_filter %>%
      select(Attack, Defense, SpAtk, SpDef, HP, Speed) %>%
      summarise_all(list(mean)) %>%
      summarise_all(list(round)) %>%
      `colnames<-`(stat_names) %>%
      gather("Stat", "Value") %>%
      mutate(side = 'Average') %>%
      mutate(Value = -Value) 
    
    
    df_full <- rbind(df,df_avg)
    
    
    
    plot <- df_full %>% 
      ggplot(aes(x = Stat, y = Value, group = side, fill = side,
                 text = paste0(ifelse(side=='Average', 'Group Average', input$pokemon_name),
                               '<br>', Stat, ': ', abs(Value))) +
      geom_bar(stat = "identity", width = 0.75) +
      coord_flip() +
      labs(x = "", y = "") +
      ggtitle(paste0("Comparison of ", input$pokemon_name, " Stats Against Average")))
    
    ggplotly(plot, tooltip = c("text")) %>%
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
  })
}
  
shinyApp(ui, server)
  
## This app is supposed to show you the base stats of a chosen pokemon against the average
## of all other pokemon on a plotly butterfly chart and it was working until I tried to 
## make the average change based on the generation chosen and now theres some fatal error
## that I cannot figure out.
