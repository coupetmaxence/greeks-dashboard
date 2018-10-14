#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(plotly)

# Load functions for custom UI elements
source('utils.R')

# Load functions from premium and greek computing
source('pricing.R')



# Define UI for application that draws a histogram
ui <- material_page(
  title = "Options premiums and greeks calculations",
  nav_bar_color = "teal",
  nav_bar_fixed = TRUE,
  material_side_nav(
    fixed = FALSE,
    #image_source = "material_wp.jpg",
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Vanilla options" = "vanilla_side_nav_tab",
        "Combinaison of options" = "combinaison_side_nav_tab"
      )
      #icons = c("cast", "insert_chart")
    )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "vanilla_side_nav_tab",
    tags$br(),
    material_row(
      material_column(
        width = 3,
        material_card(
          title = 'Option informations',
          depth = 4,
          material_row(
            material_column(
              width = 12,
              material_dropdown(
                input_id = "opt_type",
                label = "Option Type",
                choices = c(
                  "Call" = "c",
                  "Put" = "p"
                ),
                selected = "c"
              ) 
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "K",
                label = "Strike price",
                min_value = 1,
                max_value = 200,
                initial_value = 50,
                step = 0.5)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "Sigma",
                label = "Volatility",
                min_value = 0.01,
                max_value = 1,
                initial_value = 0.2,
                step = 0.05)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "Rate",
                label = "Interest rate",
                min_value = 0.01,
                max_value = 1,
                initial_value = 0.05,
                step = 0.05)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "Dividend",
                label = "Dividend rate",
                min_value = 0,
                max_value = 1,
                initial_value = 0,
                step = 0.05)
            )
            
            
            
          )
          
        ),
        material_card(
          title = 'Plotting parameters',
          depth = 4,
          material_row(
            material_column(
              width = 6,
              material_number_box(
                input_id = "S0_range_min",
                label = "Range for S0",
                min_value = 1,
                max_value = 200,
                initial_value = 30,
                step = 1)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "S0_range_max",
                label = " ",
                min_value = 1,
                max_value = 200,
                initial_value = 60,
                step = 1)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "T_range_min",
                label = "Range for T",
                min_value = 0.01,
                max_value = 5,
                initial_value = 0.1,
                step = 0.1)
            ),
            material_column(
              width = 6,
              material_number_box(
                input_id = "T_range_max",
                label = "",
                min_value = 0.01,
                max_value = 5,
                initial_value = 2,
                step = 0.1)
            )
          )
          
        )
      ),
      material_column(
        width = 9,
        material_card(
          title = "Option premium",
          depth = 4,
          plotlyOutput("premium", height = '40.5em')#'36.7em')
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Delta",
          depth = 4,
          plotlyOutput("delta")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Gamma",
          depth = 4,
          plotlyOutput("gamma")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Vega",
          depth = 4,
          plotlyOutput("vega")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Theta",
          depth = 4,
          plotlyOutput("theta")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Rho",
          depth = 4,
          plotlyOutput("rho")
        )
      )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  nbr_values <- 30
  # renderPlotly() also understands ggplot2 objects!
  get_K <- reactive({
    input$K
  })
  
  get_Div <- reactive({
    input$Dividend
  })
  
  get_prices <- reactive({
    c(seq(from = input$S0_range_min,
          to = input$S0_range_max, 
          length.out = nbr_values))
  })
  
  get_type <- reactive({
    input$opt_type
  })
  
  # Making sure that the min and max values are not inverted for spot plotting range
  observe({
    s_min <- input$S0_range_min
    s_max <- input$S0_range_max
    if(s_min > s_max)
    {
      input_id_1 <- 'S0_range_max'
      input_id_2 <- 'S0_range_min'
      value <- s_min
      session$sendCustomMessage(
        type = "shinymaterialJS",
        paste0(
          "$('#", input_id_1, "').val(", s_min, ");Shiny.onInputChange('", input_id_1, "', ", s_min, ");"
        )
      )
      
      session$sendCustomMessage(
        type = "shinymaterialJS",
        paste0(
          "$('#", input_id_2, "').val(", s_max, ");Shiny.onInputChange('", input_id_2, "', ", s_max, ");"
        )
      )
    }
  })
  
  # Making sure that the min and max values are not inverted for spot plotting maturity
  observe({
    t_min <- input$T_range_min
    t_max <- input$T_range_max
    if(t_min > t_max)
    {
      input_id_1 <- 'T_range_max'
      input_id_2 <- 'T_range_min'
      session$sendCustomMessage(
        type = "shinymaterialJS",
        paste0(
          "$('#", input_id_1, "').val(", t_min, ");Shiny.onInputChange('", input_id_1, "', ", t_min, ");"
        )
      )
      
      session$sendCustomMessage(
        type = "shinymaterialJS",
        paste0(
          "$('#", input_id_2, "').val(", t_max, ");Shiny.onInputChange('", input_id_2, "', ", t_max, ");"
        )
      )
    }
  })
  
  get_T <- reactive({
    c(seq(from = input$T_range_min,
          to = input$T_range_max, 
          length.out = nbr_values))
  })
  
  get_Sigma <- reactive({
    input$Sigma
  })
  
  get_r <- reactive({
    input$Rate
  })
  
  output$delta <- renderPlotly({
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, Delta, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Delta"
    )
    p<- plot_ly(x=prices, y=times, 
            z=premiums
            , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  output$gamma <- renderPlotly({
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, Gamma, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Gamma"
    )
    p<- plot_ly(x=prices, y=times, 
                z=premiums
                , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  output$vega <- renderPlotly({
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, Vega, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Vega"
    )
    p<- plot_ly(x=prices, y=times, 
                z=premiums
                , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  output$theta <- renderPlotly({
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, Theta, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Theta"
    )
    p<- plot_ly(x=prices, y=times, 
                z=premiums
                , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  output$rho <- renderPlotly({
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, Rho, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Rho"
    )
    p<- plot_ly(x=prices, y=times, 
                z=premiums
                , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  output$premium <- renderPlotly({
    
    
    prices <- get_prices()
    times <- get_T()
    
    combinaison <- expand.grid(times, prices)
    somme <- apply(combinaison, 1, BS_Price, K=get_K(), Sigma=get_Sigma(), r=get_r(), div = get_Div(), opt_type = get_type())
    premiums <- matrix(somme, nrow=nbr_values, ncol=nbr_values)
    
    axx <- list(
      title = "S0"
    )
    
    axy <- list(
      title = "T"
    )
    
    axz <- list(
      title = "Premium"
    )
    p<- plot_ly(x=prices, y=times, 
                z=premiums
                , type = 'surface') %>%
      layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                          cameraposition=list(list(-0.1, 0.5, -0.7, -0.2),
                                              list(0.0, 0, 0.0),
                                              2.8)))
    p$elementId <- NULL
    p
  })
  
  

  
}

shinyApp(ui, server)

