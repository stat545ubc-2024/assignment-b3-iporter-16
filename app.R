library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(ggpie)
library(shinythemes)

cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

print(str(cheeses))

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Cheese of the World", "Cheesy!"),
  tags$h4("For cheese-lovers everywhere: choose your milk (or plant-based!) preferences and explore your options."),

  #The sidebar contains a milkInput multiple-selection tool, which allows the user's choice of milk(s)
  #that are then used for the two graphical outputs. It also shows how many cheeses are available for those milks.
  sidebarLayout(
    sidebarPanel(h3("What milk?"),
                 checkboxGroupInput("milkInput", "",
                              choices = c("cow","goat","buffalo","camel","donkey","sheep","yak","plant-based"),
                              selected = c("cow")),
                style = "background-color: skyblue;border: 2px solid black; border-radius: 5px; padding: 10px;",
                textOutput("cheeseCount")),

  #The main panel contains tabs for the cheese colour pie chart, and cheese family bar chart.
  #As a backdrop we are using a free cheese image, sourced below.
    mainPanel(h3("Explore the world of cheese!"),
              tabsetPanel(
                tabPanel("Many colours",plotOutput("pieChart")),
                tabPanel("Many families",plotOutput("barChart"))),

              tags$p("Cheese image source: ",
                     tags$a("Nouri Atchabao",
                            href = "https://www.vecteezy.com/vector-art/94524-free-cheese-seamless-vector",
                            target = "_blank"),
                     style = "font-size: 10px; color: white; margin-top: 15px;"),
              style = "background-image: url('cheese_pic.jpg');
               background-size: cover;
               background-repeat: no-repeat;
               background-position: center;
               border: 2px solid black;
               border-radius: 5px;
               padding: 10px;
               color: black;"),
  ),
)

server <- function(input, output) {
  #Tidy the data by separating each milk and colour, and simplifying these inputs by merging similar colors.
  cheese_long <- cheeses %>%
    separate_rows(milk, sep = ", ") %>%
    separate_rows(color, sep = ", ") %>%
    mutate(color = case_when(
      str_detect(color, "yellow|straw") ~ "yellow",
      str_detect(color, "white|ivory|cream") ~ "white",
      str_detect(color, "orange") ~ "orange",
      str_detect(color, "blue") ~ "blue",
      TRUE ~ color))

  #Create a reactive variable counting the number of cheeses with the milk filter applied, for the cheeseCount text
  filtered_cheese <- reactive({
    cheese_long %>%
      filter(milk %in% input$milkInput)})
  output$cheeseCount <- renderText({
    ifelse(nrow(filtered_cheese())==0,
      paste("There are no cheeses available - try picking more milk types!"),
      paste("There are", nrow(filtered_cheese()), "cheeses available to you. Wow!")
  )
  })

  #Tab 1: a pie chart of cheese colours, faceted by milk(s) selected in the input section above.
  #Additionally factor the cheese colours and assign these levels hex codes so the graph is consistently coded.
  output$pieChart <- renderPlot({
    cheese_long$color <- cheese_long$color %>% factor(levels=c("white","red","orange","yellow","green","blue","brown"))
    cheese_cols <- c("white"="white","red"="darkred","orange"="#ffa600",
                     "yellow"="#f9e02e","green"="#b8d1ae","blue"="#a5d2fb","brown"="#613f00")
    cheese_long %>%
      filter(!is.na(color)) %>%
      filter(milk %in% input$milkInput) %>%
      ggpie(x=color,
            by=milk,
            percent=FALSE,
            nrow=ceiling(length(input$milkInput) / 3),
            facet.label.size = 14) +
      scale_fill_manual(values=cheese_cols)+
      theme(
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold")
      )
  })
  #Tab 2: a bar chart of cheese types (families) faceted by milk selected in the input section above.
  #Bars are automatically ordered and coloured by quantity of options.
  output$barChart <- renderPlot({
    cheese_long %>%
      filter(!is.na(family)) %>%
      filter(milk %in% input$milkInput) %>%
      count(family) %>%
      mutate(family = fct_reorder(family,n,.desc=TRUE)) %>%
      ggplot(aes(x = fct_infreq(family), y = n, fill = n)) +  # Fill by count
      geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
      labs(x = "Cheese family", y = "Number of options") +
      theme_classic() +
      scale_fill_gradient(low = "lightyellow", high = "darkorange") +  # Color gradient from light to dark orange
      scale_color_manual(values="black") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"))+
      scale_y_continuous(expand = c(0,0))
  })
}
shinyApp(ui = ui, server = server)
