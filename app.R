library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(ggpie)
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

print(str(cheeses))

ui <- fluidPage(
  titlePanel("Cheese of the World", "Cheesy!"),

  #In the sidebar we provide a milkInput multiple-selection tool, which allows choices of milk
  #These milk choices are then graphed in the pie chart in the main panel.
  sidebarLayout(
    sidebarPanel(h3("Choose your milk(s)"),
                 selectInput("milkInput", "Milk choices",
                              choices = c("cow","goat","buffalo","camel","donkey","goat","sheep","yak","plant-based"),
                              selected = c("cow","plant-based"),
                              multiple=TRUE)),
    mainPanel(h2("Cheeses of many colours"),
      plotOutput("pieChart")
    ),
  ),

)

server <- function(input, output) {
  #first we tidy the databy separating each milk and colour, and simplifying these inputs. We also
  #filter to remove the NA values for our variables of interest.
  cheese_long <- cheeses %>%
    separate_rows(milk, sep = ", ") %>%
    separate_rows(color, sep = ", ") %>%
    mutate(color = case_when(
      str_detect(color, "yellow|straw") ~ "yellow",
      str_detect(color, "white|ivory|cream") ~ "white",
      str_detect(color, "orange") ~ "orange",
      str_detect(color, "blue") ~ "blue",
      TRUE ~ color)) %>%
    filter(!is.na(color)) %>% filter(!is.na(milk))
  #Here we are creating a pie chart of cheese colours, faceted by milk selected in the input section above.
  #We also facet our cheese colours and assign these levels hex codes so the graph is consistently coded.
  output$pieChart <- renderPlot({
    cheese_long$color <- cheese_long$color %>% factor(levels=c("white","red","orange","yellow","green","blue","brown"))
    cheese_cols <- c("white","darkred","#ffa600","#f9e02e","#b8d1ae","#a5d2fb","#613f00")
    cheese_long%>%
      filter(milk %in% input$milkInput) %>%
      ggpie(x=color,
            by=milk,
            percent=FALSE,
            nrow=ceiling(length(input$milkInput) / 3)) +
      scale_fill_manual(values=cheese_cols)
  })
}
shinyApp(ui = ui, server = server)

# I am building a simple R shiny app. My dataset is called "cheeses" and it includes columns "milk", "country", "texture", "color" and "vegetarian".
# Currently, the "milk" data contains many milks separated by commas. I want to first use the pivot_longer() function to create a separate entry for each milk type. The same is true for the "color" column.
# In the main panel I want a ggplot generated pie chart of the number of cheeses of each color, faceted by milk. I want the pie chart segments to be colored according to the cheese color.

