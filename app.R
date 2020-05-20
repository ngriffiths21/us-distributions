library(shiny)
library(tidyverse)

ui <- fluidPage(
  h1("US Population distribution by age"),
  radioButtons("age_groups", label = "Age groups", choices = list(`Five years` = 5, `Ten years` = 10)),
  radioButtons("adults", label = "Population", choices = list(`All` = "all", `Adults only` = "adults")),
  tableOutput("data"),
  p("Population of the United States in 2019. Source: US Census, American Community Survey.")
)

server <- function(input, output, session) {
  census_data <- read_csv("2019census_gender_age.csv", col_types = "ciii")

  census_data10 <- 
    census_data %>%
    mutate(group = cumsum(rep_along(Age, 1:0))) %>%
    group_by(group) %>%
    summarize(All = sum(All), Male = sum(Male), Female = sum(Female)) %>%
    mutate(group = c("Less than 10", "11 to 20 years", "21 to 30 years",
                     "31 to 40 years", "41 to 50 years", "51 to 60 years",
                     "61 to 70 years", "71 to 80 years", "80 years and over")) %>%
    rename(Age = group) %>%
    mutate(All = scales::percent(All / sum(All), .1)) %>%
    mutate(Male = scales::percent(Male / sum(Male), .1)) %>%
    mutate(Female = scales::percent(Female / sum(Female), .1)) %>%
    mutate(adult = c(rep(FALSE, 2), rep(TRUE, 7)))

  census_data5 <-
    census_data %>%
    mutate(All = scales::percent(All / sum(All), .1)) %>%
    mutate(Male = scales::percent(Male / sum(Male), .1)) %>%
    mutate(Female = scales::percent(Female / sum(Female), .1)) %>%
    mutate(adult = c(rep(FALSE, 4), rep(TRUE, 14)))
  
  age_groups <- reactive({
    if(input$age_groups == 5) {
      census_data5
    } else if (input$age_groups == 10) {
      census_data10
    }
  })

  populations <- reactive({
    if(input$adults == "adults") {
      select(filter(age_groups(), adult), -adult)
    } else {
      select(age_groups(), -adult)
    }
  })

  output$data <- renderTable({
    populations()
  })
}

shinyApp(ui, server)
