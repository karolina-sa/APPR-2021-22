library(shiny)
library(shinyWidgets)

shinyUI(
  fluidPage(
    h4("Število prenočitev v Sloveniji v izbranem mesecu:"),
    fluidRow(
      column(
        width = 4,
        radioButtons(
          "drzava", 
          label = "Tip turistov:",
          choices = list("Tuji" = "Tuji", 
                         "Domači" = "Domači",
                         "Skupaj" = "Skupaj"),
          selected = "Skupaj"),
        airDatepickerInput("mesec",
                           label = "Izbira meseca:",
                           value = "2022-02-01",
                           maxDate = "2022-02-01",
                           minDate = "2019-01-01",
                           view = "months", #editing what the popup calendar shows when it opens
                           minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                           dateFormat = "yyyy-mm"
        )
      ),
      column(
        width = 8,
        plotOutput("graf")
      )
    )
  )
)
