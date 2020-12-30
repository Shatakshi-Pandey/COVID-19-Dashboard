# Loading libraries
library(shiny)
library(shinydashboard)
library(COVID19)
library(DT)
library(plotly)

# Loading data
Data <- covid19()

# Manipulating data
`Cumulative to Individual` <- function(x) {
  if (length(x) > 1) {
    abs(x[2:length(x)] - x[1:(length(x) - 1)])
  } else {
    cat("Vector length not sufficient!\n")
  }
}
`Manipulated Data` <- rbind(Data[1, c(3:9, 11:21)], apply(Data[, c(3:9, 11:21)], 2, `Cumulative to Individual`))
`Manipulated Data` <- cbind(Data[, c(1:2)], `Manipulated Data`, Data[, c(10, 22:35)])
for (i in 1:length(unique(`Manipulated Data`$id)))
{
  `Manipulated Data`[head(which(`Manipulated Data`$id == unique(`Manipulated Data`$id)[i]), 1), ] <- Data[head(which(`Manipulated Data`$id == unique(`Manipulated Data`$id)[i]), 1), c(1, 2, 3:9, 11:21, 10, 22:35)]
}
Data <- `Manipulated Data`
Data$month <- format(Data$date, "%m")

# User Interface
ui <- shinyUI(
  dashboardPage(
    title = "Covid19Tracker", skin = "blue",
    dashboardHeader(
      title = "Covid19Tracker", dropdownMenuOutput("msgOutput")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Visualization", tabName = "viz", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "dashboard",
          fluidRow(
            valueBox(sum(`Manipulated Data`$recovered), "Recovered", icon = icon("user-shield"), color = "green"),
            valueBox(sum(`Manipulated Data`$confirmed), "Confirmed Cases", icon = icon("check-circle"), color = "yellow"),
            valueBox(sum(`Manipulated Data`$deaths), "No. of Deaths", icon = icon("skull-crossbones"), color = "red")
          ),
          fluidRow(
            selectInput("country", "Select the country", choices = c(
              "Afghanistan", "Angola", "Albania", "Egypt", "Andorra",
              "United Arab Emirates", "Argentina"
            )),
          ),
          fluidPage(
            tabsetPanel(
              type = "tab",
              tabPanel("Data", DTOutput("Data")),
              tabPanel("Plot", plotOutput("lplot"))
            )
          )
        ),
        tabItem(
          tabName = "viz",
          fluidRow(
            plotOutput("plot")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$Data <- {
    (
      renderDT(
        countryFilter <- subset(Data, Data$administrative_area_level_1 == input$country, select = c(id, date, recovered, confirmed, deaths, hosp))
      )
    )
  }

  output$plot <- renderPlot({
    barplot(`Manipulated Data`$confirmed,
      names.arg = `Manipulated Data`$id, xlab = "Confirmed Cases", ylab = "Country Id", col = "blue",
      main = "Bar Plot", border = "red"
    )
  })
  output$lplot <- renderPlot({
    fbar <- droplevels(Data[which(Data$administrative_area_level_1 == input$country), ])
    plot(fbar$month, fbar$confirmed,
      main = input$country,
      ylab = "Confirmed",
      xlab = "Month"
    )
  })
}

# Application
shinyApp(ui = ui, server = server)