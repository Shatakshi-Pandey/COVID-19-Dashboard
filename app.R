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
              "United Arab Emirates", "Argentina", "Armenia", "American Samoa", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan",
              "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", "Bahrain", "Bahamas", "Bosnia and Herzegovina", "Belarus",
              "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei", "Bhutan", "Botswana", "Costa Atlantica", "Central African Republic", "Canada", "Switzerland",
              "Chile", "China", "Cote d'Ivoire", "Cameroon", "Congo, the Democratic Republic of the", "Congo", "Colombia", "Comoros", "Cape Verde", "Costa Rica",
              "Cuba", "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", "Diamond Princess", "Algeria",
              "Ecuador", "Egypt", "Eritrea", "Spain", "Estonia", "Ethiopia", "Finland", "Fiji", "France", "Gabon", "United Kingdom", "Georgia", "Ghana", "Guinea",
              "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Grand Princess", "Greece", "Grenada", "Guatemala", "Guam", "Guyana", "Honduras", "Croatia",
              "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", "Kyrgyzstan",
              "Cambodia", "Saint Kitts and Nevis", "Korea, South", "Kuwait", "Laos", "Lebanon", "Liberia", "Libya", "Saint Lucia", "Liechtenstein", "Sri Lanka", "Lesotho",
              "Lithuania", "Luxembourg", "Latvia", "Morocco", "Monaco", "Moldova", "Madagascar", "Maldives", "Mexico", "Marshall Islands", "North Macedonia", "Mali", "Malta",
              "Myanmar", "Montenegro", "Mongolia", "Northern Mariana Islands", "Mozambique", "Mauritania", "MS Zaandam", "Mauritius", "Malawi", "Malaysia", "Namibia", "Niger",
              "Nigeria", "Nicaragua", "Netherlands", "Norway", "Nepal", "New Zealand", "Oman", "Pakistan", "Panama", "Peru", "Philippines", "Papua New Guinea", "Poland",
              "Puerto Rico", "Portugal", "Paraguay", "Palestine", "Qatar", "Kosovo", "Romania", "Russia", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore", "Solomon Islands",
              "Sierra Leone", "El Salvador", "San Marino", "Somalia", "Serbia", "South Sudan", "Sao Tome and Principe", "Suriname", "Slovakia", "Slovenia",
              "Sweden", "Swaziland", "Seychelles", "Syria", "Chad", "Togo", "Thailand", "Tajikistan", "Timor-Leste", "Trinidad and Tobago", "Tunisia", "Turkey",
              "Taiwan", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", "Uzbekistan", "Holy See", "Saint Vincent and the Grenadines", "Venezuela",
              "Virgin Islands, U.S.", "Vietnam", "Vanuatu", "Samoa", "Yemen", "South Africa", "Zambia", "Zimbabwe"
            )),
          ),
          fluidPage(
            tabsetPanel(
              type = "tab",
              tabPanel("Plot", plotlyOutput("lplot")),
              tabPanel("Data", DTOutput("Data"))
            )
          )
        ),
        tabItem(
          tabName = "viz",
          fluidRow(
            plotlyOutput("plot")
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

  output$plot <- renderPlotly({
    ggplot(data = Data, aes(x = date, y = confirmed)) +
      geom_line(colour = "grey", aes(date, deaths)) +
      geom_line(colour = "#408FA6")
  })
  output$lplot <- renderPlotly({
    fbar <- droplevels(Data[which(Data$administrative_area_level_1 == input$country), ])
    ggplot(data = fbar) +
      geom_line(colour = "#FF2400", aes(date, deaths)) +
      geom_line(colour = "#2f4f4f", aes(x = date, y = confirmed)) +
      geom_line(colour = "#408FA6", aes(x = date, y = recovered)) +
      labs(x = "Date", y = "Recovered\nConfirmed\nDeaths") +
      theme_bw()
  })
}

# Application
shinyApp(ui = ui, server = server)