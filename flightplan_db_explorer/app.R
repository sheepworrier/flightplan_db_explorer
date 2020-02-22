library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(DT)

options(stringsAsFactors = FALSE)
dummy_date <- "2017-01-01"
# awards <- read_csv("https://www.dropbox.com/s/ystsm6seqhdyiho/awards.csv?dl=1")
# requests <-
#     read_csv("https://www.dropbox.com/s/wrjxhkfz4r4ho2u/requests.csv?dl=1")
segments <-
    read_csv("https://www.dropbox.com/s/ylru7m7yun2jzkm/segments.csv?dl=1",
             col_types = "dddcccccDccddcddcT") %>%
    mutate(departure = as.POSIXct(paste(dummy_date, departure)),
           arrival = as.POSIXct(paste(dummy_date, arrival)))
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FlightPlan Results Explorer"),
    fluidRow(
        column(width = 6,
               textInput("from", "Starting point:", value = "LGW"),
               textInput("to", "Destination:", value = "AGP"),
               textInput("earliest_departure", "Earliest departure time:",
                         value = "10:00")),
        column(width = 6,
               textInput("sleep_start", "Start of sleep:", value = "12:00"),
               textInput("sleep_end", "End of sleep:", value = "14:00"),
               textInput("latest_arrival", "Latest arrival time:",
                         value = "18:00"))),
    column(width = 6,
           dataTableOutput("outbound_flights")),
    column(width = 6,
           dataTableOutput("inbound_flights"))

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$outbound_flights <- renderDataTable({
        filtered <- segments %>%
            filter(stops == 0,
                   fromCity == input$from,
                   toCity == input$to,
                   lagDays == 0,
                   departure >= as.POSIXct(paste(dummy_date,
                                                 input$earliest_departure)),
                   arrival <= as.POSIXct(paste(dummy_date, 
                                               input$latest_arrival)),
                   (departure <= 
                        as.POSIXct(paste(dummy_date, input$sleep_start)) &
                        arrival <=
                            as.POSIXct(paste(dummy_date, input$sleep_end))) |
                       (departure >=
                            as.POSIXct(paste(dummy_date, input$sleep_start)) &
                            arrival >=
                                as.POSIXct(paste(dummy_date,
                                                 input$sleep_end)))) %>%
            mutate(departure = format(departure, "%H:%M"),
                   arrival = format(arrival, "%H:%M")) %>%
            select(date, departure, arrival)
        DT::datatable(filtered,
                      rownames = FALSE)
    })

    output$inbound_flights <- renderDataTable({
        filtered <- segments %>%
            filter(stops == 0,
                   fromCity == input$to,
                   toCity == input$from,
                   lagDays == 0,
                   departure >= as.POSIXct(paste(dummy_date,
                                                 input$earliest_departure)),
                   arrival <= as.POSIXct(paste(dummy_date, 
                                               input$latest_arrival)),
                   (departure <= 
                        as.POSIXct(paste(dummy_date, input$sleep_start)) &
                        arrival <=
                        as.POSIXct(paste(dummy_date, input$sleep_end))) |
                       (departure >=
                            as.POSIXct(paste(dummy_date, input$sleep_start)) &
                            arrival >=
                            as.POSIXct(paste(dummy_date,
                                             input$sleep_end)))) %>%
            mutate(departure = format(departure, "%H:%M"),
                   arrival = format(arrival, "%H:%M")) %>%
            select(date, departure, arrival)
        DT::datatable(filtered,
                      rownames = FALSE)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
