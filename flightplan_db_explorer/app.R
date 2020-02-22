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
               textInput("to", "Destination:", value = "AGP")),
        column(width = 6,
               textInput("earliest_departure", "Earliest departure time:",
                         value = "10:00"),
        textInput("latest_arrival", "Latest arrival time:",
                         value = "19:00"))),
    column(width = 6,
           h2("Outbound"),
           dataTableOutput("outbound_flights")),
    column(width = 6,
           h2("Inbound"),
           dataTableOutput("inbound_flights"))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filtered_outbound <- reactive({
        segments %>%
            filter(stops == 0,
                   fromCity == input$from,
                   toCity == input$to,
                   lagDays == 0,
                   departure >= as.POSIXct(paste(dummy_date,
                                                 input$earliest_departure)),
                   arrival <= as.POSIXct(paste(dummy_date, 
                                               input$latest_arrival)),
                   departure <= as.POSIXct(paste(dummy_date, 
                                               input$latest_arrival))) %>% 
            mutate(departure = format(departure, "%H:%M"),
                   arrival = format(arrival, "%H:%M")) %>%
            distinct(date, departure, arrival)
    })

    filtered_inbound <- reactive({
        earliest_outbound <- filtered_outbound() %>%
            arrange(date) %>%
            head(1) %>%
            pull(date)
        segments %>%
            filter(stops == 0,
                   fromCity == input$to,
                   toCity == input$from,
                   lagDays == 0,
                   date > !!earliest_outbound,
                   departure >= as.POSIXct(paste(dummy_date,
                                                 input$earliest_departure)),
                   arrival <= as.POSIXct(paste(dummy_date, 
                                               input$latest_arrival)),
                   departure <= as.POSIXct(paste(dummy_date, 
                                                 input$latest_arrival))) %>%
            mutate(departure = format(departure, "%H:%M"),
                   arrival = format(arrival, "%H:%M")) %>%
            distinct(date, departure, arrival)
    })
    
    output$outbound_flights <- renderDataTable({
        DT::datatable(filtered_outbound(),
                      rownames = FALSE)
    })

    output$inbound_flights <- renderDataTable({
        DT::datatable(filtered_inbound(),
                      rownames = FALSE)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
