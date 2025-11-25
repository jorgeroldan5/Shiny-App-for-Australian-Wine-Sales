library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(fable)
library(fabletools)
library(feasts)
library(ggplot2)
library(DT)

wines_raw <- read_csv("AustralianWines.csv", show_col_types = FALSE)

# Parse 'Jan-80' style dates and reshape to a tsibble
wines_ts <- wines_raw %>%
  mutate(
    # "01 Jan-80" -> 1980-01-01
    MonthDate = as.Date(paste("01", Month), format = "%d %b-%y"),
    Month     = yearmonth(MonthDate)
  ) %>%
  # Ensure all sales columns are numeric (Rose was character)
  mutate(
    across(
      -c(Month, MonthDate),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) %>%
  pivot_longer(
    cols      = -c(Month, MonthDate),
    names_to  = "Varietal",
    values_to = "Sales"
  ) %>%
  drop_na(Sales) %>%
  as_tsibble(index = Month, key = Varietal)

all_varietals <- sort(unique(wines_ts$Varietal))
range_month   <- range(wines_ts$Month)
min_date      <- as.Date(range_month[1])
max_date      <- as.Date(range_month[2])

#UI

ui <- fluidPage(
  titlePanel("Australian Wine Sales – Forecast Model Comparison"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "varietal",
        "Choose varietal(s):",
        choices  = all_varietals,
        selected = all_varietals[1:2],
        multiple = TRUE
      ),
      sliderInput(
        "date_range",
        "Filter date range:",
        min   = min_date,
        max   = max_date,
        value = c(min_date, max_date),
        timeFormat = "%Y-%m"
      ),
      dateInput(
        "train_end",
        "Training window ends on:",
        value = max_date - months(12),
        min   = min_date,
        max   = max_date
      ),
      numericInput(
        "h",
        "Forecast horizon (months):",
        value = 12,
        min   = 1,
        step  = 1
      ),
      checkboxInput(
        "show_decomp",
        "Show STL decomposition (per varietal)",
        value = FALSE
      ),
      helpText("Train/validation split is defined by 'Training window ends on'.")
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel(
          "Overview",
          br(),
          plotOutput("overview_plot", height = "400px"),
          conditionalPanel(
            "input.show_decomp == true",
            br(),
            h4("STL decomposition (first selected varietal)"),
            plotOutput("decomp_plot", height = "350px")
          )
        ),
        tabPanel(
          "Models & Metrics",
          br(),
          h4("Model specifications"),
          DTOutput("spec_table"),
          br(),
          h4("Accuracy (training vs validation)"),
          DTOutput("acc_table")
        ),
        tabPanel(
          "Forecasts",
          br(),
          plotOutput("fc_plot", height = "450px")
        ),
        tabPanel(
          "About",
          br(),
          h4("Data"),
          p("Australian monthly wine sales by varietal. The CSV is read and ",
            "converted into a tsibble with index = Month and key = Varietal."),
          h4("Models"),
          tags$ul(
            tags$li("TSLM: Sales ~ trend() + season()"),
            tags$li("ETS: automatic exponential smoothing (ETS(Sales))"),
            tags$li("ARIMA: automatic ARIMA specification (ARIMA(Sales))")
          ),
          h4("Workflow"),
          tags$ol(
            tags$li("Filter data by varietal and date range."),
            tags$li("Split into training (<= split date) and validation (> split date)."),
            tags$li("Fit TSLM, ETS and ARIMA per varietal on training data."),
            tags$li("Score models on training and validation windows (RMSE/MAE/MAPE)."),
            tags$li("Generate forecasts for the chosen horizon with prediction intervals."),
            tags$li("Visualize and compare model forecasts across varietals.")
          ),
          h4("Reproducibility"),
          p("All paths are project-relative and models are specified in ",
            "the server section of app.R.")
        )
      )
    )
  )
)

# SERVER 
server <- function(input, output, session) {

  # Filtered by varietal and date range (using yearmonth)
  filtered_data <- reactive({
    req(input$varietal, input$date_range)

    dr_start <- yearmonth(input$date_range[1])
    dr_end   <- yearmonth(input$date_range[2])

    wines_ts %>%
      filter(
        Varietal %in% input$varietal,
        Month >= dr_start,
        Month <= dr_end
      )
  })

  # Train / validation split
  train_data <- reactive({
    req(filtered_data())

    tr_end <- yearmonth(input$train_end)

    filtered_data() %>%
      filter(Month <= tr_end)
  })

  valid_data <- reactive({
    req(filtered_data())

    tr_end <- yearmonth(input$train_end)

    filtered_data() %>%
      filter(Month > tr_end)
  })

  # Models per varietal
  models <- reactive({
    df_train <- train_data()
    req(nrow(df_train) > 0)

    df_train %>%
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  # Forecasts for user-chosen horizon
  fc_horizon <- reactive({
    m <- models()
    req(m)
    m %>% forecast(h = input$h)
  })

  # Forecasts over validation window for scoring
  fc_valid <- reactive({
    m  <- models()
    vd <- valid_data()
    req(m, nrow(vd) > 0)

    m %>% forecast(new_data = vd)
  })

  # Plots 
  output$overview_plot <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)

    autoplot(df, Sales) +
      labs(
        x = "Month",
        y = "Sales",
        title = "Australian wine sales – filtered data"
      ) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      theme_minimal()
  })

  output$decomp_plot <- renderPlot({
    req(input$show_decomp)

    df <- filtered_data()
    req(df, nrow(df) > 0)

    first_var <- input$varietal[1]
    df_first  <- df %>% filter(Varietal == first_var)

    req(nrow(df_first) > 2 * 12)  # enough data for STL

    df_first %>%
      model(STL = STL(Sales ~ season(window = "periodic"))) %>%
      components() %>%
      autoplot() +
      labs(title = paste("STL decomposition –", first_var)) +
      theme_minimal()
  })

  output$fc_plot <- renderPlot({
    df <- filtered_data()
    fc <- fc_horizon()
    req(df, fc)

    autoplot(fc, df) +
      labs(
        title = "Comparative forecasts with prediction intervals",
        x = "Month",
        y = "Sales"
      ) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      theme_minimal()
  })

  # Model specification table 
  output$spec_table <- renderDT({
    m <- models()
    req(m)

    spec <- m %>%
      glance() %>%
      as_tibble() %>%
      select(
        Varietal,
        .model,
        any_of(c("model", "arima", "order", "seasonal", "AIC", "AICc", "BIC"))
      )

    datatable(
      spec,
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })

  # Accuracy table (train vs validation) 
  output$acc_table <- renderDT({
    m <- models()
    req(m)

    acc_train <- accuracy(m) %>%
      as_tibble() %>%
      mutate(Window = "Training")

    if (nrow(valid_data()) > 0) {
      acc_val <- accuracy(fc_valid(), valid_data()) %>%
        as_tibble() %>%
        mutate(Window = "Validation")
      acc_all <- bind_rows(acc_train, acc_val)
    } else {
      acc_all <- acc_train
    }

    acc_all <- acc_all %>%
      select(Window, Varietal, .model, RMSE, MAE, MAPE, MASE, ACF1)

    datatable(
      acc_all,
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui, server)
