library(shiny)
library(bslib)
library(fpp3)
library(gt)
library(tidyverse)
library(here)
library(urca)

# Data preparation
aus_wine <- read_csv(here::here("AustralianWines.csv"), na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |> 
    fill(Rose, .direction = "down") |> 
    mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())

varietals <- names(aus_wine)[-1]
max_date <- max(aus_wine$Month)
default_train_cutoff <- max_date - 12

# UI
ui <- page_navbar(
    title = "Australian Wine Sales Forecasting",
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    # Tab 1: Visualization
    nav_panel(
        title = "Visualization",
        layout_sidebar(
            sidebar = sidebar(
                title = "Data Selection",
                checkboxGroupInput("varietals", "Select Varietals",
                                  choices = varietals,
                                  selected = varietals),
                dateInput("train_cutoff", "Training Cutoff Date",
                         value = as_date(default_train_cutoff),
                         min = as_date(min(aus_wine$Month)),
                         max = as_date(max(aus_wine$Month))),
                dateInput("forecast_end", "Forecast End Date",
                         value = as_date(max_date),
                         min = as_date(min(aus_wine$Month)))
            ),
            plotOutput("viz_plot", height = "600px")
        )
    ),
    
    # Tab 2: Model Building
    nav_panel(
        title = "Model Building",
        layout_sidebar(
            sidebar = sidebar(
                title = "Model Selection",
                checkboxGroupInput("training_models", "Select Models",
                                  choices = c("tslm", "ets", "arima"),
                                  selected = c("tslm", "ets", "arima"))
            ),
            layout_columns(
                col_widths = c(6, 6),
                card(
                    card_header("Training Accuracy"),
                    gt_output("training_accuracy_table")
                ),
                card(
                    card_header("Model Specification"),
                    verbatimTextOutput("model_specification")
                )
            )
        )
    ),
    
    # Tab 3: Forecast
    nav_panel(
        title = "Forecast",
        layout_sidebar(
            sidebar = sidebar(
                title = "Forecast Options",
                checkboxGroupInput("forecast_models", "Select Models",
                                choices = c("tslm", "ets", "arima"),
                                selected = c("tslm", "ets", "arima"))
            ),
            layout_columns(
                col_widths = c(6, 6),
                card(
                    card_header("Forecast Accuracy"),
                    gt_output("forecast_accuracy_table")
                ),
                card(
                    card_header("Forecast Visualization"),
                    plotOutput("forecast_plot", height = "600px")
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    
    # Reactive: filtered data
    filtered_data <- reactive({
        req(input$varietals)
        
        aus_wine |> 
            select(Month, all_of(input$varietals)) |> 
            pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
            as_tsibble(index = Month, key = Varietal)
    })
    
    # Reactive: training data
    train_data <- reactive({
        filtered_data() |> 
            filter(Month < yearmonth(input$train_cutoff))
    })
    
    # Reactive: fitted models (all three models)
    fitted_models_all <- reactive({
        train_data() |>
            model(
                tslm = TSLM(Sales ~ trend() + season()),
                ets = ETS(Sales),
                arima = ARIMA(Sales)
            )
    })
    
    # Reactive: selected models for training
    fitted_models_train <- reactive({
        req(input$training_models)
        fitted_models_all() |>
            select(all_of(input$training_models))
    })
    
    # Reactive: selected models for forecast
    fitted_models_forecast <- reactive({
        req(input$forecast_models)
        fitted_models_all() |>
            select(all_of(input$forecast_models))
    })
    
    # Reactive: forecast
    forecasts <- reactive({
        req(input$forecast_models)
        
        forecast_end <- yearmonth(input$forecast_end)
        train_end <- yearmonth(input$train_cutoff)
        h_months <- as.numeric(forecast_end - train_end)
        
        fitted_models_forecast() |>
            forecast(h = paste(h_months, "months"))
    })
    
    # Output: Visualization plot
    output$viz_plot <- renderPlot({
        train_cutoff_date <- yearmonth(input$train_cutoff)
        
        filtered_data() |> 
            autoplot(Sales) +
            geom_vline(xintercept = as_date(train_cutoff_date), 
                      color = "red", linetype = "dashed") +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            labs(title = "Australian Wine Sales",
                 y = "Sales") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Output: Training accuracy
    output$training_accuracy_table <- render_gt({
        req(input$training_models)
        
        fitted_models_train() |>
            accuracy() |>
            select(Varietal, .model, RMSE, MAE, MAPE) |>
            arrange(.model, RMSE) |>
            gt() |> 
            fmt_number(decimals = 2)
    })
    
    # Output: Model specification
    output$model_specification <- renderPrint({
        req(input$training_models)
        fitted_models_train()
    })
    
    # Output: Forecast accuracy
    output$forecast_accuracy_table <- render_gt({
    req(input$forecast_models)

    train_cutoff <- yearmonth(input$train_cutoff)

    # Validation dataset = months AFTER training cutoff
    val_data <- filtered_data() |>
        filter(Month >= train_cutoff)

    forecasts() |>
        accuracy(val_data) |>
        select(Varietal, .model, RMSE, MAE, MAPE) |>
        arrange(Varietal, .model) |>
        gt() |>
        fmt_number(decimals = 2)
    })
    
    # Output: Forecast plot
    output$forecast_plot <- renderPlot({
        req(input$forecast_models)
        
        forecasts() |>
            autoplot(train_data()) +
            labs(title = "Australian Wine Sales Forecasts",
                y = "Sales",
                x = "Year") +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

# Run app
shinyApp(ui, server)