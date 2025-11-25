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

# Create month sequence for slider
month_dates <- seq(from = yearmonth("1980 Jan"), to = yearmonth("1994 Dec"), by = 1)
month_labels <- format(as.Date(month_dates), "%b %Y")

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
                tags$div(
                    style = "padding: 10px 0;",
                    sliderInput("date_range", "Select Date Range",
                               min = 1,
                               max = length(month_dates),
                               value = c(
                                   which(month_dates == default_train_cutoff),
                                   which(month_dates == max_date)
                               ),
                               step = 1,
                               ticks = FALSE)
                ),
                uiOutput("date_labels")
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
                checkboxGroupInput("training_varietals", "Select Varietals",
                                  choices = varietals,
                                  selected = varietals),
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
                checkboxGroupInput("forecast_varietals", "Select Varietals",
                                  choices = varietals,
                                  selected = varietals),
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
    ),
    
    # Custom JavaScript to format slider
    tags$head(
        tags$script(HTML(sprintf("
            $(document).ready(function() {
                var monthLabels = %s;
                
                setTimeout(function() {
                    $('#date_range').data('ionRangeSlider').update({
                        prettify: function(num) {
                            return monthLabels[num - 1];
                        },
                        grid: true,
                        grid_num: 14,
                        force_edges: true
                    });
                }, 100);
            });
        ", jsonlite::toJSON(month_labels))))
    )
)

# Server
server <- function(input, output, session) {
    
    # Reactive: get selected dates from slider
    train_cutoff <- reactive({
        month_dates[input$date_range[1]]
    })
    
    forecast_end <- reactive({
        month_dates[input$date_range[2]]
    })
    
    # Output: Date labels
    output$date_labels <- renderUI({
        div(
            style = "margin-top: 10px;",
            tags$p(
                tags$span(style = "color: red; font-weight: bold;", "● Training Cutoff: "),
                format(as.Date(train_cutoff()), "%b %Y")
            ),
            tags$p(
                tags$span(style = "color: blue; font-weight: bold;", "● Forecast End: "),
                format(as.Date(forecast_end()), "%b %Y")
            )
        )
    })
    
    # Reactive: filtered data (for visualization tab)
    filtered_data <- reactive({
        req(input$varietals)
        
        aus_wine |> 
            select(Month, all_of(input$varietals)) |> 
            pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
            as_tsibble(index = Month, key = Varietal)
    })
    
    # Reactive: training data for model building tab
    train_data <- reactive({
        req(input$training_varietals)
        
        aus_wine |> 
            select(Month, all_of(input$training_varietals)) |> 
            pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
            as_tsibble(index = Month, key = Varietal) |> 
            filter(Month < train_cutoff())
    })
    
    # Reactive: training data for forecast tab
    train_data_forecast <- reactive({
        req(input$forecast_varietals)
        
        aus_wine |> 
            select(Month, all_of(input$forecast_varietals)) |> 
            pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
            as_tsibble(index = Month, key = Varietal) |> 
            filter(Month < train_cutoff())
    })
    
    # Reactive: full data for forecast accuracy
    filtered_data_forecast <- reactive({
        req(input$forecast_varietals)
        
        aus_wine |> 
            select(Month, all_of(input$forecast_varietals)) |> 
            pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
            as_tsibble(index = Month, key = Varietal)
    })
    
    # Reactive: fitted models (all three models) for model building tab
    fitted_models_all <- reactive({
        train_data() |>
            model(
                tslm = TSLM(Sales ~ trend() + season()),
                ets = ETS(Sales),
                arima = ARIMA(Sales)
            )
    })
    
    # Reactive: fitted models for forecast tab
    fitted_models_all_forecast <- reactive({
        train_data_forecast() |>
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
        fitted_models_all_forecast() |>
            select(all_of(input$forecast_models))
    })
    
    # Reactive: forecast
    forecasts <- reactive({
        req(input$forecast_models)
        
        h_months <- as.numeric(forecast_end() - train_cutoff())
        
        fitted_models_forecast() |>
            forecast(h = paste(h_months, "months"))
    })
    
    # Output: Visualization plot
    output$viz_plot <- renderPlot({
        filtered_data() |> 
            autoplot(Sales) +
            geom_vline(xintercept = as_date(train_cutoff()), 
                      color = "red", linetype = "dashed", linewidth = 1) +
            geom_vline(xintercept = as_date(forecast_end()), 
                      color = "blue", linetype = "dashed", linewidth = 1) +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
            labs(title = "Australian Wine Sales",
                 y = "Sales",
                 x = "Year") +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                strip.text = element_text(size = 14, face = "bold"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 13),
                plot.title = element_text(size = 16)
            )
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
        
        forecasts() |>
            accuracy(filtered_data_forecast()) |>
            select(Varietal, .model, RMSE, MAE, MAPE) |>
            arrange(.model, RMSE) |>
            gt() |> 
            fmt_number(decimals = 2)
    })
    
    # Output: Forecast plot
    output$forecast_plot <- renderPlot({
        req(input$forecast_models)
        
        forecasts() |>
            autoplot(train_data_forecast()) +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
            labs(title = "Australian Wine Sales Forecasts",
                 y = "Sales",
                 x = "Year") +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                strip.text = element_text(size = 14, face = "bold"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 13),
                plot.title = element_text(size = 16)
            )
    })
}

# Run app
shinyApp(ui, server)