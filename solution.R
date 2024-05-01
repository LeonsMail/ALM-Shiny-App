#install.packages("shiny")
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)


# Read data and handle errors
tryCatch({
  data <- read_excel('dataset_MyFoodData.xlsx')
}, error = function(e) {
  showModal(modalDialog(
    title = "Error",
    paste("Error reading dataset_MyFoodData.xlsx:", e$message)
  ))
})

tryCatch({
  goal_data <- read.csv('goal_data.csv')
  goal_data$Date <- as.Date(goal_data$Date, format = "%d-%m-%Y")
}, error = function(e) {
  showModal(modalDialog(
    title = "Error",
    paste("Error reading goal_data.csv:", e$message)
  ))
})

tryCatch({
  intake_data <- read.csv('intake_data.csv')
  intake_data$Date <- as.Date(intake_data$Date, format = "%d-%m-%Y")
}, error = function(e) {
  showModal(modalDialog(
    title = "Error",
    paste("Error reading intake_data.csv:", e$message)
  ))
})


# UI function for setting goals
goal_ui <- function() {
  tryCatch({
    fluidRow(
      column(8, offset = 2,
             div(style = "background-color: #ffffff; margin-top: 100px; padding: 30px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                 h3("Set Your Daily Goals", style = "text-align: center; color: #333333; font-family: 'Roboto', sans-serif;"),
                 fluidRow(
                   column(6,
                          numericInput("goal_calories", "Goal Calories", value = 2000, width = "100%"),
                          tags$style(paste0("#goal_calories .form-control { border: 1px solid #4CAF50; color: #4CAF50; }")),
                          numericInput("goal_fats", "Goal Fats (g)", value = 70, width = "100%"),
                          tags$style(paste0("#goal_fats .form-control { border: 1px solid #FF9800; color: #FF9800; }"))
                   ),
                   column(6,
                          numericInput("goal_protein", "Goal Protein (g)", value = 50, width = "100%"),
                          tags$style(paste0("#goal_protein .form-control { border: 1px solid #2196F3; color: #2196F3; }")),
                          numericInput("goal_sugar", "Goal Sugar (g)", value = 30, width = "100%"),
                          tags$style(paste0("#goal_sugar .form-control { border: 1px solid #9C27B0; color: #9C27B0; }"))
                   )
                 ),
                 actionButton("set_goals", "Set Goals", style = "color: white; background-color: #4CAF50; border-radius: 5px; width: 100%; margin-top: 20px; font-family: 'Roboto', sans-serif;")
             )
      )
    )
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred in goal_ui:", e$message)
    ))
  })
}

# UI function for adding food
add_food_ui <- function() {
  tryCatch({
    fluidRow(
      column(8,
             h3("Add Food", style = "text-align: center; font-weight: bold; margin-left: 250px;"),
             fluidRow(
                      div(style = "background-color: #ffffff; margin-left: 300px; margin-bottom: 10px; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                      fluidRow(
                        column(6,
                        selectizeInput("breakfast", "Breakfast", choices = data$name, options = list(
                        placeholder = 'Select a food item',
                        onInitialize = I('function() { this.setValue(""); }')
                      ))),
                      column(6, style = 'padding-top: 25px;',
                      actionButton("add_breakfast", "Add Breakfast", style = "color: white; background-color: #4CAF50; border-radius: 5px;")
               ))),
                      div(style = "background-color: #ffffff; margin-left: 300px; margin-bottom: 10px; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                      fluidRow(
                        column(6,
                        selectizeInput("snack1", "Snack 1", choices = data$name, options = list(
                        placeholder = 'Select a food item',
                        onInitialize = I('function() { this.setValue(""); }')
                      ))),
                      column(6, style = "padding-top: 25px;",
                      actionButton("add_snack1", "Add Snack 1", style = "color: white; background-color: #4CAF50; border-radius: 5px;")
               ))),
                    
                    div(style = "background-color: #ffffff; margin-left: 300px; margin-bottom: 10px; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                    fluidRow(
                        column(6, 
                        selectizeInput("lunch", "Lunch", choices = data$name, options = list(
                        placeholder = 'Select a food item',
                        onInitialize = I('function() { this.setValue(""); }')
                      ))),
                      column(6, style = "padding-top: 25px;",
                      actionButton("add_lunch", "Add Lunch", style = "color: white; background-color: #4CAF50; border-radius: 5px;")
               ))),
               
                    div(style = "background-color: #ffffff; margin-left: 300px; margin-bottom: 10px; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                    fluidRow(
                        column(6, 
                        selectizeInput("snack2", "Snack 2", choices = data$name, options = list(
                        placeholder = 'Select a food item',
                        onInitialize = I('function() { this.setValue(""); }')
                      ))),
                      column(6, style = "padding-top: 25px;",
                      actionButton("add_snack2", "Add Snack 2", style = "color: white; background-color: #4CAF50; border-radius: 5px;")
               
               ))),
               
                  div(style = "background-color: #ffffff; margin-left: 300px; margin-bottom: 10px; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
                  fluidRow(
                        column(6,
                        selectizeInput("dinner", "Dinner", choices = data$name, options = list(
                        placeholder = 'Select a food item',
                        onInitialize = I('function() { this.setValue(""); }')
                      ))),
                      column(6, style = 'padding-top: 25px;',
                      actionButton("add_dinner", "Add Dinner", style = "color: white; background-color: #4CAF50; border-radius: 5px;")
               )))
             )
      )
    )
  }, error = function(e) {
    cat("Error in add_food_ui:", conditionMessage(e), "\n")
    return(NULL)
  })
}


daily_intake_tracker <- function() {
  # Read the latest goal data from the CSV file
  tryCatch({
    goal_data <- read.csv('goal_data.csv')
    goal_data$Date <- as.Date(goal_data$Date, format = "%d-%m-%Y")
    
    total_intake_today <- intake_data %>%
      mutate_at(vars(-Date, -ID, -Name), ~ifelse(. == "NULL", NA, as.numeric(.))) %>%
      summarise(
        Total_Calories = sum(Calories, na.rm = TRUE),
        Total_Fats = sum(Fats, na.rm = TRUE),
        Total_Protein = sum(Protein, na.rm = TRUE),
        Total_Sugar = sum(Sugar, na.rm = TRUE)
      )
    
    # Fetch the latest goal values
    latest_goals <- tail(goal_data, n = 1)  # Select the last row
    
    print(latest_goals)
    
    return(list(
      Total_Calories = total_intake_today$Total_Calories,
      Total_Fats = total_intake_today$Total_Fats,
      Total_Protein = total_intake_today$Total_Protein,
      Total_Sugar = total_intake_today$Total_Sugar,
      Goal_Calories = latest_goals$Calories_Goal,
      Goal_Fats = latest_goals$Fats_Goal,
      Goal_Protein = latest_goals$Protein_Goal,
      Goal_Sugar = latest_goals$Sugar_Goal
    ))
  }, error = function(e) {
    cat("Error reading goal data:", conditionMessage(e), "\n")
    return(NULL)
  })
}

plot_weekly_trend <- function(Date, nutrient) {
  # Filter data for the specified week
  tryCatch({
    start_of_week <- Date - lubridate::wday(Date) + 1
    end_of_week <- start_of_week + 6
    weekly_data <- intake_data %>%
      filter(Date >= start_of_week & Date <= end_of_week)
    
    # Aggregate nutrient values for the specified nutrient by date
    weekly_summary <- weekly_data %>%
      group_by(Date) %>%
      mutate_at(vars(-Date, -ID, -Name), ~ifelse(. == "NULL", NA, as.numeric(.))) %>%
      summarise(total_nutrient = sum(!!sym(nutrient), na.rm = TRUE)) %>%
      mutate(selected_date = ifelse(Date == {{Date}}, "True", "False"))
    
    print(weekly_summary)
    
    # Get the goal value for the specified nutrient on the given date
    goal_value <- goal_data %>% filter(Date == {{Date}}) %>%
      pull(!!sym(paste(nutrient, "_Goal", sep = '')))
    
    # Create a bar plot
    p <- ggplot(weekly_summary, aes(x = Date, y = total_nutrient, fill = selected_date)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_hline(
        yintercept = goal_value,
        color = "#FF4136",
        linetype = "dashed",
        linewidth = 1.5
      ) +
      labs(
        title = paste("Weekly Trend for", nutrient),
        x = "Date",
        y = nutrient,
        caption = paste("Goal for", nutrient, "on", format(Date, "%d-%m-%Y"), ":", goal_value)
      ) +
      scale_x_date(labels = function(x) weekdays(x, abbreviate = TRUE)) +
      theme_minimal() +
      scale_fill_manual(
        values = c("False" = "skyblue", "True" = "#FF851B"),
        guide = "none"
      ) +
      theme(
        plot.title = element_text(
          family = "sans",
          size = 18,
          color = "#333333",
          face = "bold"
        ),
        plot.caption = element_text(
          family = "sans",
          size = 12,
          color = "#666666"
        ),
        axis.title = element_text(
          family = "sans",
          size = 14,
          color = "#333333"
        ),
        axis.text = element_text(
          family = "sans",
          size = 12,
          color = "#333333"
        )
      )
    
    return(p)
  }, error = function(e) {
    cat("Error plotting weekly trend:", conditionMessage(e), "\n")
    return(NULL)
  })
}

#plot <- plot_weekly_trend(Sys.Date(), "Sugar")
#print(plot)

# UI function for daily tracking
daily_tracking_ui <- function(daily_totals) {
  fluidRow(
    column(12,
           h3("Today's Intake vs. Goals", style = "text-align: center; font-weight: bold;"),
           fluidRow(
             column(3,
                    div(id = "daily-tracking-div", style = "background-color: #ffd480; border-radius: 10px; padding: 20px; text-align: center;",
                        h4(style = "font-weight: bold;", "Calories", icon("fire")),
                        span(style = "font-size: 30px; font-weight: bold; font-family: 'Arial Black', sans-serif;", textOutput("calories")),
                        p(style = "font-size: 18px;", verbatimTextOutput("goal_calories"))
                    )
             ),
             column(3,
                    div(id = "daily-tracking-div", style = "background-color: #a9e5a9; border-radius: 10px; padding: 20px; text-align: center;",
                        h4(style = "font-weight: bold;", "Fats", icon("utensils")),
                        span(style = "font-size: 30px; font-weight: bold; font-family: 'Arial Black', sans-serif;", textOutput("fats")),
                        p(style = "font-size: 18px;", verbatimTextOutput("goal_fats"))
                    )
             ),
             column(3,
                    div(id = "daily-tracking-div", style = "background-color: #b0c4de; border-radius: 10px; padding: 20px; text-align: center;",
                        h4(style = "font-weight: bold;", "Protein", icon("dumbbell")),
                        span(style = "font-size: 30px; font-weight: bold; font-family: 'Arial Black', sans-serif;", textOutput("protein")),
                        p(style = "font-size: 18px;", verbatimTextOutput("goal_protein"))
                    )
             ),
             column(3,
                    div(id = "daily-tracking-div", style = "background-color: #ffb6c1; border-radius: 10px; padding: 20px; text-align: center;",
                        h4(style = "font-weight: bold;", "Sugar", icon("cookie-bite")),
                        span(style = "font-size: 30px; font-weight: bold; font-family: 'Arial Black', sans-serif;", textOutput("sugar")),
                        p(style = "font-size: 18px;", verbatimTextOutput("goal_sugar"))
                    )
             )
           )
    )
  )
}

# UI function for weekly trends
weekly_trends_ui <- function() {
  fluidRow(
    column(6, style = 'margin-left: 400px;',
           h3("Weekly Trends"),
           
           fluidRow(
             column(6,
                    selectInput("date", "Select Date", choices = rev(sort(unique(intake_data$Date))), selected = max(intake_data$Date))),
             column(6,
                    selectInput("nutrient", "Select Nutrient", choices = c('Calories', 'Fats', 'Protein', 'Sugar'))
             )),
           div(id = "daily-tracking-div", style = "background-color: #F7FCFC; border-radius: 10px; padding: 20px; text-align: center;",
               
           plotOutput("weekly_trends_plot")
           )
           
    )
  )
}


# Define the UI for the app
ui <- fluidPage(
  titlePanel("ALM Food Tracker App"),
  
  tabsetPanel(
    tabPanel("Set Goals", goal_ui()),
    tabPanel("Add_Food", add_food_ui()),
    tabPanel("Daily Tracking", daily_tracking_ui()),
    tabPanel("Weekly Trends", weekly_trends_ui())
  )
)



# Define the server logic for the app
server <- function(input, output, session) {
  
  # Read the latest goal data from the CSV file
  goal_data <- tryCatch(
    {
      read.csv('goal_data.csv')
    },
    error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error reading goal_data.csv:", e$message)
      ))
      NULL
    }
  )
  
  if (is.null(goal_data)) {
    return()
  }
  
  goal_data$Date <- as.Date(goal_data$Date, format = "%d-%m-%Y")
  
  # Define reactive values to store user data
  user_data <- reactiveValues(
    goals = NULL,
    daily_intake = NULL
  )
  
  # Update user goals ----------------------------------------------------------
  observeEvent(input$set_goals, {
    
    user_data$goals <- data.frame(
      Date = Sys.Date(),
      Calories_Goal = input$goal_calories,
      Fats_Goal = input$goal_fats,
      Protein_Goal = input$goal_protein,
      Sugar_Goal = input$goal_sugar
    )
    
    
    # Export a data frame to a text file using write.table()
    tryCatch({
      write.table(user_data$goals,
                  file = "goal_data.csv",
                  sep = ",",
                  append = TRUE,
                  row.names = FALSE, 
                  col.names = FALSE
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while writing to goal_data.csv:", e$message)
      ))
    })
    
  })
  
  # Add food to daily intake ---------------------------------------------------
  add_food_to_intake <- function(food_item, meal_type) {
    selected_food <- data %>% filter(name == food_item)
    
    if (nrow(selected_food) > 0) {
      food_nutrition <- selected_food[, c("ID", "Calories", "Fat (g)", "Protein (g)", "Sugars (g)")]
      
      user_data$daily_intake <- rbind(user_data$daily_intake, data.frame(
        Date = Sys.Date(),
        ID = food_nutrition$ID,
        Food = food_item,
        Meal = meal_type,
        Calories = food_nutrition$Calories,
        Fats = food_nutrition$`Fat (g)`,
        Protein = food_nutrition$`Protein (g)`,
        Sugar = food_nutrition$`Sugars (g)`
      ))
      
      tryCatch({
        write.table(user_data$daily_intake,
                    file = "intake_data.csv",
                    sep = ",",
                    append = TRUE,
                    row.names = FALSE,
                    col.names = FALSE
        )
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred while writing to intake_data.csv:", e$message)
        ))
      })
      
      print('Added intake')
    } else {
      showModal(modalDialog(
        title = "Error",
        "Food item not found in database. Please enter a valid food item."
      ))
    }
  }
  
  observeEvent(input$add_breakfast, {
    add_food_to_intake(input$breakfast, "Breakfast")
  })
  
  observeEvent(input$add_snack1, {
    add_food_to_intake(input$snack1, "Snack 1")
  })
  
  observeEvent(input$add_lunch, {
    add_food_to_intake(input$lunch, "Lunch")
  })
  
  observeEvent(input$add_snack2, {
    add_food_to_intake(input$snack2, "Snack 2")
  })
  
  observeEvent(input$add_dinner, {
    add_food_to_intake(input$dinner, "Dinner")
  })
  
  output$calories <- renderText({
    tryCatch({
      daily_totals <- daily_intake_tracker()
      toString(paste0("Total Calories: ", daily_totals$Total_Calories, " / ", daily_totals$Goal_Calories))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while calculating total calories:", e$message)
      ))
    })
  })
  
  output$fats <- renderText({
    tryCatch({
      daily_totals <- daily_intake_tracker()
      toString(paste0("Total Fats: ", daily_totals$Total_Fats, " / ", daily_totals$Goal_Fats))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while calculating total fats:", e$message)
      ))
    })
  })
  
  output$protein <- renderText({
    tryCatch({
      daily_totals <- daily_intake_tracker()
      toString(paste0("Total Protein: ", daily_totals$Total_Protein, " / ", daily_totals$Goal_Protein))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while calculating total protein:", e$message)
      ))
    })
  })
  
  output$sugar <- renderText({
    tryCatch({
      daily_totals <- daily_intake_tracker()
      toString(paste0("Total Sugar: ", daily_totals$Total_Sugar, " / ", daily_totals$Goal_Sugar))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while calculating total sugar:", e$message)
      ))
    })
  })
  
  # Render weekly trends plot
  output$weekly_trends_plot <- renderPlot({
    tryCatch({
      plot_weekly_trend(as.Date(input$date), input$nutrient)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while rendering weekly trends plot:", e$message)
      ))
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

