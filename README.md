
# ALM Food Tracker App

The ALM Food Tracker is a Shiny app developed in R, designed to help users track their daily food intake and compare it against their nutritional goals. This application provides functionalities to set daily nutritional goals, add foods to daily meals, and view daily and weekly nutritional intake trends.

## Features

- **Set Daily Goals:** Users can set daily nutritional goals for calories, fats, proteins, and sugars (this can be changed in the future with fiber, as I believe it to be more important for a healthy diet).
- **Add Food:** Users can add foods to different meals (breakfast, lunch, dinner, snacks) from a predefined list and their nutritional values are automatically tracked.
- **Daily Tracking:** The app displays daily totals for nutritional intake and compares them with set goals.
- **Weekly Trends:** Users can view nutritional intake trends over a week, helping to monitor consistency and deviations from goals.

## Installation

To run the ALM Food Tracker locally, you need to have R and RStudio installed. Follow these steps:

1. Install R from [CRAN](https://cran.r-project.org).
2. Download and install RStudio from [RStudio's website](https://rstudio.com/products/rstudio/download/).

## Running the Application

1. Open RStudio and set your working directory to the folder containing the app files.
2. Install the required R packages if you haven't already. You can do this by running the following commands in the R console:

   ```R
   install.packages("shiny")
   install.packages("shinydashboard")
   install.packages("readxl")
   install.packages("dplyr")
   install.packages("ggplot2")
   install.packages("plotly")
3. Load the Shiny library and run the app with:
   
   library(shiny)
runApp("path_to_app_directory")

Data Files
The application uses three data files to store and manage food items, daily intake, and goals:

dataset_MyFoodData.xlsx: Contains the list of food items and their nutritional values.
goal_data.csv: Stores user-defined nutritional goals.
intake_data.csv: Logs daily food intake.
For the app to function properly, make sure these files are present in the app's directory and formatted correctly.

Contributing
Contributions to the ALM Food Tracker are welcome. You can improve the codebase, add new features, or fix bugs. Please create a pull request with your contributions.
License
This project is licensed under the MIT License.
