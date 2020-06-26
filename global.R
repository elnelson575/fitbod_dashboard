# Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shinydashboard)
library(lubridate)
library(DT)
library(shinyWidgets)
library(rintrojs)
library(stringr)
library(rhandsontable)

# tabs
source("exercise_view_tab.R")
source("right_side_bar.R")
source("dashboard_tab.R")
source("muscle_group_tab.R")
backup_data <- read.csv('sample_workout.csv')
