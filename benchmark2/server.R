library(shinydashboard)
library(shiny)
library(googlesheets)
library(Benchmarking)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinyBS) # for tooltip
library(tinytex) # for tooltip

data_folder<-"user_input_data"

get_time_human <- function() {
        format(Sys.time(), "%Y%m%d-%H%M%OS")
}

saveData <- function(data,id) {
        file_name <- paste0(id,".csv")
        data <- t(c(data,id))
        write.csv(x = data, file = file.path(data_folder, file_name), 
                  row.names = FALSE, quote = TRUE)
}

loadData <- function() {
        files <- list.files(file.path(data_folder), full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) %>% 
                do.call(rbind, .)
        data<-data %>% 
                mutate(feed = homegrown_feed+purchased_feed) %>% 
                mutate(labour = exp_l_family+exp_l_hired) %>% 
                rename(farm_id=X) %>% 
                mutate(farm_id=as.character(farm_id)) %>% 
                arrange(farm_id)
        data
}

# Define the fields we want to save from the form
fields <- c("q_milk",
            "purchased_feed",
            "homegrown_feed",
            "capital",
            "exp_l_family",
            "exp_l_hired",
            "misc",
            "year",
            "business_type",
            "edu",
            "housing",
            "milking_system",
            "feeding_system",
            "robots",
            "milking_cows_tmr",
            "heifers_tmr",
            "separated",
            "manure",
            "breed",
            "milk_recording",
            "age_calving",
            "calving_interval",
            "barn_capacity",
            #output
            "q_butterfat",
            "n_lactating_cows",
            "n_dry_cows",
            "n_heifers",
            # expense info
            "labour_family",
            "labour_hired"
)
# server -----
server <- function(input, output) {
  
}