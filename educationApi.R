# Load necessary libraries
library(educationdata)
library(plumber)
library(ggplot2)
library(dplyr)

#* @apiTitle Education Data API

# Scatterplot
#* Endpoint to create a scatterplot
#* @param level The level of data (e.g., "schools", "college-university")
#* @param source The data source (e.g., "ccd", "ipeds")
#* @param topic The topic of data (e.g., "directory", "enrollment")
#* @param filters A JSON string of filter criteria (e.g., '{"ncessch": 341728004966}')
#* @param x_var The variable for the x-axis
#* @param y_var The variable for the y-axis
#* @get /scatterplot
#* @serializer contentType list(type="image/png")
function(level, source, topic, filters = "{}", x_var, y_var) {
  filter_list <- jsonlite::fromJSON(filters)
  data <- get_education_data(
    level = level,
    source = source,
    topic = topic,
    filters = filter_list
  )
  if (!x_var %in% colnames(data) || !y_var %in% colnames(data)) {
    stop("The specified variables are not in the data.")
  }
  
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    theme_minimal() +
    labs(x = x_var, y = y_var, title = paste("Scatterplot of", y_var, "vs", x_var))

  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot = plot, device = "png")

  readBin(tmp, "raw", file.info(tmp)$size)
}


#Histogram
#* Endpoint to create a historgram
#* @post /histogram
#* @param body:list
#* @serializer contentType list(type="image/png")
function(req) {
  json_body <- jsonlite::fromJSON(req$postBody)
  level <- json_body$body$level
  source <- json_body$body$source
  topic <- json_body$body$topic
  #year <- json_body$body$year
  
  expenses_data <- get_education_data(
    level = level,
    source = source,
    topic = topic,
    filters = list(year=2021))
  
  expenses_data$room_board <- replace(expenses_data$room_board , is.na(expenses_data$room_board), 0)
  
  data_with_total <- mutate(expenses_data, 
                            total_expenses = books_supplies + room_board + exp_other)
  
  plot <- ggplot(data_with_total, aes(x = total_expenses)) +
    geom_histogram(binwidth = 2500) +
    scale_x_continuous(breaks = seq(0, 55000, 5000), limits = c(0.0, 60000.0)) +
    labs(title = "Distribution of Total Expenses", 
         x = "Total Expenses (in dollars)", 
         y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.2))
  
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot = plot, device = "png")
  
  readBin(tmp, "raw", file.info(tmp)$size)
}

#OTHER ENDPOINTS AVAILABLE:

#Barplot
#* Endpoint to create a barplot
#* @param level The level of data (e.g., "schools", "college-university")
#* @param source The data source (e.g., "ccd", "ipeds")
#* @param topic The topic of data (e.g., "directory", "enrollment")
#* @param filters A JSON string of filter criteria (e.g., '{"ncessch": 341728004966}')
#* @param x_var The variable for the x-axis
#* @get /barplot
#* @serializer contentType list(type="image/png")
function(level, source, topic, filters = "{}", x_var) {
  filter_list <- jsonlite::fromJSON(filters)
  data <- get_education_data(
    level = level,
    source = source,
    topic = topic,
    filters = filter_list
  )
  if (!x_var %in% colnames(data)) {
    stop("The specified variable is not in the data.")
  }

  plot <- ggplot(data, aes_string(x = x_var)) +
    geom_bar() +
    labs(x = x_var, y = "Count", title = paste("Barplot of", x_var))+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot = plot, device = "png")

  readBin(tmp, "raw", file.info(tmp)$size)
}

#* Endpoint to retrieve education data
#* @get /getEducationData
#* @param level The education data level (e.g., "schools")
#* @param source The data source (e.g., "ccd")
#* @param topic The topic of the data (e.g., "directory")
#* @param filters JSON string of filters (e.g., '{"ncessch": 341728004966}')
#* @serializer json
function(level, source, topic, filters = "{}") {
  filter_list <- jsonlite::fromJSON(filters)
  data <- get_education_data(
    level = level,
    source = source,
    topic = topic,
    filters = filter_list
  )
  return(data)
}


