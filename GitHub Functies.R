library(httr)
library(stringr)
library(purrr)
library(readr)
library(jsonlite)
library(xml2)
library(arrow)
library(readxl)
library(tools)

# Git HUB connectie naar private repository configureren
repos <- "https://raw.githubusercontent.com/"

token <- 'xxx'




# Function to get files from GitHub and return their contents
get_git_files <- function(repos, filenames, token = NULL, invisible = TRUE, delimiter = NULL) {
  
  
  # Function to read common file type
  
  # Function to read any file type with an optional separator argument
  read_common_file <- function(file_path, delimit = delimiter) {
    file_ext <- file_ext(file_path)
    
    # Switch based on file extension
    switch(file_ext,
           csv = read_delim(file_path, delim = delimit),   # Use separator if provided
           tsv = read_delim(file_path, delim = delimit),  # Use separator if provided
           xlsx = read_excel(file_path),
           xls = read_excel(file_path),
           json = read_json(file_path),
           xml = read_xml(file_path),
           rds = read_rds(file_path),
           parquet = read_parquet(file_path),
           str_c(file_ext, ' is not supported')
    )
  }
  
         
  
  # Function to handle a single file download and read
  process_file <- function(filename) {
    # Define the URL
    url <- str_c(repos, filename)
    
    # Perform the GET request
    if (!is.null(token)) {
      request <- GET(url, add_headers(Authorization = paste("token", token)))
    } else {
      request <- GET(url)
    }
    
    # Save the content to a temporary file
    temp_file <- tempfile(fileext = str_c(".", sub(".*\\.", "", filename)))
    writeBin(content(request, "raw"), temp_file)
    
    # Read the file content using read_any_file
    return(read_common_file(temp_file))
  }
  
  # Apply the process_file function to each filename
  if(invisible) {
    results <- map(filenames, process_file) %>% invisible() %>% suppressMessages()
  } else {
    results <- map(filenames, process_file)
  }
  
  names(results) <- filenames
  
  # If there's only one file, return the result directly
  if (length(filenames) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}


get_git_files(repos, list('devise_case_1.csv','devise_case_2.csv', 'process_flowchart.png', 'example_1.json'), token, delim = ';', invisible = F)





# Function to save files from GitHub to a designated directory
save_git_files <- function(repos, filenames, token = NULL, save_in) {
  
  # Function to handle a single file download and save
  process_file <- function(filename) {
    # Define the URL
    url <- str_c(repos, filename)
    
    # Perform the GET request
    if (!is.null(token)) {
      request <- GET(url, add_headers(Authorization = paste("token", token)))
    } else {
      request <- GET(url)
    }
    
    # Define the file path for saving
    file_dir <- file.path(save_in, filename)
    
    # Save content to file
    writeBin(content(request, "raw"), file_dir)
    
    return(file_dir)
  }
  
  # Apply the process_file function to each filename
  results <- map(filenames, process_file)
  names(results) <- filenames
  
  # If there's only one file, return the result directly
  if (length(filenames) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}










