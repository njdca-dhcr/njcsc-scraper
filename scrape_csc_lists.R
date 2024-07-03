# Scrape active New Hersey Civil Service Commission Eligible Lists

library(RSelenium)
library(rvest)
library(tidyverse)
library(xml2)
library(lubridate)

# Start RSelenium
rD <- rsDriver(browser = "firefox", port = 4444L)
remDr <- rD$client

# Navigate to the initial URL
initial_url <- "https://info.csc.state.nj.us/EligibleLists/Category_List.aspx?VarCat=S&VarPromo=O"
remDr$navigate(initial_url)

# Define the path to the CSV file
csv_file <- "CSC Entry Level List.csv"

# Function to extract table from the current page and clean the data
extract_table <- function(remDr) {
  page_source <- remDr$getPageSource()[[1]]
  page <- read_html(page_source)
  table <- page %>% html_nodes(xpath="/html/body/form/table/tbody/tr[4]/td/table/tbody/tr/td/table/tbody/tr[3]") %>% html_table(header = TRUE)
  table <- data.frame(table[1]) %>%
    filter(!apply(., 1, function(row) any(str_detect(row, "« Previous Page|\\| Next Page »"))))
  return(table)
}

# Extract the initial table
all_tables <- list(extract_table(remDr))

# Function to check if the "Next Page" button is clickable
is_next_page_clickable <- function(remDr) {
  page_source <- remDr$getPageSource()[[1]]
  page <- read_html(page_source)
  next_button <- page %>% html_node(xpath = "//a[contains(@href, '__doPostBack') and contains(text(), 'Next Page')]")
  if (is.na(next_button)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Click the "Next" button and extract tables until the last page
while (is_next_page_clickable(remDr)) {
  next_button <- remDr$findElement(using = "xpath", "//a[contains(@href, '__doPostBack') and contains(text(), 'Next Page')]")
  next_button$clickElement()
  Sys.sleep(2) # Wait for the page to load
  all_tables <- append(all_tables, list(extract_table(remDr)))
}

# Append the last page table
all_tables <- append(all_tables, list(extract_table(remDr)))

# Combine all tables into a single data frame, filter out duplicates
combined_table <- bind_rows(all_tables) %>% 
  distinct(Symbol,Jurisdiction,.keep_all = TRUE) %>%
  mutate(As.Of = Sys.Date(),
         Issue.Date = mdy(Issue.Date),
         Promulgation.Date = mdy(Promulgation.Date),
         Expiration.Date = mdy(Expiration.Date))

# Check if the CSV file exists and read it if it does
if (file.exists(csv_file)) {
  previous_table <- read_csv(csv_file)
  # Remove the As.Of column from the previous table
  previous_table <- previous_table %>% select(-As.Of)
} else {
  previous_table <- NULL
}

# Remove the As.Of column from the current table
current_table <- combined_table %>% select(-As.Of)

# Compare the current table with the previous table
# TODO:fix the error from the call to all.equal, replaced this with the call to all_equal and it's erroring
if (is.null(previous_table) || !all.equal(previous_table, current_table)) {
  write_csv(combined_table, csv_file)
  message("CSV file has been updated.")
} else {
  message("No changes detected, CSV file remains unchanged.")
}

# Close RSelenium
remDr$close()
rD$server$stop()
