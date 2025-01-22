# Load necessary libraries
library(dplyr)
library(readr) # Assuming the files are CSVs. Adjust if needed (e.g., for Excel use `readxl`).

# Define the folder path
folder_path <- "/Users/manuelestebanarias/Downloads/wid_all_data"

# Get the list of files in the folder starting with "WID_data_"
file_list <- list.files(folder_path, pattern = "^WID_data_.*\\.csv$", full.names = TRUE)
n_files <- length(file_list)
# Initialize an empty data frame
combined_data <- data.frame()
 
i<-1

# Capture the start time of the process
start_time <- Sys.time()

# Loop through each file and append to the dataset
for (file in file_list) {
  # Read the current file
  temp_data <- read_delim(file, delim = ";", escape_double = FALSE, col_types = cols(country = col_character(), 
                           variable = col_character(), percentile = col_character(), 
                          year = col_number(), value = col_double(), 
                          age = col_character(), pop = col_character()), 
                          trim_ws = TRUE)
  
  # Append the data to the combined dataset
  combined_data <- bind_rows(combined_data, temp_data)
  
  
  # Print progress
  cat(i, "out of", n_files, "files processed.\n")
  i<-i+1
}
elapsed_time <- Sys.time() - start_time

cat <-("Elapsed time:", round(elapsed_time, 2), "seconds.\n"))


#Formating
combined_data$sixlet <- substr(combined_data$variable, 1, 6)
combined_data$widcode <- paste0(combined_data$sixlet, combined_data$age, combined_data$pop)  
combined_data <- combined_data %>%
  select(-sixlet, -age, -pop, -variable)


# Save the combined dataset
output_file <- file.path(folder_path, "Combined_WID_Data.csv")
write_csv(combined_data, output_file)

# Print a message when done
cat("Combined dataset saved to:", output_file, "\n")

