# PROJECT:  C:/Users/tessam/Documents/Github/pedal_to_the_metal
# PURPOSE:  
# AUTHOR:   T. Essam | USAID
# REF ID:   2d2520e4 
# LICENSE:  MIT
# DATE:     2024-11-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)


# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "2d2520e4"  #a reference to be places in viz caption


# Function to count lines of code in a local repository
count_lines_local_repo <- function(repo_path = ".", 
                                   file_extensions = c("R", "r", "py", "java", "js", 
                                                       "cpp", "c", "html", "css", 
                                                       "ts", "go", "swift")) {
  # Ensure the provided path exists
  if (!dir.exists(repo_path)) {
    stop("The specified repository path does not exist.")
  }
  
  # Get list of all files in the repository
  all_files <- list.files(path = repo_path, 
                          pattern = NULL, 
                          recursive = TRUE, 
                          full.names = TRUE,
                          ignore.case = TRUE)
  
  # Filter files by the specified extensions
  pattern <- paste0("\\.(", paste(file_extensions, collapse = "|"), ")$")
  code_files <- all_files[grepl(pattern, all_files, ignore.case = TRUE)]
  
  if (length(code_files) == 0) {
    cat("No code files found with the specified extensions.\n")
    return(NULL)
  }
  
  cat("Found", length(code_files), "code files. Counting lines...\n")
  
  # Initialize total line count
  total_lines <- 0
  
  # Optionally, create a data frame to store per-file line counts
  line_counts <- data.frame(
    file = character(),
    lines = integer(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over each file and count the lines
  for (file in code_files) {
    # Try reading the file and counting lines
    line_count <- tryCatch({
      length(readLines(file, warn = FALSE))
    }, error = function(e) {
      # If there's an error (e.g., binary file), skip the file
      0
    })
    
    total_lines <- total_lines + line_count
    
    # Append to the data frame
    line_counts <- rbind(line_counts, data.frame(file = file, lines = line_count, stringsAsFactors = FALSE))
  }
  
  # Print the total lines of code
  cat("Total lines of code:", total_lines, "\n")
  
  # Optionally, return the detailed line counts
  return(line_counts)
}

# Example Usage
# Set the repository path to the current working directory
repo_path <- "."  

# Call the function
line_counts_df <- count_lines_local_repo(repo_path)

# Optional: View the detailed line counts
# View(line_counts_df)  # Uncomment this line if running in RStudio
# Or print the first few entries
head(line_counts_df)


library(gt)

gt(line_counts_df) %>% 
grand_summary_rows(
  columns = c(lines),
  fns = list(
    sum ~ sum(.)),
  fmt = ~ fmt_number(., use_seps = T)
) %>% 
  gtExtras::gt_theme_nytimes() 
