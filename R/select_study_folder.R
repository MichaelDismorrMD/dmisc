#' Select study root directory to chose where to load and save files from
#'
#' @return A string with the path to the study folder
#' @export
#'
#' @examples
#' my_root_path <- select_study_folder()
#' # Chose one number corresponding the the folder you want to use and press enter
#' # That is now stored in the global variable my_root_path
#'
select_study_folder <- function() {
  # Define the path
  path <- "~/Library/CloudStorage"

  # List the folders in the path
  folders <- list.dirs(path, full.names = FALSE, recursive = FALSE)

  # Check if there are any folders
  if(length(folders) == 0) {
    print("No folders found in CloudStorage.")
    return(NULL)
  }

  # Print the folders with a corresponding number
  cat("Select a folder by entering the number:\n")
  for (i in 1:length(folders)) {
    cat(i, ": ", folders[i], "\n", sep="")
  }

  # Ask the user to select a folder
  selection <- as.integer(readline(prompt = "Enter the number of your choice: "))

  # Check if the selection is valid
  if(is.na(selection) || selection < 1 || selection > length(folders)) {
    print("Invalid selection.")
    return(NULL)
  }

  # Return the selected folder and assign its path to a global variable
  selected_folder <- folders[selection]
  study_root_path <- file.path(path, selected_folder) # Storing the full path globally
  print(paste("You have selected:", selected_folder))
  return(study_root_path)
}

