#' Select study root directory to chose where to load and save files from
#'
#' @param path A string containing the path to list directories from. String.
#'
#' @return A string with the path to the selected study root folder
#' @export
#'
#' @examples
#' \dontrun{
#' my_root_path <- select_study_folder()
#' }
#' # Put interactive = FALSE in the example above to not hang automated tests during build
#' # Chose one number corresponding the the folder you want to use and press enter
#' # That is now stored in the global variable my_root_path
#'
select_study_folder <- function(path = "~/Library/CloudStorage") {
  while (TRUE) {
    # List the folders in the path
    folders <- list.dirs(path, full.names = FALSE, recursive = FALSE)

    # Check if there are any folders
    if(length(folders) == 0) {
      print("No folders found in this directory.")
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
      print("Invalid selection. Please try again.")
      next
    }

    # Get the selected folder
    selected_folder <- folders[selection]
    study_root_path <- file.path(path, selected_folder)

    # Confirm the selection or explore further
    cat("You have selected:", selected_folder, "\n")
    action <- readline(prompt = "Do you want to (p)ick this folder or (s)earch within it? (p/s): ")

    if (tolower(action) == "p") {
      print(paste("You have picked the folder:", study_root_path))
      return(study_root_path)
    } else if (tolower(action) == "s") {
      # Update path to the selected folder and search again
      path <- study_root_path
    } else {
      print("Invalid option. Please try again.")
    }
  }
}


