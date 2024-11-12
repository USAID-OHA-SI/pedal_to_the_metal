#' Save a gt Table as a PNG Image with Country-Specific Filename
#'
#' This function saves a `gt` table as a PNG image file with a filename 
#' structured to include the ISO code for a specified country and a topic. 
#' An optional subtopic can also be included in the filename.
#'
#' @param gt_obj A `gt` table object to be saved as a PNG image.
#' @param cntry A string specifying the country name, used to retrieve the 
#'              corresponding ISO code.
#' @param topic A string representing the main topic of the table; used in 
#'              the filename.
#' @param subtopic An optional string representing a subtopic, which is 
#'                 appended to the main topic in the filename.
#' @param ... Additional arguments passed to `gt::gtsave()`.
#'
#' @details
#' The function creates a filename in the format `Images/{iso}_{name}.png`, 
#' where `{iso}` is the ISO code of the specified country and `{name}` is a 
#' lowercase, space-free concatenation of the `topic` and `subtopic`. The 
#' function uses `glamr::pepfar_country_list` to look up the ISO code for 
#' the specified country. A message is displayed indicating the save status 
#' with the country name and generated filename.
#'
#' @export
save_gt <- function(gt_obj, cntry, topic, subtopic = NULL, ...) {
  
  # Get the country ISO code
  iso <- glamr::pepfar_country_list %>% 
    dplyr::filter(country == cntry) %>% 
    dplyr::pull(country_iso)
  
  # Concatenate topic and subtopic if provided
  if (!is.null(subtopic)) {
    topic <- glue::glue("{topic}-{subtopic}")
  }
  
  # Format the filename
  name <- stringr::str_remove_all(topic, " ") %>% tolower()
  filename <- glue::glue("Images/{iso}_{name}.png")
  
  # Display save message
  cli::cli_alert_info("Saving...{cntry}...{filename}")
  
  # Use gt::gtsave to save the gt table
  gt::gtsave(gt_obj, filename, ...)
}
