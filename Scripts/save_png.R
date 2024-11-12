#' Save plots as png
#' 
#' Uniform filename saving for plots as png
#'
#' @param cntry PEPFAR country name
#' @param topic plot area/topic (one word)
#' @param subtopic sub topics, if necessary (one word)
#' @param ... extra params to pass to glitr::si_save
#'
#' @return
#' @export
#'
save_png <- function(cntry, topic, subtopic, ...){
  
  iso <- glamr::pepfar_country_list %>% 
    dplyr::filter(country == cntry) %>% 
    dplyr::pull(country_iso)
  
  if(!missing(subtopic))
    topic <- glue::glue("{topic}-{subtopic}")
  
  name <- stringr::str_remove_all(topic, " ") %>% tolower()
  
  filename <- glue::glue("Images/{iso}_{name}.png") 
  
  cli::cli_alert_info("Saving...{cntry}...{filename}")
  
  glitr::si_save(filename, ...)
  
}
