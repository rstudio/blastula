html_entities <- jsonlite::fromJSON("https://www.w3.org/TR/html5/entities.json") %>%
  lapply(magrittr::extract2, "characters") %>%
  list2env()
usethis::use_data(html_entities, internal = TRUE)
