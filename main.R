########################################################################################################################
target_years = c(2018, 2019)
global_limits = c(5, 41)
moving_average = 10
global_file_type = "png"

system.file("extdata", package = "BodenfeuchteGraphen") %>%
    dir(full.names = TRUE) %>%
    purrr::walk(~ source(.x))
