library(gh)
library(purrr)
library(dplyr)
library(ggplot2)
library(lubridate)

bcgov <- gh("GET /orgs/{org}", org = "bcgov")

#get number of bcgov repos
n_repos <- bcgov[["public_repos"]]

safe_get_element <- function(x, element, default = NA_character_) {
  y <- x[[element]]
  if (is.null(y)) 
    default
  else y
}

## get all repos names and date created
repo_names <- map_df(seq_len(ceiling(n_repos/30)), ~{
  repos <- gh("GET /orgs/{org}/repos", org = "bcgov", page = .x)
  data.frame(
    name = vapply(repos, safe_get_element, "", "name"),
    date_created = vapply(repos, safe_get_element, "", "created_at"), 
    top_language = vapply(repos, safe_get_element, "", "language")
  )
})

## join together and find the cumulative number of repos
repos_over_time <- repo_names %>% 
  mutate(date_created = ymd_hms(date_created)) %>% 
  mutate(date = as.Date(date_created)) %>% 
  group_by(top_language, date) %>% 
  summarise(n = n()) %>% 
  mutate(cumu = cumsum(n))

## create a df for labelling
label_df <- repos_over_time %>% 
  group_by(top_language) %>% 
  filter(cumu == max(cumu)) %>% 
  ungroup() %>% 
  slice_max(order_by = cumu, n = 10)

saveRDS(label_df, "data/label_df.rds")
saveRDS(repos_over_time, "data/repos_over_time.rds")
