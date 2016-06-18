library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(BCDA)
library(ggplot2)
library(cowplot)

events <- tbl_df(readr::read_rds("data/portal-secondary-link-collaprse-test-refined.rds"))

valid_seshs <- events %>%
  group_by(session_id) %>%
  summarize(`sections clicked on` = length(unique(section_used)) - 1) %>%
  keep_where(`sections clicked on` < 2) %>%
  ungroup %>%
  { .$session_id }
nonclickthrough_seshs <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(type == "clickthrough")) %>%
  keep_where(!clickthrough) %>%
  ungroup %>%
  { .$session_id }
search_seshs <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(section_used == "search")) %>%
  keep_where(clickthrough) %>%
  ungroup %>%
  { .$session_id }
primary_link_clickthrough_seshs <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(section_used == "primary links")) %>%
  keep_where(clickthrough) %>%
  ungroup %>%
  { .$session_id }
secondary_link_clickthrough_seshs <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(section_used == "secondary links")) %>%
  keep_where(clickthrough) %>%
  ungroup %>%
  { .$session_id }
other_projects_clickthrough_seshs <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(section_used == "other projects")) %>%
  keep_where(clickthrough) %>%
  ungroup %>%
  { .$session_id }

make_table <- function(data) {
  output <- as.matrix(data[, -1])
  colnames(output) <- names(data[, -1])
  rownames(output) <- as.character(data[[1]])
  return(output)
}
flip_cols <- function(x) { return(x[, rev(1:(dim(x)[2]))]) }
flip_rows <- function(x) { return(x[rev(1:(dim(x)[1])), ]) }

wiki_visited <- events %>%
  keep_where(session_id %in% valid_seshs & !is.na(section_used)) %>%
  keep_where(session_id %in% union(primary_link_clickthrough_seshs, secondary_link_clickthrough_seshs)) %>%
  group_by(test_group) %>%
  summarize(visited_preferred = sum(`wiki matches a preferred language`),
            visited_most_preferred = sum(`wiki matches most preferred language`),
            total = n()) %>%
  ungroup

set.seed(0)
wiki_visited %>%
  mutate(visited_preferred_not = total - visited_preferred) %>%
  select(c(test_group, visited_preferred, visited_preferred_not)) %>%
  make_table %>%
  flip_rows %>%
  beta_binom %>%
  present_bbfit(digits = 2, format = "latex")

wiki_visited %>%
  mutate(visited_most_preferred_not = total - visited_most_preferred) %>%
  select(c(test_group, visited_most_preferred, visited_most_preferred_not)) %>%
  make_table %>%
  flip_rows %>%
  beta_binom %>%
  present_bbfit(digits = 2, format = "latex")

events %>%
  group_by(session_id) %>%
  summarize(test_group = head(test_group, 1), engaged = any(type == "clickthrough")) %>%
  group_by(test_group) %>%
  summarize(bounced = n() - sum(engaged), engaged = sum(engaged)) %>%
  make_table %>%
  flip_rows %>%
  flip_cols %>%
  beta_binom %>%
  present_bbfit(digits = 2, format = "latex")

events %>%
  group_by(session_id) %>%
  summarize(test_group = head(test_group, 1), engaged = any(type == "clickthrough")) %>%
  group_by(test_group) %>%
  summarize(bounced = n() - sum(engaged), engaged = sum(engaged)) %>%
  make_table %>%
  beta_binom %>%
  present_bbfit(digits = 2, format = "latex")
