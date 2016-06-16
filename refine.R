library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange)

events <- tbl_df(readr::read_rds("data/portal-secondary-link-collaprse-test.rds"))

events %<>%
  group_by(session_id) %>%
  arrange(ts) %>%
  mutate(visit = cumsum(type == "landing")) %>%
  ungroup

# In some case we may have a clickthrough recorded before a landing event:
sum(events$visit == 0) # 70 events (0.63% of the dataset)
events %<>% keep_where(visit > 0)

events$prefix <- sub("https?://(www)?(.*)\\.wikipedia\\.org.*", "\\2", events$destination)
events$prefix[grepl("wikipedia.org/search-redirect.php", events$destination, fixed = TRUE)] <- NA
events$`wiki matches a preferred language` <- mapply(function(preferred_langs, wiki) {
  if (is.na(wiki)) {
    return(NA)
  } else {
    return(grepl(wiki, preferred_langs, fixed = TRUE))
  }
}, events$preferred_languages, events$prefix)
events$`wiki matches most preferred language` <- mapply(function(preferred_langs, wiki) {
  if (is.na(wiki)) {
    return(NA)
  } else {
    return(grepl(wiki, strsplit(preferred_langs, ",")[[1]][1], fixed = TRUE))
  }
}, events$preferred_languages, events$prefix)

readr::write_rds(events, "data/portal-secondary-link-collaprse-test-refined.rds", "gz")
