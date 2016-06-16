start_date <- as.Date("2016-06-06")
end_date <- as.Date("2016-06-14")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from ", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                           event_session_id AS session,
                           event_destination AS destination,
                           event_event_type AS type,
                           event_section_used AS section_used,
                           event_accept_language AS preferred_languages,
                           event_cohort AS test_group,
                           timestamp AS ts,
                           userAgent AS user_agent",
                           date = date,
                           table = "WikipediaPortal_14377354",
                           conditionals = "event_cohort IN('lang_dropdown-a', 'lang_dropdown-b')")
  return(data)
}))
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()

names(events)[2] <- "session_id"

devices <- uaparser::parse_agents(events$user_agent, fields = "device")
spider_session_ids <- unique(events$session_id[devices == "Spider"])
events <- events[!(events$session_id %in% spider_session_ids), ]
rm(devices, spider_session_ids)
events$user_agent <- NULL

events$test_group <- ifelse(events$test_group == "lang_dropdown-a", "A (Control)", "B (Test)")

## Process Accept-Language field:
preferred_langs <- dplyr::distinct(events[, c("session_id", "preferred_languages")])
bad_session_ids <- unique(preferred_langs[duplicated(preferred_langs$session_id), "session_id"])
bad_sessions <- events[events$session_id %in% bad_session_ids, ]
preferred_langs <- preferred_langs[!(preferred_langs$session_id %in% bad_session_ids), ]
events <- events[!(events$session_id %in% bad_session_ids), ]
events$preferred_languages <- NULL
rm(bad_session_ids)
# install.packages(c("NLP", "purrr"))
library(purrr)
accept_language <- preferred_langs$preferred_languages %>%
  strsplit(",") %>%
  map_df(.f = function(lang_id) {
    langs <- tryCatch(unname(unlist(NLP::parse_IETF_language_tag(lang_id, expand = TRUE))),
                      error = function(e) { return(NA) }, finally = NA)
    if (is.na(langs[1])) {
      return(data.frame(V1 = NA, V2 = NA, V3 = NA, V4 = NA))
    }
    num_langs_pre_en <- which(langs == "English") - 1
    return(data.frame(V1 = ifelse(length(num_langs_pre_en) == 0, NA, num_langs_pre_en),
                      V2 = langs[1],
                      V3 = length(num_langs_pre_en) > 0,
                      V4 = length(lang_id),
                      stringsAsFactors = FALSE))
  }, .id = NULL) %>%
  set_names(c("Number of languages preceeding English", "Primary language", "Includes English", "Number of Accept-Languages"))
preferred_langs <- cbind(preferred_langs, accept_language)
rm(accept_language)
events <- dplyr::left_join(events[, c("date", "ts", "session_id", "test_group", "type", "section_used", "destination")],
                           preferred_langs, by = "session_id")
rm(preferred_langs)
events <- events[!is.na(events$`Primary language`), ]

readr::write_rds(events, "~/portal-secondary-link-collaprse-test.rds", "gz")

## Locally:
dir.create("data")
system("scp stat2:/home/bearloga/portal-secondary-link-collaprse-test.rds data/")
