start_date <- as.Date("2016-06-07")
end_date <- Sys.Date()-1
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

## Validation
events %>%
  group_by(test_group) %>%
  summarize(sessions = length(unique(session_id))) %>%
  mutate(proportions = sessions/sum(sessions))
events %>%
  group_by(session_id) %>%
  summarize(buckets = length(unique(test_group))) %>%
  arrange(desc(buckets))

## Locally:
dir.create("data")
system("scp stat2:/home/bearloga/portal-lang-detect-test.rds data/")
