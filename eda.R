library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange)
library(ggplot2)
library(cowplot)

events <- tbl_df(readr::read_rds("data/portal-secondary-link-collaprse-test-refined.rds"))

## Some stats for the report:
events %>%
  dplyr::distinct(session_id, test_group) %>%
  group_by(test_group) %>%
  summarize(sessions = length(unique(session_id)))
# 2560 controls vs 3019 test sessions

## Look at number of sections clicked on
events %>%
  group_by(session_id) %>%
  summarize(`sections clicked on` = length(unique(section_used)) - 1,
            test_group = head(test_group, 1)) %>%
  group_by(test_group, `sections clicked on`) %>%
  summarize(n = n()) %>%
  spread(test_group, n)
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

## Compare number of events per session
events %>%
  keep_where(session_id %in% valid_seshs) %>%
  group_by(session_id) %>%
  summarize(events = n()) %>%
  ungroup %>%
  ggplot(aes(x = events)) +
  geom_density(adjust = 2) +
  scale_x_log10()

## Compare per-session clickthrough rates
events %>%
  keep_where(session_id %in% valid_seshs) %>%
  group_by(session_id) %>%
  summarize(ctr = sum(type == "clickthrough")/n(),
            no_ct = all(type == "landing"),
            test_group = head(test_group, 1)) %>%
  ungroup %>%
  ggplot(aes(x = ctr, color = test_group)) +
  geom_density(adjust = 1.5)

## Compare overall clickthrough rates between the two groups
events %>%
  keep_where(session_id %in% valid_seshs) %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(type == "clickthrough"),
            test_group = head(test_group, 1)) %>%
  group_by(test_group) %>%
  summarize(n = n(), clickthroughs = sum(clickthrough)) %>%
  mutate(prop = clickthroughs/n,
         se = sqrt(((prop)*(1-prop))/n),
         lower = prop + qnorm(0.025) * se,
         upper = prop + qnorm(0.975) * se) %>%
  ungroup %>%
  ggplot(aes(y = prop, x = test_group)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)

temp <- events %>%
  keep_where(session_id %in% nonclickthrough_seshs) %>%
  dplyr::distinct(session_id, test_group) %>%
  group_by(test_group) %>%
  summarize(sessions = n()) %>%
  mutate(n_group = c(2560, 3019),
         # prop_all = sessions/length(unique(events$session_id)),
         prop_group = sessions/n_group,
         type = "non-action")
temp %<>% rbind(mutate(temp, type = "engaged", prop_group = 1 - prop_group))
p1 <- ggplot(data = temp, aes(y = prop_group, x = factor(1), fill = test_group)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  geom_text(aes(y = prop_group + 0.01,
                label = sprintf("%.1f%% of %s",
                                100 * prop_group,
                                polloi::compress(n_group, 1)),
                vjust = -1),
            position = position_dodge(width = 1)) +
  ggtitle("Wikipedia Portal visitors' engagement",
          subtitle = "A/B Test of Collapsing Secondary Links") +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format(),
                     limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  scale_fill_brewer("Group", type = "qual", palette = "Set1") +
  facet_wrap(~type, nrow = 1) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray20", color = "white"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

# Search CTR
ctr_by_section <- rbind(
  events %>%
    keep_where((session_id %in% nonclickthrough_seshs) | (session_id %in% search_seshs)) %>%
    group_by(session_id) %>%
    summarize(test_group = head(test_group, 1), ct = any(type == "clickthrough")) %>%
    group_by(test_group) %>%
    summarize(ctr = sum(ct)/n(), n = n(), successes = sum(ct)) %>%
    mutate(section = "search"), 
  # Primary Link CTR
  events %>%
    keep_where((session_id %in% nonclickthrough_seshs) | (session_id %in% primary_link_clickthrough_seshs)) %>%
    group_by(session_id) %>%
    summarize(test_group = head(test_group, 1), ct = any(type == "clickthrough")) %>%
    group_by(test_group) %>%
    summarize(ctr = sum(ct)/n(), n = n(), successes = sum(ct)) %>%
    mutate(section = "primary links"),
  # Secondary Link CTR
  events %>%
    keep_where((session_id %in% nonclickthrough_seshs) | (session_id %in% secondary_link_clickthrough_seshs)) %>%
    group_by(session_id) %>%
    summarize(test_group = head(test_group, 1), ct = any(type == "clickthrough")) %>%
    group_by(test_group) %>%
    summarize(ctr = sum(ct)/n(), n = n(), successes = sum(ct)) %>%
    mutate(section = "secondary links"),
  # Other Projects CTR
  events %>%
    keep_where((session_id %in% nonclickthrough_seshs) | (session_id %in% other_projects_clickthrough_seshs)) %>%
    group_by(session_id) %>%
    summarize(test_group = head(test_group, 1), ct = any(type == "clickthrough")) %>%
    group_by(test_group) %>%
    summarize(ctr = sum(ct)/n(), n = n(), successes = sum(ct)) %>%
    mutate(section = "other projects"))
ctr_by_section <- cbind(ctr_by_section,
                        binom::binom.confint(ctr_by_section$successes, ctr_by_section$n,
                                             methods = "bayes")[, c("mean", "lower", "upper")])
p2 <- ggplot(ctr_by_section, aes(x = factor(1), y = ctr, fill = test_group)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  geom_pointrange(aes(y = mean, ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  geom_text(aes(y = upper + 0.01,
                label = sprintf("%s (%.1f%%)", polloi::compress(successes, 1), 100*ctr), vjust = -1),
            position = position_dodge(width = 1)) +
  ggtitle("Clickthrough rate by group and section used",
          subtitle = "Each section's CTR was calculated independently of the other sections") +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent_format(),
                     limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  facet_wrap(~section, nrow = 1) +
  # scale_x_discrete("Section", breaks = c("search", "primary links", "secondary links", "other projects")) +
  scale_fill_brewer("Group", type = "qual", palette = "Set1") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray20", color = "white"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
(p <- plot_grid(p1, p2, rel_widths = c(1, 2)))
ggsave("engagement-hires.png", p, path = "figures", width = 14, height = 7, units = "in", dpi = 300)

events %>%
  keep_where(session_id %in% union(secondary_link_clickthrough_seshs, other_projects_clickthrough_seshs)) %>%
  View

events %>%
  keep_where(session_id %in% valid_seshs & !is.na(section_used)) %>%
  keep_where(session_id %in% union(primary_link_clickthrough_seshs, secondary_link_clickthrough_seshs)) %>%
  group_by(test_group) %>%
  summarize(visited_preferred = sum(`wiki matches a preferred language`),
            visited_most_preferred = sum(`wiki matches most preferred language`),
            total = n()) %>%
  ungroup %>%
  mutate(preferred_prop = visited_preferred/total,
         most_preferred_prop = visited_most_preferred/total,
         preferred_se = sqrt((preferred_prop * (1 - preferred_prop))/total),
         most_preferred_se = sqrt((most_preferred_prop * (1 - most_preferred_prop))/total)) %>%
  dplyr::select(-c(total, visited_preferred, visited_most_preferred)) %>%
  gather(var, prop, -test_group) %>%
  mutate(type = sub("(most_)?preferred_((se)|(prop))", "\\2", var),
         visited = sub("((most_)?preferred)_((se)|(prop))", "\\1", var)) %>%
  dplyr::select(-var) %>%
  spread(type, prop) %>%
  ggplot(aes(y = prop, x = visited, fill = test_group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_text(aes(y = prop + qnorm(0.975) * se + 0.05, label = sprintf("%.2f%%", 100 * prop)),
            color = "black", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = prop + qnorm(0.025) * se, ymax = prop + qnorm(0.975) * se),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format(),
                     limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_brewer("Group", type = "qual", palette = "Set1") +
  scale_x_discrete("Visited Wikipedia", limits = c("preferred", "most_preferred"),
                   labels = c("in one of their preferred languages", "in their most preferred language")) +
  ggtitle("The language of the Wikipedia visited via a primary or secondary link") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(legend.position = "bottom")
