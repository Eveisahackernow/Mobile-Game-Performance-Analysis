library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Go through individual data sets

# Daily Activity - Convert platform to lowercase AND remove duplicates
data_daily_activity_clean <- data_daily_activity %>%
  mutate(platform = tolower(platform)) %>%  
  distinct() 

# Process data to get daily active users (DAU)
dau_data <- data_daily_activity_clean %>%
  group_by(activity_date) %>%
  summarise(active_users = n_distinct(user_id)) %>%
  ungroup() %>%
  arrange(activity_date)

# Count unique users per platform
platform_users <- data_daily_activity_clean %>%
  group_by(platform) %>%
  summarise(
    unique_users = n_distinct(user_id),
    .groups = 'drop'
  )

# Calculate Cohort-Based Retention
cohort_data <- data_daily_activity_clean %>%
  mutate(activity_date = as.Date(activity_date)) %>%
  group_by(user_id) %>%
  mutate(
    first_date = min(activity_date), 
    cohort = floor_date(as.Date(first_date), "month") 
  ) %>%
  ungroup() %>%
  mutate(
    day_number = as.numeric(activity_date - first_date) 
  )

# Calculate retention rates
retention_rates <- cohort_data %>%
  group_by(cohort, day_number) %>%
  summarise(
    retained_users = n_distinct(user_id),
    .groups = 'drop'
  ) %>%
  group_by(cohort) %>%
  mutate(
    total_cohort_users = first(retained_users[day_number == 0]), 
    retention_rate = retained_users / total_cohort_users * 100
  ) %>%
  ungroup()

n_day_retention <- retention_rates %>%
  filter(day_number %in% c(1, 7, 30)) %>% 
  select(cohort, day_number, retention_rate) %>%
  pivot_wider(names_from = day_number, values_from = retention_rate, names_prefix = "day_")

platform_retention <- cohort_data %>%
  group_by(cohort, platform, day_number) %>%
  summarise(
    retained_users = n_distinct(user_id),
    .groups = 'drop'
  ) %>%
  group_by(cohort, platform) %>%
  mutate(
    total_cohort_users = first(retained_users),
    retention_rate = retained_users / total_cohort_users * 100
  ) %>%
  filter(day_number == 7) 

# Rolling retention rate
data_daily_activity_clean <- data_daily_activity_clean %>%
  mutate(activity_date = as.Date(activity_date))

rolling_retention <- data_daily_activity_clean %>%
  arrange(user_id, activity_date) %>%
  group_by(user_id) %>%
  mutate(
    activity_date = as.Date(activity_date),

    next_day_active = lead(activity_date) == (activity_date + days(1)),
    
    next_7_day_active = any(
      activity_date %within% 
        interval(first(activity_date), first(activity_date) + days(6))
    )
  ) %>%
  group_by(activity_date) %>%
  summarise(
    dau = n_distinct(user_id),
    next_day_retention = mean(next_day_active, na.rm = TRUE) * 100,
    next_7_day_retention = mean(next_7_day_active, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

# Platform-specific retention curves
retention_rates_platform <- cohort_data %>%
  group_by(cohort, platform, day_number) %>%
  summarise(retained_users = n_distinct(user_id), .groups = 'drop') %>%
  group_by(cohort, platform) %>%
  mutate(
    total_cohort_users = first(retained_users[day_number == 0]),
    retention_rate = retained_users / total_cohort_users * 100
  )

ggplot(retention_rates_platform %>% filter(day_number <= 30), 
       aes(x = day_number, y = retention_rate, color = platform)) +
  geom_line(linewidth = 1) +
  facet_wrap(~cohort) +
  labs(title = "30-Day Retention by Platform and Cohort")

n_day_retention_platform <- retention_rates_platform %>%
  filter(day_number %in% c(1, 7, 30)) %>%
  select(cohort, platform, day_number, retention_rate) %>%
  pivot_wider(names_from = day_number, values_from = retention_rate, 
              names_prefix = "day_")

rolling_retention_platform <- data_daily_activity_clean %>%
  arrange(user_id, activity_date) %>%
  group_by(user_id) %>%
  mutate(
    next_day_active = lead(activity_date) == (activity_date + days(1)),
    next_7_day_active = any(activity_date %within% 
                              interval(first(activity_date), 
                                       first(activity_date) + days(6)))
  ) %>%
  group_by(activity_date, platform) %>%
  summarise(
    dau = n_distinct(user_id),
    next_day_retention = mean(next_day_active, na.rm = TRUE) * 100,
    next_7_day_retention = mean(next_7_day_active, na.rm = TRUE) * 100,
    .groups = 'drop'
  )


# Data Matches - Aggregate matches by date and type
matches_by_date_type <- data_matches %>%
  group_by(activity_date, match_type) %>%
  summarise(
    total_matches = sum(n_matches, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(activity_date, match_type)

# Aggregate data by match type and finish position to see match type difficulty
position_analysis <- data_matches %>%
  group_by(match_type, finish_position) %>%
  summarise(total_matches = sum(n_matches, na.rm = TRUE), .groups = 'drop')

# Aggregate Data by Bots & Finish Position to see if bots difficulty impact
bot_analysis <- data_matches %>%
  group_by(bots, finish_position) %>%
  summarise(
    total_matches = sum(n_matches, na.rm = TRUE),
    .groups = 'drop')

# Data Virtual Currency Purchase - Daily Gold Spending Analysis
daily_gold <- data_virtual_currency_purchases %>%
  group_by(activity_date) %>%
  summarise(
    total_gold_spent = sum(gold_spend, na.rm = TRUE),
    avg_gold_per_user = sum(gold_spend) / n_distinct(user_id)
  )
# By weekday
data_virtual_purchases <- data_virtual_currency_purchases %>%
  mutate(
    activity_date = as.Date(activity_date),
    weekday = lubridate::wday(activity_date, label = TRUE, abbr = FALSE)
  )

daily_gold <- data_virtual_purchases %>%
  group_by(activity_date, weekday) %>%  
  summarise(
    total_gold_spent = sum(gold_spend, na.rm = TRUE),
    .groups = 'drop'
  )
weekday_gold <- daily_gold %>%
  group_by(weekday) %>%
  summarise(
    total_gold = sum(total_gold_spent),
    avg_daily_gold = mean(total_gold_spent)
  ) %>%
  mutate(weekday = factor(weekday, 
                          levels = c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", "Sunday")))
# By product
product_gold_spending <- data_virtual_currency_purchases %>%
  group_by(product_group) %>%
  summarise(
    total_gold_spent = sum(gold_spend, na.rm = TRUE),
    total_purchases = sum(n_purchases, na.rm = TRUE),
    avg_gold_per_purchase = sum(gold_spend) / sum(n_purchases),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_gold_spent))


# Data in App Purchases - daily summary
daily_iap_summary <- data_in_app_purchases %>%
  group_by(activity_date, product_group) %>%
  summarise(
    daily_revenue = sum(dollar_purchase_value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = product_group,
    values_from = daily_revenue,
    values_fill = 0
  ) %>%
  mutate(
    daily_in_app_purchases = rowSums(select(., -activity_date), na.rm = TRUE),
    across(where(is.numeric), ~round(., 2))
  ) %>%
  select(activity_date, daily_in_app_purchases, everything())


# Group and aggregate purchases
iap_aggregated <- data_in_app_purchases %>%
  group_by(user_id, activity_date, product_group) %>%
  summarise(
    total_spend = sum(dollar_purchase_value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(user_id, activity_date)

# Join data to study what might impact iap
matches_aggregated <- data_matches %>%
  group_by(user_id, activity_date) %>%
  summarise(
    match_type_a = sum(n_matches[match_type == "A"], na.rm = TRUE),
    match_type_b = sum(n_matches[match_type == "B"], na.rm = TRUE),
    match_type_c = sum(n_matches[match_type == "C"], na.rm = TRUE),
    total_matches = sum(n_matches, na.rm = TRUE),
    
    bots_0 = sum(n_matches[bots == 0], na.rm = TRUE),
    bots_1 = sum(n_matches[bots == 1], na.rm = TRUE),
    bots_2 = sum(n_matches[bots == 2], na.rm = TRUE),
    bots_3 = sum(n_matches[bots == 3], na.rm = TRUE),
    .groups = 'drop'
  )


virtual_agg <- data_virtual_currency_purchases %>%
  group_by(user_id, activity_date) %>%
  summarise(
    total_gold_spent = sum(gold_spend, na.rm = TRUE),
    .groups = 'drop'
  )

final_joined_data <- iap_aggregated %>%
  full_join(matches_aggregated, by = c("user_id", "activity_date")) %>%
  full_join(virtual_agg, by = c("user_id", "activity_date")) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)), 
    across(where(is.character), ~replace_na(., "No Purchase")) 
  ) %>%
  arrange(user_id, activity_date)

# Platform VS iap
iap_aggregated <- iap_aggregated %>%
  mutate(activity_date = as.Date(activity_date))

iap_joined <- left_join(
  iap_aggregated,
  data_daily_activity_clean %>% select(user_id, activity_date, platform),
  by = c("user_id")
)


# Match amount VS iap correlation analysis
correlation_match_spending <- cor(final_joined_data$total_matches, 
                   final_joined_data$total_spend, 
                   use = "complete.obs")

# Match amount VS iap graph
ggplot(final_joined_data, aes(x = total_matches, y = total_spend)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = paste("Match Activity vs. Spending (r =", round(correlation_match_spending, 2), ")"),
    x = "Total Matches Played",
    y = "Total Spend (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()
# Match amount VS iap by product group graph
ggplot(final_joined_data, aes(x = total_matches, y = total_spend, color = product_group)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~product_group) +
  labs(title = "Spending vs. Matches by Product Type")

# Bots VS iap
bot_analysis <- final_joined_data %>%
  mutate(bot_ratio = (bots_1 + bots_2 + bots_3) / total_matches) %>%
  filter(total_matches > 0) 

ggplot(bot_analysis, aes(x = bot_ratio, y = total_spend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Does Playing with Bots Affect Spending?")
# no significant impact

# Match type VS iap
match_type_impact <- final_joined_data %>%
  mutate(
    pref_type = case_when(
      match_type_a / total_matches > 0.6 ~ "Type A",
      match_type_b / total_matches > 0.6 ~ "Type B",
      match_type_c / total_matches > 0.6 ~ "Type C",
      TRUE ~ "Mixed"
    )
  ) %>%
  group_by(pref_type) %>%
  summarise(
    avg_spend = mean(total_spend),
    avg_matches = mean(total_matches)
  )
# result align with match popularity

