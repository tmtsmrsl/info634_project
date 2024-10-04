library(tidyverse)

# Select unique response_id for 2023 from the survey main header. We only use 
# data from 2023 to avoid anomalies caused by COVID. We will use this to filter 
# all the other datasets.
response_ids <- read.csv("views/survey_main_header.csv") %>% 
  filter(year == 2023) %>%
  select(response_id) %>% 
  distinct()

# This csv contains the participation of each response_id in Maori cultural activity. 
# There could be duplicated response_id if the respondent participated in more 
# than one type of activity. We will make a classification whether the respondent 
# has experienced at least one Maori cultural activity and this will be our DV.
experienced_df <- read.csv("views/maori_cultural_experience.csv") %>% 
  filter(response_id %in% response_ids$response_id) %>%
  group_by(response_id) %>%
  summarise(experienced_maori = ifelse(any(experience != "None of these"), "Yes", "No"))

# This dataset covers with whom the tourist travelled with. Note that the 
# response_id could be duplicated because one respondent can travel with more 
# than one type of party. We will expand the travelled_with column to one row 
# per response_id.
tp_df <- read.csv("views/travel_party.csv") %>% 
  filter(response_id %in% response_ids$response_id) %>% 
  mutate(
    travelled_alone = ifelse(
      travelled_with == "No one, I was on my own", 1, 0),
    travelled_with_partner = ifelse(
      travelled_with == "My husband, wife or partner", 1, 0),
    travelled_with_child_over_15 = ifelse(
      travelled_with == "Child/children aged 15 or older", 1, 0),
    travelled_with_child_under_15 = ifelse(
      travelled_with == "Child/children aged under 15", 1, 0),
    travelled_with_adult_family = ifelse(
      travelled_with == "Other adult family / relative", 1, 0),
    travelled_with_adult_non_family = ifelse(
      travelled_with == "Other adult(s) who are not family / relatives", 1, 0)) %>% 
  select(-travelled_with) %>%
  group_by(response_id) %>%
  # we use sum here just to merge the duplicated response_id, could use max as well
  summarise(across(starts_with("travelled"), sum)) %>% 
  # convert 1 to "Yes" and 0 to "No"
  mutate(across(starts_with("travelled"),  ~ ifelse(. == 1, "Yes", "No"))) %>% 
  # group travel with person above 15 together  to reduce cardinality
  mutate(travelled_with_other_above_15 = 
           ifelse(
             travelled_with_adult_family == "Yes" | 
               travelled_with_adult_non_family == "Yes" | 
               travelled_with_partner == "Yes" | 
               travelled_with_child_over_15 == "Yes", 
             "Yes", "No")) %>%
  select(-c(travelled_with_adult_family, travelled_with_adult_non_family, 
            travelled_with_partner, travelled_with_child_over_15)) %>% 
  # drop the travel_alone column because we can deduce it from the other two columns
  select(-travelled_alone)


# This dataset contains the information if the tourist drive themselves in NZ.
transport_df <- read.csv("views/self_transport.csv") %>% 
  filter(response_id %in% response_ids$response_id) %>% 
  select(response_id, drive_yourself) %>% 
  # there are duplicates in raw data
  distinct()

# Select some of the important columns from the survey main header.
cols = c("response_id", "qtr", "country_of_residence_group", "gender", 
         "first_nz_trip", "purpose_of_visit_main", "no_days_in_nz", "age_range", 
         "visited_ni", "visited_si", "treated_spend")
smh_df <- read.csv("views/survey_main_header.csv") %>% 
  filter(response_id %in% response_ids$response_id) %>% 
  select(cols) 

# convert qtr to factor
smh_df <- smh_df %>% 
  mutate(qtr = as.factor(qtr))

# clean the visited_ni and visited_si columns
smh_df <- smh_df %>%
  # The visited_ni and visited_si columns values are not consistent. We will
  # standardize the visited_ni and visited_si columns to "Yes" and "No".
  mutate(visited_ni = case_when(visited_ni == "Visited the North Island" ~ "Yes",
                              visited_ni =="1" ~ "Yes",
                              TRUE ~ "No"),
       visited_si = case_when(visited_si == "Visited the South Island" ~ "Yes",
                              visited_si == "1" ~ "Yes",
                              TRUE ~ "No")) %>% 
  # If the value is "No" for both visited_ni and visited_si, replace them with NA. 
  # We assume that the respondent did not answer the question.
  mutate(visited_ni = ifelse(visited_ni == "No" & visited_si == "No", NA, visited_ni),
         visited_si = ifelse(visited_ni == "No" & visited_si == "No", NA, visited_si)) %>% 
  # make new column called visited_place to represent information from both columns
  mutate(visited_island = case_when(visited_ni == "Yes" & visited_si == "Yes" ~ "Both",
                                    visited_ni == "Yes" & visited_si == "No" ~ "North",
                                    visited_ni == "No" & visited_si == "Yes" ~ "South",
                                    TRUE ~ NA)) %>% 
  select(-c(visited_ni, visited_si))

# clean the age_range column by converting it to numerical
smh_df <- smh_df %>%
  mutate(age_range = case_when(
    # choose the middle number for the age ranges below
    age_range == "20 - 24" ~ 22,
    age_range == "25 - 29" ~ 27,
    age_range == "30 - 34" ~ 32,
    age_range == "35 - 39" ~ 37,
    age_range == "40 - 44" ~ 42,
    age_range == "45 - 49" ~ 47,
    age_range == "50 - 54" ~ 52,
    age_range == "55 - 59" ~ 57,
    age_range == "60 - 64" ~ 62,
    age_range == "65 - 69" ~ 67,
    age_range == "70 - 74" ~ 72,
    # choose a reasonable number for the age ranges below
    age_range == "75 or older" ~ 77,
    age_range == "Under 20" ~ 10,
    TRUE ~ NA
  ))

# clean the purpose_of_visit_main column to reduce cardinality
smh_df <- smh_df %>%
  mutate(purpose_of_visit_main = ifelse(
    purpose_of_visit_main == "Conference / convention", 
    "Other", 
    purpose_of_visit_main)
    )

# clean the country_of_residence_group column to reduce cardinality
smh_df <- smh_df %>%
  # the regrouping is based on the region and also the mean of experience_maori
  mutate(country_of_residence_group = case_when(
    country_of_residence_group %in% c("Australia", "Rest of Oceania") ~ "Oceania",
    country_of_residence_group %in% c("UK", "Germany", "Rest of Europe") ~ "Europe",
    country_of_residence_group %in% c("Japan", "China", "Korea, Republic of", "Rest of Asia") ~ "Asia",
    country_of_residence_group %in% c("USA", "Canada", "Rest of Americas") ~ "Americas",
    country_of_residence_group == "Africa and Middle East" ~ "Africa and Middle East",
    TRUE ~ NA
  ))

# make new column called spend_per_day
smh_df <- smh_df %>% 
  # treated_spend is the total spend per visitor in their whole trip
  mutate(spend_per_day = treated_spend / no_days_in_nz)

# join all the intermediary datasets
merged_df <- response_ids %>% 
  left_join(experienced_df, by = "response_id") %>% 
  left_join(tp_df, by = "response_id") %>% 
  left_join(transport_df, by = "response_id") %>% 
  left_join(smh_df, by = "response_id") 


# drop rows with uneligible values
merged_df <- merged_df %>% 
  na.omit() %>%
  filter(gender %in% c("Male", "Female")) %>%
  filter(drive_yourself %in% c("Yes", "No"))

# save as csv
write.csv(merged_df, "merged_data.csv", row.names = FALSE)
