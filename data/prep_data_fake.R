packages <- c("tidyverse", "psych")
lapply(packages, require, character.only = TRUE)

possible_nodes <- names(bfi)[1:(ncol(bfi) - 3)]
set.seed(352)
nodes <- sample(possible_nodes, size = 18, replace = FALSE)
Ps <- sample(1:nrow(bfi), size = 650)
fake_data <- bfi %>% 
  select(any_of(nodes), education, gender, age) %>% 
  slice(Ps)
node_names <- c(
  "Anger",
  "AuthInd",
  "BlackWhite",
  "Disgust",
  "Fluency",
  "Suffer",
  "Upset",
  "Wrong",
  "Policy"
)
new_names <- c(
  paste(node_names, "_Smoking", sep = ""), paste(node_names, "_Guns", sep = ""),
  "education",
  "gender", 
  "age"
  )
names(fake_data) <- new_names

# add demographics labels to fake_data
gender_labels <- tibble(
  gender = 1:6,
  gen_label = c(
    "Male",
    "Female",
    "Transgender",
    "Non-Binary",
    "Specified Another Identity",
    "Specified Another Identity" # fake_data_3 had 6 instead of 5
  )
)
race_labels <- tibble( # ask about 8
  race = 1:8,
  race_label = c(
    "White",
    "Hispanic",
    "Black or African-American",
    "Asian",
    "Native American",
    "Pacific Islander",
    "Specified Another Race or Ethnicity",
    "Specified Another Race or Ethnicity" # until jordan gets back to me
  )
)
edu_labels <- tibble(
  education = 1:10,
  edu_label = c(
    "Less than High School",
    "Some High School",
    "High School/GED",
    "Some College",
    "2-Year Degree (Associate's)",
    "4-Year Degree (Bachelor's)",
    "Some Graduate School",
    "Master's Degree",
    "Professional Degree (JD, MD)",
    "Doctoral Degree"
  )
)
# add demographics labels to fake_data
gender_labels <- tibble(
    gender = 1:6,
    gen_label = c(
        "Male",
        "Female",
        "Transgender",
        "Non-Binary",
        "Specified Another Identity",
        "Specified Another Identity" # fake_data_3 had 6 instead of 5
    )
)
race_labels <- tibble( # ask about 8
    race = 1:8,
    race_label = c(
        "White",
        "Hispanic",
        "Black or African-American",
        "Asian",
        "Native American",
        "Pacific Islander",
        "Specified Another Race or Ethnicity",
        "Specified Another Race or Ethnicity" # until jordan gets back to me
    )
)
edu_labels <- tibble(
    education = 1:10,
    edu_label = c(
        "Less than High School",
        "Some High School",
        "High School/GED",
        "Some College",
        "2-Year Degree (Associate's)",
        "4-Year Degree (Bachelor's)",
        "Some Graduate School",
        "Master's Degree",
        "Professional Degree (JD, MD)",
        "Doctoral Degree"
    )
)
fake_data <- fake_data %>% 
  mutate(race = sample(
    1:8, 
    size = nrow(.), 
    replace = TRUE, 
    prob = c(0.5, 0.15, 0.2, 0.1, 0.02, 0.01, 0.01, 0.01)
    ))

fake_data <- left_join(fake_data, gender_labels)
fake_data <- left_join(fake_data, race_labels)
fake_data <- left_join(fake_data, edu_labels) 
fake_data <- fake_data %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(
    cols = c(ends_with("smoking"), ends_with("guns")),
    names_to = "issue",
    values_to = "response"
      ) %>% 
  separate(issue, into = c("node", "issue")) %>% 
  pivot_wider(names_from = node, values_from = "response") 

write_csv(fake_data, here("dissertation", "data", "fake_data.csv"))
