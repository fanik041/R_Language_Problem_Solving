library(tidyr)
library(starschemar)
install.packages("devtools")
library(devtools)

dput(colnames(mrs_age))

c(
  "Reception Year",
  "Reception Week",
  "Reception Date",
  "Data Availability Year",
  "Data Availability Week",
  "Data Availability Date",
  "Year",
  "WEEK",
  "Week Ending Date",
  "REGION",
  "State",
  "City",
  "Age Range",
  "Deaths"
)

dm <- dimensional_model() %>%
  define_dimension(name = "When",
                   attributes = c("Week Ending Date",
                                  "WEEK",
                                  "Year")) %>%
  define_dimension(name = "When Available",
                   attributes = c("Data Availability Date",
                                  "Data Availability Week",
                                  "Data Availability Year")) %>%
  define_dimension(name = "Where",
                   attributes = c("REGION",
                                  "State",
                                  "City")) %>%
  define_dimension(name = "Who",
                   attributes = c("Age Range"))

# }

print(dm)

ft <- read.csv("Question 1/Deaths_in_122_U.S._cities_-_1962-2016._122_Cities_Mortality_Reporting_System.csv")
# columns to consider in the definition
dput(colnames(ft))
# c("Year", "WEEK", "Week Ending Date", "REGION", "State", "City",
# "Pneumonia and Influenza Deaths", "All Deaths", "Other Deaths"
# )

dm <- dimensional_model() %>%
  define_fact(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "Other Deaths"
    ),
  ) %>%
  define_dimension(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  ) %>%
  define_dimension(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  )

st <- star_schema(ft, dm) %>%
  snake_case() %>%
  character_dimensions(
    NA_replacement_value = "Unknown",
    length_integers = list(week = 2)
  )

read.csv("Question 1/bank-additional-full.csv")

