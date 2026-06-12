# Global 

library(shiny)
library(dplyr)
library(readr)
library(stringi)  # Add this - handles Unicode properly

# Helper function to validate card_id format (e.g., "MEX3", "ENG10")
validate_card_id <- function(id) {
  grepl("^[A-Z]{2,4}\\d{1,2}$", toupper(trimws(id)))
}


# make the page labels 
page_labels <- c(
  "Page 1 - Introduction" = "1",
  "Page 2 - Host Nations" = "2",
  "Page 8 - Mexico" = "8",
  "Page 10 - South Africa" = "10",
  "Page 12 - South Korea" = "12",
  "Page 14 - Czechia" = "14",
  "Page 16 - Canada" = "16",
  "Page 18 - Bosnia and Herzegovina" = "18",
  "Page 20 - Qatar" = "20",
  "Page 22 - Switzerland" = "22",
  "Page 24 - Brazil" = "24",
  "Page 26 - Morocco" = "26",
  "Page 28 - Haiti" = "28",
  "Page 30 - Scotland" = "30",
  "Page 32 - USA" = "32",
  "Page 34 - Paraguay" = "34",
  "Page 36 - Australia" = "36",
  "Page 38 - Türkiye" = "38",
  "Page 40 - Germany" = "40",
  "Page 42 - Curaçao" = "42",
  "Page 44 - Ivory Coast" = "44",
  "Page 46 - Ecuador" = "46",
  "Page 48 - Netherlands" = "48",
  "Page 50 - Japan" = "50",
  "Page 52 - Sweden" = "52",
  "Page 54 - Tunisia" = "54",
  "Page 58 - Belgium" = "58",
  "Page 60 - Egypt" = "60",
  "Page 62 - Iran" = "62",
  "Page 64 - New Zealand" = "64",
  "Page 66 - Spain" = "66",
  "Page 68 - Cape Verde" = "68",
  "Page 70 - Saudi Arabia" = "70",
  "Page 72 - Uruguay" = "72",
  "Page 74 - France" = "74",
  "Page 76 - Senegal" = "76",
  "Page 78 - Iraq" = "78",
  "Page 80 - Norway" = "80",
  "Page 82 - Argentina" = "82",
  "Page 84 - Algeria" = "84",
  "Page 86 - Austria" = "86",
  "Page 88 - Jordan" = "88",
  "Page 90 - Portugal" = "90",
  "Page 92 - Congo DR" = "92",
  "Page 94 - Uzbekistan" = "94",
  "Page 96 - Colombia" = "96",
  "Page 98 - England" = "98",
  "Page 100 - Croatia" = "100",
  "Page 102 - Ghana" = "102",
  "Page 104 - Panama" = "104",
  "Page 106 - FIFA World Cup History 1" = "106",
  "Page 108 - FIFA World Cup History 2" = "108"
)

