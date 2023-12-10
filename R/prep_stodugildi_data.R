library(tidyverse)
library(readxl)
library(here)
library(glue)

read_stodugildi_total <- function(year) {
  path <- here("data-raw", "arbok", "rbok-{year}-toflur.xlsx") |> glue()
  read_xlsx(path, sheet = "Tafla 17", skip = 4) |> 
    slice(-1) |> 
    rename("svfn" = 1, "sveitarfelag" = 2, stodugildi_total = "stöðugildi") |> 
    select(sveitarfelag, stodugildi_total) |> 
    mutate(ar = year)
}

read_stodugildi_skola <- function(year) {
  path <- here("data-raw", "arbok", "rbok-{year}-toflur.xlsx") |> glue()
  read_excel(
    path, 
    sheet = "Tafla 18", 
    range = "A7:N356"
  ) |> 
    select(
      sveitarfelag = 3,
      stodugildi_skola_kenn = 11,
      stodugildi_skola_kenn_an_rettinda = 12,
      stodugildi_skola = 14
    ) |> 
    filter(
      str_detect(sveitarfelag, "[Ss]amtals")
    ) |> 
    mutate(
      sveitarfelag = str_replace(sveitarfelag, " [Ss]amtals", "") |> 
        str_replace("^[0-9]+ ", "")
    ) |> 
    mutate(
      ar = year
    )
}

read_stodugildi_leikskola <- function(year) {
  path <- here("data-raw", "arbok", "rbok-{year}-toflur.xlsx") |> glue()
  read_excel(
    path, 
    sheet = "Tafla 19", 
    range = "A8:Q406"
  ) |> 
    select(
      sveitarfelag = 3,
      stodugildi_leiksk_kenn = 14,
      stodugildi_leiksk_onnur_mennt = 15,
      stodugildi_leikskola = 17
    ) |> 
    filter(
      str_detect(sveitarfelag, "samtals")
    ) |> 
    mutate(
      sveitarfelag = str_replace(sveitarfelag, " samtals", "") |> 
        str_replace("^[0-9]+ ", "")
    ) |> 
    mutate(
      ar = year
    )
}

d_leikskolar <- map_dfr(2018:2023, read_stodugildi_leikskola) |> 
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjavíkurborg") ~ "Reykjavíkurborg",
      str_detect(sveitarfelag, "Hafnarfj") ~ "Hafnarfjarðarkaupstaður",
      str_detect(sveitarfelag, "Seltjar") ~ "Seltjarnarnesbær",
      TRUE ~ sveitarfelag
    )
  )


d_total <- map_dfr(2018:2023, read_stodugildi_total) |> 
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjavíkurborg") ~ "Reykjavíkurborg",
      str_detect(sveitarfelag, "Hafnarfj") ~ "Hafnarfjarðarkaupstaður",
      TRUE ~ sveitarfelag
    )
  )



d_skolar <- map_dfr(2018:2023, read_stodugildi_skola) |> 
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjavíkurborg") ~ "Reykjavíkurborg",
      str_detect(sveitarfelag, "Hafnarfj") ~ "Hafnarfjarðarkaupstaður",
      str_detect(sveitarfelag, "Seltjar") ~ "Seltjarnarnesbær",
      TRUE ~ sveitarfelag
    )
  )


d_total |> 
  inner_join(
    d_skolar,
    by = join_by(sveitarfelag, ar)
  ) |> 
  inner_join(
    d_leikskolar,
    by = join_by(sveitarfelag, ar)
  ) |> 
  mutate(
    stodugildi_annad = stodugildi_total - stodugildi_skola - stodugildi_leikskola,
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjavíkurb") ~ "Reykjavíkurborg",
      str_detect(sveitarfelag, "Hafnarfj") ~ "Hafnarfjarðarkaupstaður",
      TRUE ~ sveitarfelag
    )
  ) |> 
  write_csv(
    here("data", "arbok_stodugildi.csv")
  )
