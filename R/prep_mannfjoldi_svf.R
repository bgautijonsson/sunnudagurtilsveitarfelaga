library(tidyverse)
library(hagstofa)
library(here)

mannfjoldi <- hg_data(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
) |> 
  filter(
    Kyn == "Alls",
    Aldur == "Alls",
    Sveitarfélag != "Alls"
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(mannfjoldi = mannfjoldi_eftir_sveitarfelagi_kyni_og_aldri_1_januar_1998_2023) |> 
  mutate(ar = parse_number(ar)) |> 
  select(-kyn, -aldur)

mannfjoldi |> 
  write_csv(
    here("data", "mannfjoldi_svf.csv")
  )

mannfjoldi_leiksk <- hg_data(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
) |> 
  filter(
    Kyn == "Alls",
    Sveitarfélag != "Alls",
    Aldur %in% c(
      "Á 1. ári",
      " 1 árs",
      str_c(" ", 2:5, " ára")
      )
    ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(mannfjoldi = mannfjoldi_eftir_sveitarfelagi_kyni_og_aldri_1_januar_1998_2023) |> 
  mutate(ar = parse_number(ar)) |> 
  select(-kyn, -aldur) |> 
  summarise(
    mannfjoldi_leiksk = sum(mannfjoldi),
    .by = c(sveitarfelag, ar)
  )

mannfjoldi_skol <- hg_data(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
) |> 
  filter(
    Kyn == "Alls",
    Sveitarfélag != "Alls",
    Aldur %in% str_c(" ", 6:18, " ára")
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(mannfjoldi = mannfjoldi_eftir_sveitarfelagi_kyni_og_aldri_1_januar_1998_2023) |> 
  mutate(ar = parse_number(ar)) |> 
  select(-kyn, -aldur) |> 
  summarise(
    mannfjoldi_skol = sum(mannfjoldi),
    .by = c(sveitarfelag, ar)
  )


mannfjoldi |> 
  inner_join(
    mannfjoldi_leiksk
  ) |> 
  inner_join(
    mannfjoldi_skol
  ) |> 
  write_csv(
    here("data", "mannfjoldi_svf.csv")
  )
