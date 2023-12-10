library(tidyverse)
library(readxl)
library(here)
library(glue)
library(patchwork)
library(hagstofa)
library(scales)
library(metill)
library(ggh4x)
theme_set(theme_metill())

d <- read_csv(
  here("data", "arbok_stodugildi.csv")
) |> 
  filter(
    sveitarfelag %in% c(
      "Reykjavíkurborg",
      "Garðabær",
      "Kópavogsbær",
      "Hafnarfjarðarkaupstaður",
      "Mosfellsbær",
      "Seltjarnarnesbær"
    )
  )


mannfjoldi <- read_csv(here("data", "mannfjoldi_svf.csv")) |> 
  mutate(
    Samtals = mannfjoldi
  ) |> 
  rename(
    Skólar = mannfjoldi_skol,
    Leikskólar = mannfjoldi_leiksk,
    "Annað" = mannfjoldi
  ) |> 
  pivot_longer(c(-sveitarfelag, -ar), values_to = "mannfjoldi")



p <- d |> 
  mutate(
    Skólar = stodugildi_skola_kenn / stodugildi_skola,
    Leikskólar = stodugildi_leiksk_kenn / stodugildi_leikskola
  ) |> 
  select(sveitarfelag, ar, Skólar, Leikskólar) |> 
  pivot_longer(c(-ar, -sveitarfelag)) |> 
  ggplot(aes(ar, value)) +
  stat_smooth(
    data = ~rename(.x, svf = sveitarfelag),
    geom = "line",
    span = 0.9,
    se = 0,
    aes(group = svf),
    alpha = 0.1,
    linewidth = 0.3
  ) +
  stat_smooth(
    geom = "line",
    span = 0.9,
    se = 0,
    linewidth = 1.5,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed")
  ) +
  scale_x_continuous(
    breaks = c(2018:2023),
    expand = expansion(),
    limits = c(2018, 2023.5)
  ) +
  scale_y_continuous(
    labels = label_hlutf(accuracy = 1)
  ) +
  guides(
    x = "axis_truncated",
    y = "axis_truncated"
  ) +
  facet_grid(cols = vars(sveitarfelag), rows = vars(name), scales = "free_y") +
  labs(
    x = NULL,
    y = "% með kennaramenntun",
    title = "Almennt er lægri hlutfall starfsfólks með kennararéttindi en fyrir 5 árum",
    subtitle = "Hlutfall starfsfólks með kennara- eða leikskólakennaramenntun í sveitarfélögum Höfuðborgarsvæðis (2018 - 2023)"
  ) +
  theme(
    panel.spacing.x = unit(0.05, "snpc"),
    strip.text = element_text(size = 11),
    axis.text.x = element_text(size = 8)
  )

ggsave(
  plot = p,
  filename = here(
    "Figures", "10-12-2023.png"
  ),
  width = 8, height = 0.4 * 8, scale = 1.6
)


