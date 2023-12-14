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



p1 <- d |> 
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
    labels = label_hlutf(accuracy = 1),
    expand = expansion()
  ) +
  guides(
    x = guide_axis_truncated(trunc_lower = 2018, trunc_upper = 2022),
    y = "axis_truncated"
  ) +
  coord_cartesian(clip = "off") +
  facet_grid(cols = vars(sveitarfelag), rows = vars(name), scales = "free_y") +
  labs(
    x = NULL,
    y = NULL,
    subtitle = str_c(
      "% starfsfólks með kennara- eða leikskólakennaramenntun"
    )
  ) +
  theme(
    # panel.spacing.x = unit(0.08, "snpc"),
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5),
    plot.subtitle = element_text(face = "bold")
  )






p2 <- d |> 
  mutate(
    Skólar = stodugildi_skola_kenn,
    Leikskólar = stodugildi_leiksk_kenn,
  ) |> 
  select(sveitarfelag, ar, Skólar, Leikskólar) |> 
  pivot_longer(c(-ar, -sveitarfelag)) |> 
  inner_join(
    mannfjoldi,
    by = join_by(sveitarfelag, name, ar)
  ) |> 
  mutate(
    barn_per_kennari = mannfjoldi / value
  ) |> 
  ggplot(aes(ar, barn_per_kennari)) +
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
    breaks = c(2018:2022),
    expand = expansion(),
    limits = c(2018, 2023.5)
  ) +
  scale_y_continuous(
    expand = expansion()
  ) +
  guides(
    x = guide_axis_truncated(trunc_lower = 2018, trunc_upper = 2022),
    y = "axis_truncated"
  ) +
  coord_cartesian(clip = "off") +
  facet_grid(cols = vars(sveitarfelag), rows = vars(name), scales = "free_y") +
  labs(
    x = NULL,
    y = NULL,
    title = NULL,
    subtitle = str_c(
      "Fjöldi barna á hvert stöðugildi kennaramenntaðra"
    )
  ) +
  theme(
    # panel.spacing.x = unit(0.08, "snpc"),
    strip.text.x = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.background.x = element_blank(),
    plot.margin = margin(t = 0, r = 5, b = 5, l = 5),
    plot.subtitle = element_text(face = "bold")
  )
  

p <- p1 + 
  p2 + 
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Hefur kennaramenntuðu starfsfólki fjölgað í takt við vonir?",
    subtitle = str_c(
      "Almennt er lægri % stöðugilda með kennararéttindi árið 2023 en 2018 og börnum á hvern kennaramenntaðan hefur helst fækkað í Hafnarfirði"
    )
  )

p

ggsave(
  plot = p,
  filename = here(
    "Figures", "10-12-2023.png"
  ),
  width = 8, height = 0.621 * 8, scale = 1.6
)

