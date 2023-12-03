library(tidyverse)
library(readxl)
library(here)
library(glue)
library(patchwork)
library(hagstofa)
library(scales)
library(metill)
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
  ) |> 
  rename(
    "Samtals" = stodugildi_total,
    "Skólar" = stodugildi_skola,
    "Leikskólar" = stodugildi_leikskola,
    "Annað (Ekki skólar eða leikskólar)" = stodugildi_annad
  ) |> 
  pivot_longer(c(-ar, -sveitarfelag)) |> 
  mutate(
    name = fct_relevel(
      name,
      "Leikskólar",
      "Skólar",
      "Annað (Ekki skólar eða leikskólar)"
    )
  )

mannfjoldi <- read_csv(here("data", "mannfjoldi_svf.csv"))


d1 <- d |> 
  mutate(
    type = "Hrein fjölgun"
  ) |> 
  mutate(
    value = value / value[ar == min(ar)],
    .by = c(sveitarfelag, name)
  ) 


d2 <- d |> 
  mutate(
    type = "Fjölgun á hvern íbúa"
  ) |> 
  inner_join(
    mannfjoldi,
    by = join_by(sveitarfelag, ar)
  ) |> 
  mutate(
    value = value / mannfjoldi,
    value = value / value[ar == min(ar)],
    .by = c(sveitarfelag, name)
  )  


p <- d1 |> 
  bind_rows(
    d2
  ) |>
  mutate(
    type = fct_relevel(type, "Hrein fjölgun")
  ) |> 
  ggplot(aes(ar, value, col = sveitarfelag)) +
  geom_hline(
    yintercept = 1,
    lty = 2,
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 1,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed")
  ) +
  scale_y_continuous(
    labels = function(x) hlutf(x - 1),
    breaks = breaks_pretty(5)
  ) +
  scale_colour_brewer(
    palette = "Set1",
    guide = guide_legend(nrow = 1, label.position = "top")
  ) +
  facet_grid(
    cols = vars(name),
    rows = vars(type)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "top"
  ) +
  labs(
    title = "Stöðugildum hefur fjölgað hraðar en íbúum hjá flestum sveitarfélögum á Höfuðborgarsvæðinu",
    subtitle = "(%) fjölgun stöðugilda (hrein / á hvern íbúa) miðað við 2018",
    x = NULL,
    y = NULL,
    col = NULL,
    caption = "Reiknað út frá Árbókum Sveitarfélaga hjá www.samband.is"
  )


ggsave(
  plot = p,
  filename = here(
    "Figures", "03-12-2023.png"
  ),
  width = 8, height = 0.5 * 8, scale = 1.6
)




p
