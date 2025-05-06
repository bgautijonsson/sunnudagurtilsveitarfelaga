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
  select(
    ar,
    sveitarfelag,
    stodugildi = stodugildi_total
  )

mannfjoldi <- read_csv(here("data", "mannfjoldi_svf.csv")) |> 
  select(sveitarfelag, ar, mannfjoldi)


d_2024 <- tribble(
  ~sveitarfelag, ~stodugildi,
  "Reykjavíkurborg", 8606,
  "Garðabær", 948,
  "Kópavogsbær", 2023,
  "Mosfellsbær", 841,
  "Seltjarnarnesbær", 258,
  "Hafnarfjarðarkaupstaður", 1877
)

plot_dat <- d |> 
  bind_rows(
    d_2024 |> 
      mutate(ar = 2024)
  ) |> 
  inner_join(
    mannfjoldi,
    by = join_by(sveitarfelag, ar)
  ) |> 
  mutate(
    value = stodugildi / mannfjoldi
  ) 



span <- 0.7

p <- plot_dat |>
  ggplot(aes(ar, 1000 * value)) +
  geom_point(
    col = "black",
    size = 2
    ) +
  stat_smooth(
    data = ~ rename(.x, svf = sveitarfelag),
    geom = "line",
    linewidth = 1,
    span = span,
    se = 0,
    col = "grey",
    alpha = 0.3,
    aes(group = svf)
  ) +
  stat_smooth(
    aes(group = sveitarfelag),
    col = "black",
    geom = "line",
    linewidth = 1,
    span = span,
    se = 0
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1",
    guide = guide_legend(nrow = 1, label.position = "top")
  ) +
  facet_wrap(
    vars(sveitarfelag)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "top",
    panel.spacing.x = unit(0.5, "cm")
  ) +
  labs(
    title = "Hefur stöðugildum fjölgað hraðar en íbúum hjá sveitarfélögum höfuðborgarsvæðisins?",
    subtitle = "Stöðugildi sveitarfélaga á 1.000 íbúa (2018 - 2024)",
    x = NULL,
    y = NULL,
    col = NULL,
    caption = str_c(
      "Reiknað út frá Árbókum Sveitarfélaga hjá www.samband.is\n",
      "Gögn og kóði: https://github.com/bgautijonsson/sunnudagurtilsveitarfelaga"
    )
  )

p

ggsave(
  plot = p,
  filename = here(
    "Figures", "stodugildi_a_ibua.png"
  ),
  width = 8, height = 0.5 * 8, scale = 1.6
)

