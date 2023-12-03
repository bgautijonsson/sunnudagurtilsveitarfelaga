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
    "Annað" = stodugildi_annad
  ) |> 
  pivot_longer(c(-ar, -sveitarfelag)) 

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

d1 <- d |> 
  mutate(
    type = "hreint"
  ) |> 
  mutate(
    value = value / value[ar == min(ar)],
    .by = c(sveitarfelag, name)
  ) 


d2 <- d |> 
  mutate(
    type = "íbúar"
  ) |> 
  inner_join(
    mannfjoldi,
    by = join_by(sveitarfelag, ar, name)
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
    type = fct_relevel(type, "hreint") |> 
      fct_recode(
        "Hrein fjölgun" = "hreint",
        "Fjölgun á hvern íbúa\n(Í réttum aldurshóp)" = "íbúar"
      ),
    name = fct_relevel(
      name,
      "Leikskólar",
      "Skólar",
      "Annað"
    ) |> 
      fct_recode(
        "Leikskólar\n(0-5 ára)" = "Leikskólar",
        "Skólar\n(6-18 ára)" = "Skólar",
        "Annað\n(Allir íbúar)" = "Annað",
        "Samtals\n(Allir íbúar)" = "Samtals"
      )
  ) |> 
  ggplot(aes(ar, value, col = sveitarfelag)) +
  geom_hline(
    yintercept = 1,
    lty = 2,
    alpha = 0.5,
    linewidth = 0.5
  ) +
  stat_smooth(
    geom = "line",
    linewidth = 1,
    span = 1,
    se = 0,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed")
  ) +
  scale_y_continuous(
    labels = function(x) hlutf(x - 1),
    breaks = c(0.75, 1, 1.25, 1.5, 1.75)
  ) +
  scale_colour_brewer(
    palette = "Set1",
    guide = guide_legend(nrow = 1, label.position = "top")
  ) +
  facet_grid(
    cols = vars(name),
    rows = vars(type),
    scales = "free_y"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "top"
  ) +
  labs(
    title = "Hefur stöðugildum fjölgað hraðar en íbúum hjá sveitarfélögum Höfuðborgarsvæðisins?",
    subtitle = "(%) fjölgun stöðugilda (hrein / á hvern íbúa) miðað við 2018",
    x = NULL,
    y = NULL,
    col = NULL,
    caption = str_c(
      "Reiknað út frá Árbókum Sveitarfélaga hjá www.samband.is\n",
      "Gögn og kóði: https://github.com/bgautijonsson/sunnudagurtilsveitarfelaga"
    )
  )


ggsave(
  plot = p,
  filename = here(
    "Figures", "03-12-2023.png"
  ),
  width = 8, height = 0.5 * 8, scale = 1.6
)
