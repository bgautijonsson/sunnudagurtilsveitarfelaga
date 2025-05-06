library(tidyverse)
library(readxl)
library(here)
library(glue)
library(metill)
library(ggh4x)
theme_set(theme_metill())

read_stodugildi_skola <- function(year) {
  path <- here("data-raw", "arbok", "rbok-{year}-toflur.xlsx") |> glue()
  read_excel(
    path, 
    sheet = "Tafla 18", 
    range = "A7:N356"
  ) |> 
    select(
      sveitarfelag = 3,
      skoli = 4,
      nemendur = 10,
      stodugildi_skola_kenn = 11,
      stodugildi_skola_kenn_an_rettinda = 12,
      stodugildi_skola = 14
    ) |> 
    fill(sveitarfelag, .direction = "down") |> 
    drop_na() |> 
    mutate(
      sveitarfelag = str_replace(sveitarfelag, " [Ss]amtals", "") |> 
        str_replace("^[0-9]+ ", "")
    ) |> 
    mutate(
      ar = year - 1
    )
}



d_skolar <- map_dfr(2018:2023, read_stodugildi_skola) |> 
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjavíkurborg") ~ "Reykjavíkurborg",
      str_detect(sveitarfelag, "Hafnarfj") ~ "Hafnarfjarðarkaupstaður",
      str_detect(sveitarfelag, "Seltjar") ~ "Seltjarnarnesbær",
      TRUE ~ sveitarfelag
    )
  )

p <- d_skolar |> 
  summarise(
    n = sum(nemendur),
    .by = c(sveitarfelag, ar)
  ) |> 
  filter(
    sveitarfelag %in%
      c(
        "Reykjavíkurborg",
        "Kópavogsbær",
        "Garðabær",
        "Seltjarnarnesbær",
        "Mosfellsbær",
        "Hafnarfjarðarkaupstaður"
      )
  ) |> 
  mutate(
    value = n / n[ar == min(ar)],
    .by = sveitarfelag
  ) |> 
  ggplot(aes(ar, value)) +
  geom_hline(
    yintercept = 1, 
    lty = 2,
    linewidth = 0.5,
    alpha = 0.7
  ) +
  geom_line(
    data = ~ rename(.x, svf = sveitarfelag),
    aes(group = svf),
    alpha = 0.2, 
    linewidth = 0.3
  ) +
  geom_line(
    linewidth = 1.5
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_extended(6),
    labels = \(x) hlutf(x - 1),
    guide = guide_axis_truncated()
  ) +
  facet_wrap("sveitarfelag", scales = "free") +
  labs(
    x = NULL,
    y = NULL,
    title = "Breyting á fjölda grunnskólanemenda frá 2017",
    subtitle = "Mikill munur er á fjölgun grunnskólanemenda eftir sveitarfélögum Höfuðborgarsvæðis",
    caption = str_c(
      "Unnið úr Árbókum Sveitarfélaga hjá Sambandi Íslenskra Sveitarfélaga", "\n",
      "Gögn og kóði: https://github.com/bgautijonsson/sunnudagurtilsveitarfelaga"
    )
  )


p


ggsave(
  plot = p,
  filename = "Figures/grunnskolanemendur.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)




