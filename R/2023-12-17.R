library(tidyverse)
library(readxl)
library(visitalaneysluverds)
library(metill)
library(geomtextpath)
library(ggtext)
library(ggh4x)
theme_set(theme_metill())


path <- "data-raw/net-sundurlidun.xlsx"
d <- read_excel(
  path = path, 
  skip = 4
) |> 
  janitor::clean_names() |> 
  fill(ar, sveitarfelag, malaflokkar, deild_1) |> 
  filter(
    tegund == "Laun og launatengd gjöld"
  ) |> 
  mutate_at(
    vars(sveitarfelag, malaflokkar, deild_1),
    \(x) str_replace(x, "^[0-9 ]*", "")
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
    ar = parse_number(ar),
    total = vnv_convert(total, ar)
  )


mannfjoldi <- read_csv("data/mannfjoldi_svf.csv") |> 
  select(sveitarfelag, ar, mannfjoldi)


p <- d |> 
  filter(
    deild_1 %in% c(
      "Þjónusta við aldraða",
      "Þjónusta við fatlaða",
      "Skrifstofur sveitarfélagsins"
    )
  ) |> 
  inner_join(
    mannfjoldi,
    by = join_by(sveitarfelag, ar)
  ) |> 
  mutate(
    kr_per_ibui = 1e3 * total / mannfjoldi
  ) |> 
  arrange((deild_1)) |> 
  ggplot(aes(ar, kr_per_ibui)) +
  geom_textsmooth(
    aes(group = deild_1, col = deild_1, label = deild_1, hjust = deild_1),
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    span = 0.6,
    linewidth = 0.8, 
    # boxlinewidth = 0,
    # fill = "#faf9f9",
    # label.r = 0,
    # label.padding = -0.1
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated(trunc_lower = 2002, trunc_upper = 2022),
    breaks = seq(2002, 2022, by = 5)
  ) +
  scale_y_continuous(
    labels = label_isk(scale = 1e-3),
    limits = c(0, NA),
    breaks = pretty_breaks(4),
    expand = expansion(),
    guide = guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_hjust_manual(
    values = c(0.2, 0.7, 0.9)
  ) +
  facet_wrap("sveitarfelag") +
  theme(
    legend.position = "none"
  ) +
  labs(
    col = NULL,
    x = NULL,
    y = NULL,
    title = "Launaútgjöld sveitarfélaga tengd þjónustu við fatlaða hafa aukist mikið síðastliðinn áratug",
    subtitle = "Laun og launatengd gjöld sýnd á hvern íbúa sveitarfélaga höfuðborgarsvæðis (fast verðlag 2023)",
    caption = str_c(
      "Reiknað út frá gögnum Sambands íslenskra sveitarfélaga hjá www.samband.is\n",
      "Gögn og kóði: https://github.com/bgautijonsson/sunnudagurtilsveitarfelaga"
    )
  )


ggsave(
  plot = p,
  filename = "Figures/2023-12-17.png",
  width = 8, height = 0.5 * 8, scale = 1.5
)
