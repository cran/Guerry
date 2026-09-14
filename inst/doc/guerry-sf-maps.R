## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  dpi = 96
)

## ----load---------------------------------------------------------------------
library(Guerry)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
data(gfrance85)
data(Guerry_ranks)

## ----st_as_sf-----------------------------------------------------------------
gf_sf <- st_as_sf(gfrance85)
class(gf_sf)
names(gf_sf)

## ----sf-basic-----------------------------------------------------------------
ggplot(gf_sf) +
  geom_sf(fill = "grey90", color = "white") +
  theme_void()

## ----guerry-instruction-map, echo=FALSE, out.width="60%", fig.align="center"----
knitr::include_graphics("../man/figures/Guerry1833-instruction.jpg")

## ----sf-choropleth------------------------------------------------------------
ggplot(gf_sf) +
  geom_sf(aes(fill = Literacy), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "PuBu", direction = -1, name = "Literacy") +
  theme_void()

## ----sf-facet, fig.width=8, fig.height=6--------------------------------------
main_vars <- c("Crime_pers", "Crime_prop", "Literacy", "Donations", "Infants", "Suicides")

ranks_sf <- gf_sf |>
  select(dept) |>
  left_join(Guerry_ranks |> select(dept, all_of(main_vars)), by = "dept") |>
  pivot_longer(cols = all_of(main_vars), names_to = "variable", values_to = "rank")

ggplot(ranks_sf) +
  geom_sf(aes(fill = rank), color = NA) +
  facet_wrap(~ variable) +
  scale_fill_distiller(palette = "PuBu", direction = -1, name = "Rank") +
  theme_void() +
  theme(strip.text = element_text(size = 11, face = "bold"))

## ----sf-region, fig.width=7, fig.height=7-------------------------------------
col.region <- colors()[c(149, 254, 468, 552, 26)]  # same colors used in the README

ggplot(gf_sf) +
  geom_sf(aes(fill = Region), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = Department, color = Region == "W"),
               size = 3.2, check_overlap = TRUE) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "white"), guide = "none") +
  scale_fill_manual(values = col.region) +
  theme_void()

