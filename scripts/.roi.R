# paquetes ----------------------------------------------------------------

library(magick)
library(ggspatial)
library(tidyterra)
library(terra)
library(tidyverse)

# https://leaflet-extras.github.io/leaflet-providers/preview/

# datos -------------------------------------------------------------------

# vector de la región de interés
roi <- vect("vector/deltaEbro_Temp.shp") |>
  project("EPSG:4326")

# esri <- maptiles::get_tiles(
#   x = roi,
#   provider = "Esri.WorldImagery",
#   zoom = 13,
#   crop = TRUE
# )

# writeRaster(esri, "raster/esri.tif", overwrite = TRUE)
esri <- rast("esri.tif")

# RGB ---------------------------------------------------------------------

# cuadro <- ext(esri)
# asp <- abs(cuadro$"ymax"-cuadro$"ymin")/abs(cuadro$"xmax"-cuadro$"xmin")

ancho <- 20
alto <- ancho * (1080 / 1267)

g_rgb <- ggplot() +
  geom_spatraster_rgb(
    data = esri,
    maxcell = dim(esri)[1] * dim(esri)[2]
    # maxcell = 5e5
  ) +
  annotation_north_arrow(
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(.5, "cm"),
    pad_y = unit(.5, "cm"),
    location = "tr",
    style = north_arrow_fancy_orienteering(
      line_col = "white",
      text_col = "white"
    )
  ) +
  annotation_scale(
    line_col = "white",
    text_col = "white",
    pad_x = unit(.5, "cm"),
    pad_y = unit(.5, "cm"),
    location = "br"
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(
    breaks = c(.65, .75, .85),
    scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  scale_y_continuous(
    breaks = c(40.65, 40.75),
    scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  theme_void(base_size = 8) +
  theme(
    text = element_text(family = "Fira Code"),
    axis.text = element_text(),
    axis.text.y = element_text(angle = 90),
    axis.ticks = element_line(),
    axis.ticks.length.x = unit(2, "pt"),
    axis.ticks.length.y = unit(2, "pt")
  )

ggsave(
  plot = g_rgb,
  filename = "figuras/roi_rgb.png",
  width = ancho,
  height = alto,
  units = "cm"
)

browseURL("figuras/roi_rgb.png")

# INSET -------------------------------------------------------------------

# región alrededor de España
r <- ext(c(-10, 4, 36, 44)) |>
  vect(crs = "EPSG:4326")

# países limítrofes
l <- rgeoboundaries::gb_adm0(
  country = c("ESP", "PRT", "FRA")
) |>
  vect() |>
  crop(r)

asp_l <- (ext(l)$"ymax" - ext(l)$"ymin") / (ext(l)$"xmax" - ext(l)$"xmin")

roi_l <- vect(ext(esri), crs = "EPSG:4326") |>
  centroids() |>
  buffer(width = 50000) |>
  ext() |>
  vect(crs = "EPSG:4326")

g_inset <- ggplot() +
  geom_spatvector(
    data = l,
    aes(fill = shapeGroup),
    color = "black",
    linewidth = .5,
    show.legend = FALSE
  ) +
  geom_spatvector(data = roi_l, fill = NA, color = "red", linewidth = 1) +
  scale_fill_manual(
    breaks = c("ESP", "FRA", "PRT"),
    values = c("white", "grey95", "grey95")
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey92", color = "black")
  )

ggsave(
  plot = g_inset,
  filename = "figuras/roi_inset.png",
  width = 20,
  height = 20 * 986 / 1276,
  units = "cm"
)

# browseURL("figuras/roi_inset.png")

# COMPOSITE ---------------------------------------------------------------

roi_rgb <- image_read("figuras/roi_rgb.png")
roi_inset <- image_read("figuras/roi_inset.png") |>
  image_resize("500x")

roi_rgb |>
  image_composite(
    composite_image = roi_inset,
    offset = "+75+20",
    gravity = "northwest"
  ) |>
  image_write(
    "figuras/roi.png"
  )

browseURL("figuras/roi.png")
