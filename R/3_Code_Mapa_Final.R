# install.packages(c('ggnewscale', 'ggspatial'))
devtools::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
library(sf)
library(ggnewscale)
library(ggspatial)
library(tidyverse)
library(readxl)

mat <- read_sf("gis/ma_limite_integrador.shp")
sul <- brazilmaps::get_brmap(geo = "Region", class = "sf") %>% # shape dos da regiao sul
  subset(desc_rg == "SUL") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

e_matrix <- read_xlsx("data/data_ferns.xlsx", sheet = "e_matrix") %>%
  arrange(site)

e_matrix$ref[e_matrix$ref == "This study"] <- "This study"
e_matrix$ref[!e_matrix$ref == "This study"] <- "Literature"

view(e_matrix)
dim(e_matrix)
p <- e_matrix


states <- brazilmaps::get_brmap(geo = "State", class = "sf") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

flo_typ <- st_read("gis/veg_sul.shp", quiet = TRUE) # shape tipos florestais


flo_typ$legenda[which(flo_typ$legenda == "Água")] <- "Water"
flo_typ$legenda[which(flo_typ$legenda == "Estepe")] <- "Other physiognomies"
flo_typ$legenda[which(
  flo_typ$legenda == "Floresta Estacional Decidual"
)] <- "Dry Forest"
flo_typ$legenda[which(
  flo_typ$legenda == "Floresta Estacional Semidecidual"
)] <- "Dry Forest"
flo_typ$legenda[which(
  flo_typ$legenda == "Floresta Ombrófila Mista"
)] <- "Araucaria Forest"
flo_typ$legenda[which(flo_typ$legenda == "Savana")] <- "Other physiognomies"
flo_typ$legenda[which(
  flo_typ$legenda == "Savana-Estépica"
)] <- "Other physiognomies"
flo_typ$legenda[which(
  flo_typ$legenda == "Formação Pioneira"
)] <- "Riverine/Coastal Woodlands"
flo_typ$legenda[which(flo_typ$legenda == "Contato")] <- "Mosaic"
flo_typ$legenda[which(
  flo_typ$legenda == "Floresta Ombrófila Densa"
)] <- "Dense Rainforest"

my_palette <- c("#B0F4FA", "#8B0069", "#75C165", "#A96C00")
col <- data.frame(Tipo = NA, cor = NA)
col[3, ] <- c("Dry forest", "#8B0069")
col[2, ] <- c("Dense Rainforest", "#c3f4f7")
col[1, ] <- c("Mixed Rainforest (Araucaria Forest)", "#75C165")
col[5, ] <- c("Other physiognomies", "white")
col[6, ] <- c("Restingas and riverine forest", "#A96C00")
col[4, ] <- c("Mosaic", "gray50")
col[7, ] <- c("Water", "lightblue")

mat_s <- st_transform(mat, crs = 4674) |> st_make_valid()
summary(st_is_valid(flo_typ))
summary(st_is_valid(mat_s))

mat_fl <- st_intersection(flo_typ, mat_s)
saveRDS(mat_fl, file = "gis/ma_physio.RDS")

wo <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

a <- ggplot() +
  geom_sf(
    data = st_as_sf(wo, crs = st_crs(4326)),
    aes(lty = "Country Limits"),
    color = "gray50",
    fill = "gray80", # remove cor de fundo
    size = 0.3
  ) +
  geom_sf(
    data = states,
    lwd = .5,
    fill = "white"
  ) +
  geom_sf(data = mat_fl, aes(fill = legenda), alpha = .6, lwd = .06) +
  scale_fill_manual(
    values = col$cor
  ) +
  labs(fill = "Physiognomies") +
  geom_sf(
    data = states,
    aes(alpha = "Brazilian States Limits"),
    col = "black",
    lwd = .5,
    fill = NA
  )

a + ggnewscale::new_scale_fill() +
  geom_point(
    data = p,
    aes(x = Long, y = Lat, fill = ref, shape = ref),
    size = 2.5,
    col = "black",
    alpha = 0.95,
    stroke = .5
  ) +
  scale_color_manual(values = "black") +
  scale_size_continuous(range = c(2, 4)) +
  scale_fill_manual(
    name = "Data from",
    values = c("gray80", "gray40")
  ) +
  scale_shape_manual(values = c(21, 22), name = "Data from") +
  coord_sf(
    xlim = c(-59, -44), # define min e max da longitude
    ylim = c(-35, -22), # define min e max da latitude
    crs = 4326,
    expand = F
  ) +
  labs(
    alpha = " ",
    size = "Richness",
    colour = " ",
    linetype = " ",
    caption = "Datum WGS84
       Map data adapted from Instituto Brasileiro de Geografia e Estatística (IBGE)
    Integrative boundaries of the Atlantic Forest (Muylaert et al. 2018)"
  ) +
  annotation_scale(
    location = "br", # insere a escala da legenda
    width_hint = 0.2,
    unit_category = "metric",
    line_width = 2,
    text_cex = 0.6, # tamanho do texto
    text_col = "black"
  ) + # cor do texto
  annotation_north_arrow(
    location = "br", # insere a flecha para o norte
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 0),
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_y = unit(.8, "cm")
  ) + # estilo da flecha (veja o help para outros estilos)
  theme_bw(base_size = 9) +
  geom_text(
    aes(x = -48, y = -30, label = "Atlantic Ocean"),
    angle = 45,
    size = 3
  ) +
  geom_text(aes(x = -56.5, y = -32, label = "Uruguay"), angle = 0, size = 3) +
  geom_text(
    aes(x = -57.7, y = -29, label = "Argentina"),
    angle = 50,
    size = 3
  ) +
  geom_text(aes(x = -57, y = -24.5, label = "Paraguay"), angle = 0, size = 3) +
  geom_text(x = -51.5, y = -25, aes(label = "Brazil"), size = 3) +
  theme(
    axis.title.x = element_blank(), # apaga o titulo do eixo x
    axis.title.y = element_blank(),
    legend.margin = margin(r = 5, l = 5, t = .5, b = 5), # altero as margens da legenda
    legend.title = element_text(size = 9),
    legend.key = element_rect(fill = "transparent"),
    legend.text.align = 0,
    panel.background = element_rect(fill = "lightblue")
  )

ggsave(
  "figs/map1.tiff",
  dpi = 300,
  width = 170,
  height = 170,
  bg = "white",
  units = "mm"
)
ggsave(
  "figs/map1.jpg",
  dpi = 300,
  width = 170,
  height = 170,
  bg = "white",
  units = "mm"
)
