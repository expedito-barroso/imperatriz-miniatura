# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): diagnóstico do pavimento das vias no OSM
# Recorte: AOI do núcleo urbano de Imperatriz (MA), vias de carro do OSM
# Fonte  : OpenStreetMap, extrato Geofabrik (.osm.pbf), via osmextract
#
# Autor : Antonio Expedito Ferreira Barroso de Carvalho
# Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
# Data  : novembro/2026
# R     : 4.6.1
# Email : expebarroso@uemasul.edu.br
# ==============================================================================
#
# Objetivo deste script
#   Medir a extensão das vias de carro por tipo de pavimento (etiqueta surface)
#   e por classe de via. Base da convenção de pavimento (opção A) do script 03.
#   Resultados da rodada de 02/11/2026 (AOI anterior, de 2015) registrados no
#   tutorial; rodar de novo após trocar a AOI atualiza os números.

# 1 - Pacotes, AOI e extrato
# ------------------------------------------------------------------------------

pacman::p_load(sf, dplyr, tidyr, osmextract, here)
sf::sf_use_s2(FALSE)

aoi_wgs <- sf::read_sf(here::here("data", "processed", "aoi_imperatriz.gpkg"), layer = "aoi") |>
  sf::st_transform(4326)
arq_pbf <- list.files(here::here("data", "raw", "osm"), pattern = "\\.osm\\.pbf$", full.names = TRUE)
stopifnot(length(arq_pbf) == 1)

# 2 - Leitura das linhas com a etiqueta de pavimento (surface)
# ------------------------------------------------------------------------------

# Nova conversão do .pbf, agora com a coluna surface (cerca de 2 minutos)
linhas <- osmextract::oe_read(
  arq_pbf,
  layer                 = "lines",
  extra_tags            = c("surface", "oneway"),
  boundary              = aoi_wgs,
  boundary_type         = "clipsrc",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
) |>
  sf::st_transform(31983) |>
  dplyr::mutate(km = as.numeric(sf::st_length(geometry)) / 1000)

# 3 - Só as vias de carro (as classes que entram no piso)
# ------------------------------------------------------------------------------

classes_carro <- c("trunk", "trunk_link", "primary", "primary_link",
                   "secondary", "secondary_link", "tertiary", "tertiary_link",
                   "unclassified", "residential", "living_street", "service")

vias <- linhas |>
  sf::st_drop_geometry() |>
  dplyr::filter(highway %in% classes_carro)

# 4 - Agrupamento dos valores de surface em categorias
# ------------------------------------------------------------------------------

# Correspondência com os termos usados no Brasil:
#   asphalt                      = asfalto
#   paving_stones                = bloquete de concreto intertravado ("blocrete")
#   sett, cobblestone,
#   unhewn_cobblestone           = paralelepípedo e pedra tosca
#   concrete, concrete:plates    = concreto
#   unpaved, dirt, earth, ground,
#   gravel, fine_gravel, compacted,
#   sand, grass, mud, pebblestone = não pavimentada
#   paved (sem especificar)      = pavimentada sem tipo
vias <- vias |>
  dplyr::mutate(categoria = dplyr::case_when(
    is.na(surface)                                            ~ "sem informação",
    surface == "asphalt"                                      ~ "asfalto",
    surface == "paving_stones"                                ~ "bloquete",
    surface %in% c("sett", "cobblestone", "unhewn_cobblestone") ~ "paralelepípedo",
    surface %in% c("concrete", "concrete:plates", "concrete:lanes") ~ "concreto",
    surface %in% c("unpaved", "dirt", "earth", "ground", "gravel", "fine_gravel",
                   "compacted", "sand", "grass", "mud", "pebblestone") ~ "não pavimentada",
    surface == "paved"                                        ~ "pavimentada sem tipo",
    TRUE                                                      ~ "outros"
  ))

# 5 - Resultados
# ------------------------------------------------------------------------------

# 5.1 - Total por categoria, com percentual sobre o km total de vias de carro
cat("\nVias de carro na AOI:", round(sum(vias$km), 1), "km\n\n")
vias |>
  dplyr::group_by(categoria) |>
  dplyr::summarise(km_total = round(sum(km), 1), .groups = "drop") |>
  dplyr::mutate(pct = round(100 * km_total / sum(km_total), 1)) |>
  dplyr::arrange(dplyr::desc(km_total)) |>
  print(n = Inf)

# 5.2 - Valores originais de surface, para conferir o agrupamento
cat("\nValores de surface encontrados (km):\n")
vias |>
  dplyr::filter(!is.na(surface)) |>
  dplyr::count(surface, wt = round(km, 1), name = "km_total", sort = TRUE) |>
  # count() sobre tabela comum devolve data.frame; sem a conversão para
  # tibble, o argumento n do print() é lido como na.print e gera erro
  dplyr::as_tibble() |>
  print(n = Inf)

# 5.3 - Categoria por classe de via (km)
cat("\nCategoria por classe de via (km):\n")
vias |>
  dplyr::group_by(highway, categoria) |>
  dplyr::summarise(km_total = round(sum(km), 1), .groups = "drop") |>
  tidyr::pivot_wider(names_from = categoria, values_from = km_total, values_fill = 0) |>
  dplyr::as_tibble() |>
  print(n = Inf, width = Inf)
