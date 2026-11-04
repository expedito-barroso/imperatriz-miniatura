# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): diagnóstico do OSM para o piso vetorial
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
#   Medir, por classe de via, a extensão e o preenchimento das etiquetas que o
#   piso vetorial usaria (faixas, sentido, calçadas), rotatórias, ferrovia e
#   faixas de pedestre. Base das convenções da seção 2.5.1 do script 03.
#   Resultados da rodada de 02/11/2026 (AOI anterior, de 2015) registrados no
#   tutorial; rodar de novo após trocar a AOI atualiza os números.

# 1 - Pacotes e AOI
# ------------------------------------------------------------------------------

# Carrega os pacotes já usados no script 03 e a AOI em UTM e em WGS 84
pacman::p_load(sf, dplyr, osmextract, here)
sf::sf_use_s2(FALSE)

aoi     <- sf::read_sf(here::here("data", "processed", "aoi_imperatriz.gpkg"), layer = "aoi") |>
  sf::st_transform(31983)
aoi_wgs <- sf::st_transform(aoi, 4326)

# Único extrato .osm.pbf em data/raw/osm/
arq_pbf <- list.files(here::here("data", "raw", "osm"), pattern = "\\.osm\\.pbf$", full.names = TRUE)
stopifnot(length(arq_pbf) == 1)

# 2 - Leitura das linhas com as etiquetas que o piso usaria
# ------------------------------------------------------------------------------

# extra_tags acrescenta colunas que a configuração padrão do osmextract não
# traz (faixas, sentido, rotatória, ferrovia, calçada); force_vectortranslate
# refaz a conversão do .pbf com essas colunas (cerca de 2 minutos)
linhas <- osmextract::oe_read(
  arq_pbf,
  layer                 = "lines",
  extra_tags            = c("lanes", "oneway", "junction", "railway", "sidewalk", "footway"),
  boundary              = aoi_wgs,
  boundary_type         = "clipsrc",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
) |>
  sf::st_transform(31983) |>
  dplyr::mutate(km = as.numeric(sf::st_length(geometry)) / 1000)

# 3 - Vias por classe: extensão e preenchimento de faixas e sentido
# ------------------------------------------------------------------------------

# Uma linha por classe de via, da mais extensa para a menos extensa:
# km total, % do km com número de faixas (lanes) e % do km com sentido (oneway)
vias_classe <- linhas |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(highway)) |>
  dplyr::group_by(highway) |>
  # o total recebe nome próprio (km_total): no summarise, um nome repetido
  # passaria a valer o total nas linhas seguintes e os percentuais sairiam errados
  dplyr::summarise(
    pct_lanes    = round(100 * sum(km[!is.na(lanes)])    / sum(km), 1),
    pct_oneway   = round(100 * sum(km[!is.na(oneway)])   / sum(km), 1),
    pct_sidewalk = round(100 * sum(km[!is.na(sidewalk)]) / sum(km), 1),
    km_total     = round(sum(km), 1),
    .groups = "drop"
  ) |>
  dplyr::relocate(km_total, .after = highway) |>
  dplyr::arrange(dplyr::desc(km_total))

print(vias_classe, n = Inf)

# 4 - Rotatórias, ferrovia e calçadas mapeadas como linha
# ------------------------------------------------------------------------------

# Rotatórias: trechos com junction = roundabout
cat("\nRotatórias (km):",
    round(sum(linhas$km[linhas$junction %in% "roundabout"]), 2), "\n")

# Ferrovia: km por tipo (rail, abandoned, etc.)
cat("\nFerrovia (km por tipo):\n")
print(linhas |> sf::st_drop_geometry() |> dplyr::filter(!is.na(railway)) |>
        dplyr::count(railway, wt = round(km, 2), name = "km"))

# Calçadas desenhadas como via própria (footway = sidewalk)
cat("\nCalçadas mapeadas como linha (km):",
    round(sum(linhas$km[linhas$footway %in% "sidewalk"]), 2), "\n")

# 5 - Faixas de pedestre (pontos)
# ------------------------------------------------------------------------------

# A camada de pontos traz highway = crossing; contagem dentro da AOI
pontos <- osmextract::oe_read(
  arq_pbf,
  layer                 = "points",
  boundary              = aoi_wgs,
  boundary_type         = "clipsrc",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
)
cat("\nFaixas de pedestre (pontos highway = crossing):",
    sum(pontos$highway %in% "crossing"), "\n")
