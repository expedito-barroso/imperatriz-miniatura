# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): 01, definição da AOI
# Recorte: aglomerados de edificações de 2023 no município de Imperatriz (MA),
#          IBGE 2105302, a oeste da ferrovia
# Fonte  : Google Open Buildings V3 Polygons (centroides, script 00, via GEE);
#          IBGE, Malha Municipal 2020, via geobr (Ipea); OpenStreetMap,
#          extrato Geofabrik (.osm.pbf), via osmextract
#
# Autor : Antonio Expedito Ferreira Barroso de Carvalho
# Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
# Data  : novembro/2026
# R     : 4.6.1
# Email : expebarroso@uemasul.edu.br
# ==============================================================================
#
# Objetivo deste script
#   Produzir a AOI (Area of Interest, área de interesse) que delimita a cena.
#   A AOI é traçada pela densidade das edificações do Open Buildings V3
#   (2023): as edificações são contadas numa grade, as células densas são
#   agrupadas em aglomerados, os aglomerados pequenos são descartados e o
#   resultado é recortado pelo limite municipal e pela ferrovia (nada a leste
#   dos trilhos entra na cena).
#
#   Versão anterior (até 02/11/2026): maior mancha urbana do IBGE de 2015 com
#   buffer de 500 m. Ela deixava de fora loteamentos e condomínios recentes
#   (ex.: Ecopark, Verona, Império Romano) e cortava núcleos na borda.
#
# Entradas
#   data/raw/ob_v3_centroides_municipio.csv   script 00 (GEE), baixado do Drive
#   data/raw/osm/<extrato>.osm.pbf            extrato Geofabrik (o mesmo do script 03)
#
# Saídas
#   data/processed/aoi_imperatriz.gpkg        AOI em SIRGAS 2000 / UTM 23S
#                                             (EPSG:31983), usada pelo script 03
#   data/processed/aoi_imperatriz_gee.zip     shapefile da AOI em WGS 84
#                                             (EPSG:4326), para o asset do GEE
#   data/processed/aoi_inspecao.gpkg          camadas para conferência no QGIS:
#                                             aoi, aglomerados, ferrovia,
#                                             municipio, aoi_anterior
#   data/processed/aoi_imperatriz_2015.gpkg   cópia da AOI anterior (feita uma
#                                             única vez, antes de sobrescrever)
#
# Execução
#   Abrir o projeto .Rproj e rodar o script inteiro (Ctrl+Shift+Enter).


# ------------------------------------------------------------------------------
# 1. - Pacotes
# ------------------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  sf,          # vetores: leitura, operações geométricas, escrita
  terra,       # grade de contagem e agrupamento das células densas
  geobr,       # malha municipal do IBGE
  osmextract,  # leitura da ferrovia no extrato OSM local
  dplyr,       # manipulação de tabelas
  here,        # caminhos relativos à raiz do projeto
  zip          # compactação do shapefile para o GEE
)

# Geometria plana: todas as operações métricas são feitas em UTM
sf::sf_use_s2(FALSE)


# ------------------------------------------------------------------------------
# 2. - Parâmetros
# ------------------------------------------------------------------------------

# 2.1 - Identificação do município e projeções
# ------------------------------------------------------------------------------

COD_MUNI <- 2105302   # código IBGE de Imperatriz (MA)
CRS_UTM  <- 31983     # SIRGAS 2000 / UTM zona 23S
CRS_GEE  <- 4326      # WGS 84, para o upload no GEE

# 2.2 - Filtros das edificações (os mesmos do script 03)
# ------------------------------------------------------------------------------

CONF_MIN <- 0.70      # confiança mínima do V3
AREA_MIN <- 16        # m²

# 2.3 - Densidade e aglomerados (heurísticas, calibradas pela seção 6)
# ------------------------------------------------------------------------------

# Grade de contagem: células de CELULA_M; a densidade de cada célula é a soma
# das edificações numa janela de JANELA x JANELA células (150 m x 150 m =
# 2,25 ha). Célula densa: pelo menos DENS_MIN edificações na janela.
CELULA_M <- 50
JANELA   <- 3
DENS_MIN <- 5          # cerca de 2 edificações por hectare

# Aglomerados (células densas vizinhas, inclusive na diagonal) com menos de
# N_MIN_AGLOMERADO edificações são descartados (sítios, povoados pequenos)
N_MIN_AGLOMERADO <- 150

# Distância máxima ao núcleo contínuo (o aglomerado com mais edificações).
# Decisão de 02/11/2026, a partir da tabela de distâncias da seção 6: entram
# o núcleo, os aglomerados encostados nele (até 1 km) e os dois seguintes
# (2,35 e 3,3 km), que devem ser os núcleos ao norte e a nor-nordeste
# indicados na inspeção (confirmar no QGIS); o próximo está a 4,85 km e os
# demais, povoados rurais, a mais de 8 km.
DIST_MAX_M <- 3500

# Ressalva registrada na inspeção de 02/11/2026: a borda passa no meio do
# Conjunto Canto da Serra (ao sul do Sebastião Régis); as fileiras do sudoeste,
# provavelmente concluídas depois das imagens do Open Buildings V3, não têm
# edificações nos dados e ficam fora. A ressalva vai para metadados_cena.json
# (script 03, seção 9.3) e aparece no painel "Fontes" da página.

# Fechamento: amplia e depois reduz os aglomerados por FECHAMENTO_M, o que
# une núcleos separados por menos de 2 x FECHAMENTO_M (um córrego, uma
# avenida larga) e suaviza o contorno em degraus da grade
FECHAMENTO_M <- 150

# Margem final em torno dos aglomerados, para não cortar as edificações da borda
BUFFER_M <- 100

# 2.4 - Ferrovia
# ------------------------------------------------------------------------------

# Valores da etiqueta railway do OSM tratados como trilho ativo
CLASSES_TRILHO <- c("rail")

# Só a Ferrovia Norte-Sul limita a cidade a leste. O Ramal Suzano (e um trecho
# sem nome na ponta oeste dele) corta o município de leste a oeste ao norte da
# cidade e não serve de limite.
TRILHOS_NOMES <- c("Ferrovia Norte-Sul")

# 2.5 - Pastas e arquivos
# ------------------------------------------------------------------------------

dir_raw  <- here::here("data", "raw")
dir_proc <- here::here("data", "processed")
DIR_OSM  <- file.path(dir_raw, "osm")
dir.create(dir_proc, recursive = TRUE, showWarnings = FALSE)

arq_centroides <- file.path(dir_raw, "ob_v3_centroides_municipio.csv")
arq_gpkg       <- file.path(dir_proc, "aoi_imperatriz.gpkg")
arq_inspecao   <- file.path(dir_proc, "aoi_inspecao.gpkg")


# ------------------------------------------------------------------------------
# 3. - Leitura das entradas
# ------------------------------------------------------------------------------

# 3.1 - Limite municipal (IBGE 2020)
# ------------------------------------------------------------------------------

# simplified = FALSE mantém a resolução original do contorno; é o recorte que
# impede a AOI de avançar sobre Davinópolis, João Lisboa e outros vizinhos
muni <- geobr::read_municipality(
  code_muni    = COD_MUNI,
  year         = 2020,
  simplified   = FALSE,
  showProgress = FALSE
) |>
  sf::st_make_valid() |>
  sf::st_transform(CRS_UTM)

stopifnot(nrow(muni) == 1)
geom_muni <- sf::st_geometry(muni)

# 3.2 - Centroides das edificações (script 00)
# ------------------------------------------------------------------------------

if (!file.exists(arq_centroides)) {
  stop("Arquivo não encontrado: ", arq_centroides,
       ". Rode o script 00 no GEE e baixe o CSV do Drive para data/raw/.")
}

edif <- read.csv(arq_centroides) |>
  dplyr::filter(conf >= CONF_MIN, area >= AREA_MIN) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = CRS_GEE) |>
  sf::st_transform(CRS_UTM)

# Só as edificações dentro do limite municipal do IBGE
edif <- edif[sf::st_intersects(edif, geom_muni, sparse = FALSE)[, 1], ]
message("Edificações no município (conf >= ", CONF_MIN, ", área >= ",
        AREA_MIN, " m²): ", nrow(edif))

# 3.3 - Ferrovia (OpenStreetMap)
# ------------------------------------------------------------------------------

arq_pbf <- list.files(DIR_OSM, pattern = "\\.osm\\.pbf$", full.names = TRUE)
if (length(arq_pbf) != 1) {
  stop("Esperado exatamente 1 arquivo .osm.pbf em ", DIR_OSM,
       "; encontrados: ", length(arq_pbf))
}

# Recorte pelo município (clipsrc): os trilhos terminam no limite municipal,
# de modo que cortam o polígono do município de borda a borda
trilhos <- osmextract::oe_read(
  arq_pbf,
  layer                 = "lines",
  extra_tags            = "railway",
  boundary              = sf::st_transform(muni, CRS_GEE),
  boundary_type         = "clipsrc",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
) |>
  sf::st_transform(CRS_UTM) |>
  dplyr::filter(railway %in% CLASSES_TRILHO, name %in% TRILHOS_NOMES)

message("Trilhos no município: ", nrow(trilhos), " trechos, ",
        round(sum(as.numeric(sf::st_length(trilhos))) / 1000, 1), " km")
if (nrow(trilhos) == 0) stop("Nenhum trilho encontrado no município.")


# ------------------------------------------------------------------------------
# 4. - Corte pela ferrovia
# ------------------------------------------------------------------------------

# 4.1 - Partes do município separadas pelos trilhos
# ------------------------------------------------------------------------------

# Os trilhos, engrossados em 1 m, são subtraídos do município; o que sobra
# se parte em pedaços, um de cada lado da linha
geom_partes <- sf::st_difference(geom_muni, sf::st_union(sf::st_buffer(trilhos, 1))) |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_cast("POLYGON", warn = FALSE)

partes <- sf::st_sf(geometry = geom_partes) |>
  dplyr::mutate(
    parte    = dplyr::row_number(),
    area_km2 = round(as.numeric(sf::st_area(geometry)) / 1e6, 2),
    n_edif   = lengths(sf::st_intersects(geometry, edif))
  )

# 4.2 - Lado da cidade
# ------------------------------------------------------------------------------

# O lado da cidade é a parte com mais edificações; as demais ficam fora.
# A tabela mostra quantas edificações cada parte descartada leva consigo:
# um número alto numa parte a oeste indicaria um ramal ferroviário cortando
# a cidade, a ser conferido no QGIS.
# (as_tibble: sem ela, o argumento n do print() seria lido como na.print)
partes |>
  sf::st_drop_geometry() |>
  dplyr::arrange(dplyr::desc(n_edif)) |>
  dplyr::as_tibble() |>
  print(n = 20)

lado_cidade <- partes |> dplyr::slice_max(n_edif, n = 1, with_ties = FALSE)
edif <- edif[sf::st_intersects(edif, lado_cidade, sparse = FALSE)[, 1], ]
message("Edificações no lado da cidade: ", nrow(edif))


# ------------------------------------------------------------------------------
# 5. - Aglomerados por densidade
# ------------------------------------------------------------------------------

# 5.1 - Grade de contagem
# ------------------------------------------------------------------------------

# Grade alinhada ao retângulo envolvente do lado da cidade
grade <- terra::rast(
  terra::ext(terra::vect(lado_cidade)),
  resolution = CELULA_M,
  crs        = paste0("EPSG:", CRS_UTM)
)

# Número de edificações por célula (células sem edificação = 0)
contagem <- terra::rasterize(terra::vect(edif), grade, fun = "length", background = 0)

# 5.2 - Densidade na janela e células densas
# ------------------------------------------------------------------------------

densidade <- terra::focal(contagem, w = JANELA, fun = "sum", na.rm = TRUE)
densas    <- terra::ifel(densidade >= DENS_MIN, 1, NA)

# 5.3 - Aglomerados e contagem de edificações em cada um
# ------------------------------------------------------------------------------

# patches() numera os grupos de células densas vizinhas (8 direções)
aglom_r <- terra::patches(densas, directions = 8)

n_por_aglom <- terra::zonal(contagem, aglom_r, fun = "sum", na.rm = TRUE)
names(n_por_aglom) <- c("aglomerado", "n_edif")

# 5.4 - Polígonos dos aglomerados
# ------------------------------------------------------------------------------

aglomerados <- terra::as.polygons(aglom_r) |>
  sf::st_as_sf()
names(aglomerados)[1] <- "aglomerado"
aglomerados <- aglomerados |>
  dplyr::left_join(n_por_aglom, by = "aglomerado") |>
  dplyr::mutate(area_ha = round(as.numeric(sf::st_area(geometry)) / 1e4, 1)) |>
  dplyr::arrange(dplyr::desc(n_edif))

# 5.5 - Distância ao núcleo contínuo e seleção
# ------------------------------------------------------------------------------

# O núcleo contínuo é o aglomerado com mais edificações (o primeiro após a
# ordenação). Mantido = tamanho mínimo e distância máxima ao núcleo.
nucleo <- aglomerados[1, ]
aglomerados <- aglomerados |>
  dplyr::mutate(
    dist_km = round(as.numeric(sf::st_distance(geometry, nucleo)) / 1000, 2),
    mantido = n_edif >= N_MIN_AGLOMERADO & dist_km * 1000 <= DIST_MAX_M
  )


# ------------------------------------------------------------------------------
# 6. - Diagnóstico para calibração
# ------------------------------------------------------------------------------

# Tabela dos aglomerados por distância ao núcleo. A
# decisão sobre DENS_MIN, N_MIN_AGLOMERADO e DIST_MAX_M parte desta tabela e da
# conferência no QGIS (camada "aglomerados" do aoi_inspecao.gpkg).
message("Aglomerados: ", nrow(aglomerados), "; mantidos (>= ", N_MIN_AGLOMERADO,
        " edificações e até ", DIST_MAX_M / 1000, " km do núcleo): ", sum(aglomerados$mantido))
message("Edificações nos aglomerados mantidos: ",
        sum(aglomerados$n_edif[aglomerados$mantido]), " de ", nrow(edif),
        " (", round(100 * sum(aglomerados$n_edif[aglomerados$mantido]) / nrow(edif), 1), "%)")

# Aglomerados com o tamanho mínimo, do mais próximo ao mais distante do núcleo
cat("\nAglomerados com pelo menos", N_MIN_AGLOMERADO, "edificações, por distância ao núcleo:\n")
aglomerados |>
  sf::st_drop_geometry() |>
  dplyr::filter(n_edif >= N_MIN_AGLOMERADO) |>
  dplyr::arrange(dist_km) |>
  dplyr::as_tibble() |>
  print(n = 40)


# ------------------------------------------------------------------------------
# 7. - Construção da AOI
# ------------------------------------------------------------------------------

# 7.1 - Fechamento, margem e preenchimento de buracos
# ------------------------------------------------------------------------------

# Remove os buracos de cada polígono (praças, lagoas e vazios cercados pela
# cidade passam a fazer parte da AOI)
sem_buracos <- function(g) {
  g |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    lapply(function(p) sf::st_polygon(p[1])) |>
    sf::st_sfc(crs = sf::st_crs(g))
}

geom_aoi <- aglomerados |>
  dplyr::filter(mantido) |>
  sf::st_union() |>
  sf::st_buffer(FECHAMENTO_M) |>
  sf::st_buffer(-FECHAMENTO_M + BUFFER_M) |>
  sem_buracos() |>
  sf::st_union()

# 7.2 - Recorte pelo município e pelo lado da cidade
# ------------------------------------------------------------------------------

# A margem pode ultrapassar o limite municipal e os trilhos; o recorte final
# devolve a AOI ao lado oeste da ferrovia, dentro de Imperatriz
geom_aoi <- geom_aoi |>
  sf::st_intersection(sf::st_geometry(lado_cidade)) |>
  sf::st_collection_extract("POLYGON") |>
  suppressWarnings() |>
  sf::st_union()

aoi <- sf::st_sf(
  nome     = "Imperatriz_aglomerados_2023",
  cod_muni = COD_MUNI,
  buffer_m = BUFFER_M,
  geometry = geom_aoi
) |>
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6)

n_dentro <- sum(sf::st_intersects(edif, aoi, sparse = FALSE)[, 1])
message("Área da AOI: ", round(aoi$area_km2, 1), " km²; ",
        sf::st_cast(sf::st_geometry(aoi), "POLYGON") |> length(), " polígono(s); ",
        n_dentro, " edificações dentro")

# 7.3 - Comparação com a AOI anterior
# ------------------------------------------------------------------------------

# Na primeira execução desta versão, a AOI de 2015 é copiada antes de ser
# sobrescrita; nas seguintes, a cópia já existe e é só lida
arq_2015 <- file.path(dir_proc, "aoi_imperatriz_2015.gpkg")
if (file.exists(arq_gpkg) && !file.exists(arq_2015)) file.copy(arq_gpkg, arq_2015)

aoi_anterior <- NULL
if (file.exists(arq_2015)) {
  aoi_anterior <- sf::read_sf(arq_2015, layer = "aoi") |> sf::st_transform(CRS_UTM)
  n_antes <- sum(sf::st_intersects(edif, aoi_anterior, sparse = FALSE)[, 1])
  message("AOI anterior: ", round(sum(as.numeric(sf::st_area(aoi_anterior))) / 1e6, 1),
          " km², ", n_antes, " edificações; nova: +", n_dentro - n_antes, " edificações")
}


# ------------------------------------------------------------------------------
# 8. - Inspeção visual
# ------------------------------------------------------------------------------

# Município em cinza, aglomerados mantidos em laranja, descartados em
# cinza-escuro, trilhos em preto, AOI anterior tracejada e AOI nova em azul
plot(geom_muni, col = "grey95", border = "grey60", main = "AOI: aglomerados de 2023")
plot(sf::st_geometry(aglomerados[!aglomerados$mantido, ]), col = "grey55", border = NA, add = TRUE)
plot(sf::st_geometry(aglomerados[aglomerados$mantido, ]), col = "#E8A35A", border = NA, add = TRUE)
plot(sf::st_geometry(trilhos), col = "black", lwd = 1.5, add = TRUE)
if (!is.null(aoi_anterior)) plot(sf::st_geometry(aoi_anterior), border = "grey30", lty = 2, add = TRUE)
plot(sf::st_geometry(aoi), border = "#1F4E79", lwd = 2, add = TRUE)


# ------------------------------------------------------------------------------
# 9. - Exportação
# ------------------------------------------------------------------------------

# 9.1 - GeoPackage em UTM para o script 03
# ------------------------------------------------------------------------------

sf::st_write(aoi, arq_gpkg, layer = "aoi", delete_layer = TRUE, quiet = TRUE)

# 9.2 - Camadas de inspeção para o QGIS
# ------------------------------------------------------------------------------

# delete_layer = TRUE substitui cada camada sem apagar o arquivo (evita o
# erro de permissão quando o arquivo ficou preso a uma sessão anterior)
inspecao <- list(
  aoi          = aoi,
  aglomerados  = aglomerados,
  ferrovia     = trilhos |> dplyr::select(railway),
  municipio    = muni |> dplyr::select(name_muni),
  aoi_anterior = aoi_anterior
)
inspecao <- Filter(Negate(is.null), inspecao)
for (nm in names(inspecao)) {
  sf::st_write(inspecao[[nm]], arq_inspecao, layer = nm, delete_layer = TRUE, quiet = TRUE)
}

# 9.3 - Shapefile compactado em WGS 84 para o GEE
# ------------------------------------------------------------------------------

# Os nomes de campo são encurtados porque o shapefile limita a 10 caracteres
dir_shp <- file.path(dir_proc, "aoi_imperatriz_gee")
dir.create(dir_shp, showWarnings = FALSE)

aoi |>
  sf::st_transform(CRS_GEE) |>
  dplyr::select(nome, cod_muni, buffer_m) |>
  sf::st_write(file.path(dir_shp, "imperatriz_aoi.shp"),
               delete_layer = TRUE, quiet = TRUE)

arqs_shp <- list.files(dir_shp, pattern = "^imperatriz_aoi\\.", full.names = TRUE)
zip::zip(
  zipfile = file.path(dir_proc, "aoi_imperatriz_gee.zip"),
  files   = arqs_shp,
  mode    = "cherry-pick"
)

message("Arquivos gravados em: ", dir_proc)
message("Próximo passo: conferir aoi_inspecao.gpkg no QGIS antes de subir a AOI no GEE.")
