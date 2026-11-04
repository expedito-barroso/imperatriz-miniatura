# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): 03, integração das camadas da cena
# Recorte: AOI do núcleo urbano de Imperatriz (MA), produzida pelo script 01
# Fontes : Google Open Buildings V3 Polygons e 2.5D Temporal V1 (2023), via GEE;
#          Copernicus DEM GLO-30 (2024_1), via GEE; OpenStreetMap, extrato
#          Geofabrik (.osm.pbf), via osmextract
#
# Autor : Antonio Expedito Ferreira Barroso de Carvalho
# Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
# Data  : novembro/2026
# R     : 4.6.1
# Email : expebarroso@uemasul.edu.br
# ==============================================================================
#
# Objetivo deste script
#   Transformar os dados exportados pelo GEE (script 02) e as feições do
#   OpenStreetMap em camadas prontas para a renderização 3D (etapa 6):
#     edificações com altura discretizada em pavimentos, classe visual e cota
#     de base; vias com largura por hierarquia; água; áreas verdes; piso
#     vetorial (eixos das vias com pavimento e marcação, caminhos de pedestre,
#     quadras e faixas de pedestre).
#
# Entradas (em data/raw/, baixadas do Google Drive, pasta tiny_imperatriz)
#   ob_v3_poligonos_imperatriz.geojson
#   ob_temporal_2023_imperatriz*.tif   (um ou mais arquivos; o GEE fatia
#                                       exportações grandes)
#   dem_glo30_imperatriz*.tif
#   osm/<extrato>.osm.pbf               extrato regional do Geofabrik (seção 2.7)
# Entrada (em data/processed/, gerada pelo script 01)
#   aoi_imperatriz.gpkg
#
# Saídas
#   data/processed/cena/edificacoes.geojson   coordenadas locais em metros
#   data/processed/cena/vias.geojson
#   data/processed/cena/agua.geojson
#   data/processed/cena/verde.geojson
#   data/processed/cena/aoi.geojson
#   data/processed/cena/eixos.geojson         piso vetorial: eixos das vias de carro
#   data/processed/cena/pedestres.geojson     piso vetorial: caminhos e ciclovias
#   data/processed/cena/quadras.geojson       piso vetorial: AOI menos vias e calçadas
#   data/processed/cena/faixas.geojson        piso vetorial: faixas de pedestre (pontos)
#   data/processed/cena/metadados_cena.json   origem, parâmetros, contagens, créditos
#   data/processed/cena_imperatriz_utm.gpkg    as mesmas camadas em EPSG:31983,
#                                              para inspeção no QGIS
#   output/figuras/03_hist_alturas.png         diagnóstico das alturas
#   output/tabelas/03_resumo_classes.csv       contagens por classe e pavimento
#   output/tabelas/03_sessionInfo.txt          reprodutibilidade


# ------------------------------------------------------------------------------
# 1. - Pacotes
# ------------------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  sf,             # vetores
  terra,          # rasters
  exactextractr,  # estatística zonal ponderada pela fração de cobertura
  osmextract,     # leitura de extratos .osm.pbf do OpenStreetMap
  dplyr,          # manipulação de tabelas
  ggplot2,        # gráfico de diagnóstico
  jsonlite,       # arquivo de metadados
  here            # caminhos relativos à raiz do projeto
)

# Geometria plana em todo o script: as operações são feitas em UTM
sf::sf_use_s2(FALSE)


# ------------------------------------------------------------------------------
# 2. - Parâmetros
# ------------------------------------------------------------------------------

# 2.1 - Projeção e ano
# ------------------------------------------------------------------------------

CRS_UTM <- 31983   # SIRGAS 2000 / UTM 23S, igual ao script 01 e à exportação do GEE
ANO     <- 2023    # ano das alturas exportadas no script 02

# 2.2 - Filtros das edificações
# ------------------------------------------------------------------------------

# Confiança mínima do V3 Polygons. O conjunto vai de 0,65 a 1,0; 0,70 descarta
# a faixa mais incerta. Escolha heurística, revisável após inspeção no QGIS.
CONF_MIN <- 0.70

# Área mínima do contorno, em m². Polígonos menores que ~ 1 pixel de 4 m
# (16 m²) não têm altura estimável e tendem a ser ruído.
AREA_MIN <- 16

# Limiar de presença usado para mascarar a altura. A confiança de presença
# não é calibrada (vale só para ordenar pixels); 0,5 é ponto de corte usual.
PRESENCA_MIN <- 0.5

# Cobertura mínima, em pixels de 4 m, para aceitar a mediana de altura.
# Abaixo disso, a altura é imputada (ver ALT_PADRAO).
COBERTURA_MIN <- 0.5

# 2.3 - Discretização e classes visuais (heurísticas declaradas)
# ------------------------------------------------------------------------------

PE_DIREITO      <- 3     # metros por pavimento, para converter altura em pavimentos
ALT_PADRAO      <- 3     # altura imputada quando não há estimativa (1 pavimento)
GALPAO_AREA_MIN <- 800   # m²; edificação grande e baixa é lida como galpão
GALPAO_PAV_MAX  <- 2
VERTICAL_PAV_MIN <- 4    # a partir de 4 pavimentos, classe "vertical"
TELHADO_M        <- 1.5  # metros descontados da altura antes de dividir pelo
                         # pé-direito: a altura do Open Buildings vai até o topo
                         # da edificação e inclui o telhado

# 2.4 - Relevo
# ------------------------------------------------------------------------------

USAR_RELEVO   <- TRUE  # FALSE coloca todas as bases na cota zero
JANELA_RELEVO <- 5     # janela da média móvel, em pixels de 30 m (150 m)

# 2.5 - Larguras visuais das vias, em metros, por classe do OSM
# ------------------------------------------------------------------------------

# Valores de representação gráfica, não medições de campo
LARGURA_VIAS <- c(
  motorway = 16, trunk = 14, primary = 12, secondary = 10, tertiary = 8,
  unclassified = 6, residential = 6, living_street = 5, service = 4
)

# 2.5.1 - Piso vetorial: vias, calçadas e quadras
# ------------------------------------------------------------------------------

# Convenções definidas a partir do diagnóstico do OSM de 02/11/2026
# (diag_osm_piso.R e diag_osm_pavimento.R): o OSM de Imperatriz quase não
# informa número de faixas, calçadas nem tipo de pavimento, então largura,
# marcação e pavimento seguem a classe da via.

# Classes de via de carro e a classe-base de cada uma (os acessos "_link"
# herdam largura e marcação da via principal; "track" é estrada de terra)
CLASSES_CARRO <- c(
  trunk = "trunk", trunk_link = "trunk", primary = "primary",
  primary_link = "primary", secondary = "secondary",
  secondary_link = "secondary", tertiary = "tertiary",
  tertiary_link = "tertiary", unclassified = "unclassified",
  residential = "residential", living_street = "living_street",
  service = "service", track = "track"
)
LARGURA_EIXO <- c(LARGURA_VIAS, track = 4)   # metros, por classe-base

# Pistas de mão única das classes abaixo são uma das pistas de uma avenida
# dupla (60 a 88% de mão única no diagnóstico) e recebem esta fração da largura
CLASSES_PISTA_DUPLA <- c("trunk", "primary", "secondary")
FATOR_MAO_UNICA     <- 0.6

# Classes que recebem marcação no asfalto (as residenciais ficam sem)
CLASSES_MARCADAS <- c("trunk", "primary", "secondary", "tertiary")

# Calçada derivada, de cada lado de toda via de carro (não há calçada mapeada)
CALCADA_M <- 2

# Valores de surface do OSM: terra, bloquete e o conjunto de valores válidos
# (fora dele, a etiqueta é tratada como ausente; ex.: um nome de rua gravado
# por engano no campo surface)
SURFACE_TERRA <- c("unpaved", "dirt", "earth", "ground", "gravel", "fine_gravel",
                   "compacted", "sand", "grass", "mud", "pebblestone")
SURFACE_BLOQUETE <- "paving_stones"
SURFACE_VALIDOS <- c(SURFACE_TERRA, SURFACE_BLOQUETE, "asphalt", "paved",
                     "concrete", "concrete:plates", "concrete:lanes",
                     "sett", "cobblestone", "unhewn_cobblestone")

# Vias de pedestre e ciclovias, com largura visual em metros
LARGURA_PEDESTRE <- c(footway = 2, pedestrian = 4, path = 1.5, cycleway = 2)

# Quadras menores que esta área (m²) são descartadas (sobras de recorte)
AREA_MIN_QUADRA <- 20

# Tolerância de simplificação dos eixos, em metros (reduz vértices sem
# alterar o traçado na escala da maquete)
SIMPLIFICA_M <- 0.5

# 2.6 - Pastas
# ------------------------------------------------------------------------------

dir_raw  <- here::here("data", "raw")
dir_proc <- here::here("data", "processed")
dir_cena <- file.path(dir_proc, "cena")
dir_fig  <- here::here("output", "figuras")
dir_tab  <- here::here("output", "tabelas")

for (d in c(dir_cena, dir_fig, dir_tab)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# 2.7 - Arquivo do OpenStreetMap
# ------------------------------------------------------------------------------

# Pasta do extrato regional do OSM em formato .osm.pbf, baixado manualmente
# do Geofabrik (América do Sul > Brasil > Nordeste). A seção 8 lê o único
# arquivo .osm.pbf encontrado aqui; o osmextract grava ao lado dele o
# GeoPackage intermediário da conversão.
DIR_OSM <- file.path(dir_raw, "osm")
dir.create(DIR_OSM, recursive = TRUE, showWarnings = FALSE)

# 2.8 - Moldura da água
# ------------------------------------------------------------------------------

# Margem, em metros, do retângulo que recorta a camada de água (seção 8.3).
# A AOI termina na margem direita do Tocantins; a moldura amplia o recorte
# só para a água, para que o leito do rio entre inteiro na cena.
MARGEM_AGUA_M <- 4000

# 2.9 - Árvores e gramados (Dynamic World, script 02, seção 5.1)
# ------------------------------------------------------------------------------

# Classes da banda label do Dynamic World (moda de 2023). Árvores: piso verde
# com árvores plantadas pela página. Gramados: piso verde-claro, sem árvores;
# reúne grama, vegetação alagada, cultivo e arbustos (convenção visual).
CLASSES_DW_ARVORES <- 1
CLASSES_DW_GRAMA   <- c(2, 3, 4, 5)

# Manchas menores que estas áreas (m²) são descartadas: "árvore onde tiver
# volume" exclui quintais e copas isoladas (25 pixels de 10 m = 0,25 ha)
AREA_MIN_ARVOREDO_M2 <- 2500
AREA_MIN_GRAMA_M2    <- 2500

# Tolerância de simplificação dos contornos (os polígonos saem da grade de
# 10 m em degraus; 5 m reduz vértices sem mudar a forma na escala da maquete)
SIMPLIFICA_VERDE_M <- 5

# 2.10 - Aeroporto
# ------------------------------------------------------------------------------

# Largura padrão (m) de pista e taxiway quando o OSM não informa a etiqueta
# width; pátios (aeroway=apron) vêm como polígonos
LARGURA_AEROVIA <- c(runway = 45, taxiway = 18)


# ------------------------------------------------------------------------------
# 3. - Leitura das entradas
# ------------------------------------------------------------------------------

# 3.1 - AOI
# ------------------------------------------------------------------------------

aoi <- sf::read_sf(file.path(dir_proc, "aoi_imperatriz.gpkg"), layer = "aoi") |>
  sf::st_transform(CRS_UTM)

# 3.2 - Contornos das edificações
# ------------------------------------------------------------------------------

# O GeoJSON vem do GEE em WGS 84; a reprojeção leva tudo para UTM
fp_bruto <- sf::read_sf(file.path(dir_raw, "ob_v3_poligonos_imperatriz.geojson")) |>
  sf::st_transform(CRS_UTM)

message("Contornos lidos: ", nrow(fp_bruto))

# 3.3 - Rasters exportados pelo GEE
# ------------------------------------------------------------------------------

# Função auxiliar: o GEE divide exportações grandes em vários arquivos com
# sufixos numéricos. Quando houver mais de um, monta um mosaico virtual (VRT).
ler_raster_gee <- function(prefixo) {
  arqs <- list.files(dir_raw, pattern = paste0("^", prefixo, ".*\\.tif$"),
                     full.names = TRUE)
  if (length(arqs) == 0) stop("Nenhum arquivo encontrado para: ", prefixo)
  if (length(arqs) == 1) terra::rast(arqs) else terra::vrt(arqs)
}

r_alt <- ler_raster_gee(paste0("ob_temporal_", ANO, "_imperatriz"))
r_dem <- ler_raster_gee("dem_glo30_imperatriz")

# Cobertura do solo do Dynamic World (opcional): sem o arquivo, a cena usa só
# o verde do OSM, como na versão anterior
tem_dw <- length(list.files(dir_raw, pattern = paste0("^dw_", ANO, "_imperatriz.*\\.tif$"))) > 0
r_dw   <- if (tem_dw) ler_raster_gee(paste0("dw_", ANO, "_imperatriz")) else NULL
if (!tem_dw) message("Dynamic World ausente em data/raw/: árvores e gramados só do OSM.")

# Os nomes das bandas nem sempre sobrevivem ao GeoTIFF; são atribuídos pela
# ordem em que foram selecionadas no script 02 (seção 4.2)
names(r_alt) <- c("building_height", "building_presence")
names(r_dem) <- "dem"

# Conferência das projeções: o script 02 exporta em EPSG:31983. Se a
# descrição do CRS gravada no GeoTIFF não for reconhecida como idêntica,
# o raster é reprojetado (vizinho mais próximo, para não suavizar alturas)
# e um aviso é emitido para registro.
crs_alvo <- paste0("EPSG:", CRS_UTM)

if (!terra::same.crs(r_alt, crs_alvo)) {
  warning("Raster de altura fora de ", crs_alvo, "; reprojetando.")
  r_alt <- terra::project(r_alt, crs_alvo, method = "near")
}
if (!terra::same.crs(r_dem, crs_alvo)) {
  warning("Raster de relevo fora de ", crs_alvo, "; reprojetando.")
  r_dem <- terra::project(r_dem, crs_alvo, method = "bilinear")
}


# ------------------------------------------------------------------------------
# 4. - Limpeza dos contornos
# ------------------------------------------------------------------------------

# 4.1 - Filtro de confiança, validade e tipo geométrico
# ------------------------------------------------------------------------------

fp <- fp_bruto |>
  dplyr::filter(confidence >= CONF_MIN) |>
  sf::st_make_valid() |>
  # st_make_valid pode gerar coleções; extrai apenas as partes poligonais
  sf::st_collection_extract("POLYGON") |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_cast("POLYGON", warn = FALSE)

# 4.2 - Área mínima e pertença à AOI
# ------------------------------------------------------------------------------

# A área é recalculada em UTM (a do GEE foi calculada na esfera).
# A pertença à AOI usa um ponto interno ao polígono (point on surface),
# que, ao contrário do centroide, cai sempre dentro da edificação.
fp <- fp |>
  dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::filter(area_m2 >= AREA_MIN)

dentro <- sf::st_intersects(sf::st_point_on_surface(fp), aoi, sparse = FALSE)[, 1]
fp <- fp[dentro, ]

# 4.3 - Identificador estável
# ------------------------------------------------------------------------------

fp <- fp |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::select(id, area_m2, confidence, full_plus_code)

message("Contornos após limpeza: ", nrow(fp),
        " (", round(100 * nrow(fp) / nrow(fp_bruto), 1), "% do total lido)")


# ------------------------------------------------------------------------------
# 5. - Altura por edificação
# ------------------------------------------------------------------------------

# 5.1 - Máscara de presença
# ------------------------------------------------------------------------------

# Pixels com presença abaixo do limiar recebem NA; assim a mediana de cada
# contorno usa apenas pixels que o modelo considera edificação
h_mascarada <- terra::ifel(r_alt[["building_presence"]] >= PRESENCA_MIN,
                           r_alt[["building_height"]], NA)

# 5.2 - Mediana ponderada própria
# ------------------------------------------------------------------------------

# A operação "median" embutida no exactextractr (testada na versão 0.10.0)
# interpola entre valores: num contorno com 99 pixels de 3 m e 1 pixel de
# 30 m ela devolve 16,5 m. Para alturas, interessa o valor que divide o peso
# acumulado ao meio, sem interpolação. A função abaixo ordena os valores,
# acumula as frações de cobertura e devolve o primeiro valor que atinge 50%.
# No mesmo caso de teste, devolve 3 m.
mediana_ponderada <- function(values, coverage_fraction) {
  ok <- !is.na(values)
  v  <- values[ok]
  w  <- coverage_fraction[ok]
  if (length(v) == 0 || sum(w) == 0) return(NA_real_)
  o  <- order(v)
  v  <- v[o]
  pa <- cumsum(w[o]) / sum(w)   # peso acumulado relativo
  v[which(pa >= 0.5)[1]]
}

# 5.3 - Estatística zonal
# ------------------------------------------------------------------------------

# h_med: mediana ponderada da altura (função da seção 5.2).
# n_pix: operação "count" do exactextractr, soma das frações de cobertura
#        dos pixels válidos, isto é, quantos pixels de 4 m (equivalentes)
#        sustentam a mediana.
fp <- fp |>
  dplyr::mutate(
    h_med = exactextractr::exact_extract(h_mascarada, fp,
                                         fun = mediana_ponderada, progress = TRUE),
    n_pix = exactextractr::exact_extract(h_mascarada, fp,
                                         fun = "count", progress = TRUE)
  )

# 5.4 - Imputação das alturas ausentes
# ------------------------------------------------------------------------------

# Sem pixels válidos suficientes, a edificação recebe 1 pavimento e é marcada,
# para que a proporção imputada seja relatada na seção 10
fp <- fp |>
  dplyr::mutate(
    imputado = is.na(h_med) | n_pix < COBERTURA_MIN,
    h_est    = dplyr::if_else(imputado, ALT_PADRAO, h_med)
  )


# ------------------------------------------------------------------------------
# 6. - Discretização e classes visuais
# ------------------------------------------------------------------------------

# 6.1 - Pavimentos
# ------------------------------------------------------------------------------

# Altura em pavimentos inteiros (mínimo 1) e altura de cena correspondente.
# A discretização reduz o ruído da resolução efetiva de 4 m, que se
# manifesta como alturas fracionárias em casario térreo.
# Desconta-se TELHADO_M antes da divisão: sem o desconto, a cumeeira de uma
# casa térrea (cerca de 5 m) era arredondada para 2 pavimentos. Em 01/11/2026,
# a regra sem desconto deu 42.357 edificações de 1 pavimento e 71.079 de 2;
# com o desconto de 1,5 m, 99.930 e 15.984.
fp <- fp |>
  dplyr::mutate(
    pav    = pmax(1L, as.integer(round((h_est - TELHADO_M) / PE_DIREITO))),
    h_cena = pav * PE_DIREITO
  )

# 6.2 - Classe visual
# ------------------------------------------------------------------------------

# Três classes para a paleta da cena; regras heurísticas, sem uso do solo real
fp <- fp |>
  dplyr::mutate(
    tipo = dplyr::case_when(
      area_m2 >= GALPAO_AREA_MIN & pav <= GALPAO_PAV_MAX ~ "galpao",
      pav >= VERTICAL_PAV_MIN                            ~ "vertical",
      TRUE                                               ~ "casa"
    )
  )


# ------------------------------------------------------------------------------
# 7. - Cota de base (relevo)
# ------------------------------------------------------------------------------

if (USAR_RELEVO) {

  # 7.1 - Suavização do MDS
  # ----------------------------------------------------------------------------

  # A média móvel atenua o sinal de edificações e copas embutido no GLO-30,
  # deixando a forma geral do terreno
  dem_suave <- terra::focal(r_dem, w = JANELA_RELEVO, fun = "mean", na.rm = TRUE)

  # 7.2 - Cota relativa
  # ----------------------------------------------------------------------------

  # A cota mínima da AOI vira zero; a cena trabalha com desníveis, não altitudes
  z_min   <- terra::global(dem_suave, "min", na.rm = TRUE)[1, 1]
  dem_rel <- dem_suave - z_min

  # 7.3 - Cota média sob cada edificação
  # ----------------------------------------------------------------------------

  fp$z_base <- exactextractr::exact_extract(dem_rel, fp, fun = "mean", progress = TRUE)
  fp$z_base[is.na(fp$z_base)] <- 0

} else {
  fp$z_base <- 0
}


# ------------------------------------------------------------------------------
# 8. - Camadas do OpenStreetMap
# ------------------------------------------------------------------------------

# As camadas vêm de um arquivo .osm.pbf local (extrato regional do Geofabrik),
# lido com o pacote osmextract. Esta via substitui as consultas à Overpass API,
# que falharam em 01/11/2026 (erro de SSL em overpass-api.de; HTTP 504 em
# duas instâncias alternativas).

# 8.1 - Arquivo OSM local e funções auxiliares
# ------------------------------------------------------------------------------

# Localiza o único arquivo .osm.pbf em data/raw/osm/ (ver seção 2.7)
arq_pbf <- list.files(DIR_OSM, pattern = "\\.osm\\.pbf$", full.names = TRUE)
if (length(arq_pbf) != 1) {
  stop("Esperado exatamente 1 arquivo .osm.pbf em ", DIR_OSM,
       "; encontrados: ", length(arq_pbf))
}
message("Arquivo OSM: ", basename(arq_pbf))

# O osmextract recorta pela AOI em coordenadas geográficas (WGS 84)
aoi_wgs <- sf::st_transform(aoi, 4326)

# Moldura da água: retângulo envolvente da AOI ampliado por MARGEM_AGUA_M
# (seção 2.8), em UTM para o recorte final e em WGS 84 para a leitura
moldura_utm <- aoi |>
  sf::st_buffer(MARGEM_AGUA_M) |>
  sf::st_bbox() |>
  sf::st_as_sfc()
moldura_wgs <- sf::st_transform(moldura_utm, 4326)

# Lê uma camada do arquivo OSM já recortada por um limite (por padrão, a AOI).
# boundary_type = "clipsrc" corta as geometrias no limite durante a
# conversão do .pbf para GeoPackage (feita pelo GDAL, uma vez por camada).
# force_vectortranslate = TRUE refaz a conversão a cada chamada, para que o
# GeoPackage intermediário contenha sempre a camada pedida.
# extra: etiquetas do OSM a acrescentar como colunas (ex.: oneway, surface),
# que a configuração padrão do osmextract não traz
ler_osm <- function(camada, limite = aoi_wgs, extra = NULL) {
  osmextract::oe_read(
    arq_pbf,
    layer                 = camada,
    extra_tags            = extra,
    boundary              = limite,
    boundary_type         = "clipsrc",
    force_vectortranslate = TRUE,
    quiet                 = FALSE
  ) |>
    sf::st_transform(CRS_UTM)
}

# Recorta uma camada pela AOI em UTM, preservando os atributos
# (segurança adicional após o recorte geográfico do osmextract)
recortar <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  suppressWarnings(sf::st_intersection(x, sf::st_geometry(aoi)))
}

# 8.2 - Vias
# ------------------------------------------------------------------------------

# Camada "lines" do OSM: filtra as classes listadas em LARGURA_VIAS, converte
# em faixas (buffer de meia largura) e funde por classe para reduzir o número
# de geometrias enviadas ao navegador
linhas_osm <- ler_osm("lines", extra = c("oneway", "surface", "junction", "aeroway", "width"))

vias <- linhas_osm |>
  dplyr::filter(highway %in% names(LARGURA_VIAS)) |>
  dplyr::select(osm_id, highway) |>
  dplyr::mutate(largura = unname(LARGURA_VIAS[highway]))

if (nrow(vias) == 0) stop("Nenhuma via das classes de LARGURA_VIAS na AOI.")

vias <- vias |>
  sf::st_buffer(dist = vias$largura / 2) |>
  dplyr::group_by(classe = highway, largura) |>
  dplyr::summarise(.groups = "drop") |>
  recortar()

# Converte o resultado de um recorte em linhas simples (descarta pontos e
# coleções que a interseção pode gerar)
para_linhas <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x |>
    sf::st_collection_extract("LINESTRING") |>
    suppressWarnings() |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING", warn = FALSE)
}

# 8.2.1 - Eixos das vias de carro (piso vetorial)
# ------------------------------------------------------------------------------

# Cada trecho guarda a classe-base, se é mão única, a largura desenhada, o
# pavimento e o tipo de marcação. Pavimento (opção A): "terra" e "bloquete"
# só onde o OSM registra; o restante, inclusive sem etiqueta, é desenhado
# como "pavimentada"; "track" sem etiqueta é terra.
eixos <- linhas_osm |>
  dplyr::filter(highway %in% names(CLASSES_CARRO)) |>
  dplyr::mutate(
    classe        = unname(CLASSES_CARRO[highway]),
    mao_unica     = oneway %in% c("yes", "1", "true", "-1") | junction %in% "roundabout",
    largura       = unname(LARGURA_EIXO[classe]) *
                    dplyr::if_else(mao_unica & classe %in% CLASSES_PISTA_DUPLA,
                                   FATOR_MAO_UNICA, 1),
    pav_informado = surface %in% SURFACE_VALIDOS,
    pavimento     = dplyr::case_when(
      surface %in% SURFACE_TERRA                ~ "terra",
      surface %in% SURFACE_BLOQUETE             ~ "bloquete",
      classe == "track" & !pav_informado        ~ "terra",
      TRUE                                      ~ "pavimentada"
    ),
    marcacao      = dplyr::case_when(
      pavimento == "terra"                        ~ "nenhuma",
      classe %in% CLASSES_MARCADAS & mao_unica    ~ "mao_unica",
      classe %in% CLASSES_MARCADAS                ~ "mao_dupla",
      TRUE                                        ~ "nenhuma"
    )
  ) |>
  dplyr::select(osm_id, classe, mao_unica, largura, pavimento, pav_informado, marcacao) |>
  recortar() |>
  para_linhas() |>
  sf::st_simplify(dTolerance = SIMPLIFICA_M, preserveTopology = TRUE) |>
  dplyr::mutate(km = as.numeric(sf::st_length(geometry)) / 1000) |>
  dplyr::filter(km > 0.001)

if (is.null(eixos) || nrow(eixos) == 0) stop("Nenhum eixo de via de carro na AOI.")

# Extensão por pavimento e parcela com pavimento informado, usadas na
# ressalva gravada nos metadados (seção 9.3)
km_por_pavimento <- tapply(eixos$km, eixos$pavimento, sum) |> round(1)
pct_pav_informado <- round(100 * sum(eixos$km[eixos$pav_informado]) / sum(eixos$km), 1)
message("Eixos de via: ", round(sum(eixos$km), 1), " km; pavimento informado em ",
        pct_pav_informado, "% da extensão")
print(km_por_pavimento)

# 8.2.2 - Vias de pedestre e ciclovias
# ------------------------------------------------------------------------------

pedestres <- linhas_osm |>
  dplyr::filter(highway %in% names(LARGURA_PEDESTRE)) |>
  dplyr::mutate(classe = highway, largura = unname(LARGURA_PEDESTRE[highway])) |>
  dplyr::select(osm_id, classe, largura) |>
  recortar() |>
  para_linhas()

# 8.3 - Água
# ------------------------------------------------------------------------------

# Camada "multipolygons" do OSM: polígonos de área (lagos, leito de rios,
# parques, matas). É lida uma vez, recortada pela moldura (seção 8.1), e
# usada nas seções 8.3 e 8.4.
poligonos_osm <- ler_osm("multipolygons", limite = moldura_wgs) |>
  sf::st_make_valid()

# natural=water inclui o leito do rio Tocantins mapeado como polígono.
# summarise() sem grupos funde tudo em uma feição. A água é recortada pela
# moldura, e não pela AOI, para que o leito do rio entre inteiro na cena.
agua <- poligonos_osm |>
  dplyr::filter(natural %in% "water")

if (nrow(agua) > 0) {
  agua <- agua |>
    dplyr::summarise() |>
    sf::st_intersection(moldura_utm) |>
    suppressWarnings()
} else {
  agua <- NULL
}

# 8.3.1 - Eixo do rio (rota dos barcos)
# ------------------------------------------------------------------------------

# Linha central dos rios (waterway = river) dentro da moldura da água. A AOI
# termina na margem, e o eixo do Tocantins fica fora dela; por isso a camada
# "lines" é lida de novo, agora pela moldura (cerca de 2 minutos). A página
# usa essas linhas como rota dos barcos.
rio <- ler_osm("lines", limite = moldura_wgs) |>
  dplyr::filter(waterway %in% "river") |>
  sf::st_intersection(moldura_utm) |>
  suppressWarnings() |>
  dplyr::select(name) |>
  para_linhas()

if (!is.null(rio)) {
  rio <- rio |>
    sf::st_simplify(dTolerance = 5, preserveTopology = TRUE) |>
    dplyr::mutate(km = as.numeric(sf::st_length(geometry)) / 1000) |>
    dplyr::filter(km >= 2) |>
    dplyr::select(name)
  if (nrow(rio) == 0) rio <- NULL
}
message("Rio (rota dos barcos): ", ifelse(is.null(rio), 0, nrow(rio)), " trecho(s)")

# 8.4 - Áreas verdes
# ------------------------------------------------------------------------------

# Quatro marcações combinadas: praças e parques, gramados, matas e florestas.
# A coluna origem registra a marcação; cada marcação é fundida em uma feição.
# %in% trata os valores ausentes (NA) como falso, sem gerar erro.
verde <- poligonos_osm |>
  dplyr::mutate(origem = dplyr::case_when(
    leisure %in% "park"   ~ "leisure=park",
    landuse %in% "grass"  ~ "landuse=grass",
    natural %in% "wood"   ~ "natural=wood",
    landuse %in% "forest" ~ "landuse=forest",
    TRUE                  ~ NA_character_
  )) |>
  dplyr::filter(!is.na(origem))

if (nrow(verde) > 0) {
  verde <- verde |>
    dplyr::group_by(origem) |>
    dplyr::summarise(.groups = "drop") |>
    recortar()
} else {
  verde <- NULL
}

# 8.4.1 - Árvores e gramados do Dynamic World
# ------------------------------------------------------------------------------

# A classificação de 10 m é recortada pela AOI, reduzida a duas classes
# (1 = árvores, 2 = gramados), convertida em polígonos e filtrada por área
arvoredo <- NULL
if (tem_dw) {
  if (!terra::same.crs(r_dw, crs_alvo)) r_dw <- terra::project(r_dw, crs_alvo, method = "near")
  dw_aoi <- terra::mask(terra::crop(r_dw, terra::vect(aoi)), terra::vect(aoi))

  # reclassificação: árvores -> 1, gramados -> 2, demais classes -> sem dado
  tabela_dw <- rbind(cbind(CLASSES_DW_ARVORES, 1), cbind(CLASSES_DW_GRAMA, 2))
  dw_cls <- terra::classify(dw_aoi, tabela_dw, others = NA)

  # polígonos por classe; cada mancha contínua vira uma feição
  manchas <- terra::as.polygons(dw_cls) |>
    sf::st_as_sf() |>
    sf::st_set_crs(CRS_UTM) |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_cast("POLYGON", warn = FALSE)
  names(manchas)[1] <- "classe_dw"
  manchas <- manchas |>
    dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
    dplyr::filter((classe_dw == 1 & area_m2 >= AREA_MIN_ARVOREDO_M2) |
                  (classe_dw == 2 & area_m2 >= AREA_MIN_GRAMA_M2)) |>
    sf::st_simplify(dTolerance = SIMPLIFICA_VERDE_M, preserveTopology = TRUE) |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON") |>
    suppressWarnings()

  arvoredo <- manchas |>
    dplyr::filter(classe_dw == 1) |>
    dplyr::transmute(origem = "dynamic_world_arvores")

  grama_dw <- manchas |>
    dplyr::filter(classe_dw == 2) |>
    dplyr::transmute(origem = "dynamic_world_grama")

  # o gramado do Dynamic World se soma ao verde do OSM (seção 8.4)
  verde <- dplyr::bind_rows(verde, grama_dw)

  message("Dynamic World: ", nrow(arvoredo), " manchas de árvores (",
          round(sum(as.numeric(sf::st_area(arvoredo))) / 1e4, 1), " ha); ",
          nrow(grama_dw), " manchas de gramado (",
          round(sum(as.numeric(sf::st_area(grama_dw))) / 1e4, 1), " ha)")
}

# 8.4.2 - Aeroporto
# ------------------------------------------------------------------------------

# Pistas e taxiways são linhas do OSM com largura (etiqueta width ou padrão
# da seção 2.10); pátios são polígonos. A página desenha a pista em cinza
# com a linha central tracejada.
pistas <- linhas_osm |>
  dplyr::filter(aeroway %in% names(LARGURA_AEROVIA)) |>
  dplyr::mutate(
    tipo    = aeroway,
    w_osm   = suppressWarnings(as.numeric(gsub(",", ".", width))),
    largura = dplyr::if_else(!is.na(w_osm) & w_osm >= 5 & w_osm <= 80,
                             w_osm, unname(LARGURA_AEROVIA[aeroway]))
  ) |>
  dplyr::select(tipo, largura) |>
  recortar() |>
  para_linhas()

patios <- poligonos_osm |>
  dplyr::filter(aeroway %in% "apron") |>
  dplyr::summarise() |>
  recortar()
if (!is.null(patios) && nrow(patios) == 0) patios <- NULL

message("Aeroporto: ", ifelse(is.null(pistas), 0, nrow(pistas)), " trechos de pista/taxiway; ",
        ifelse(is.null(patios), 0, nrow(patios)), " pátio(s)")

# 8.5 - Quadras
# ------------------------------------------------------------------------------

# Quadra = AOI menos o leito das vias de carro (asfalto + calçada dos dois
# lados). nQuadSegs = 4 limita os vértices das curvas do buffer; a união de
# milhares de buffers pode levar alguns minutos.
leito <- eixos |>
  sf::st_buffer(dist = eixos$largura / 2 + CALCADA_M, nQuadSegs = 4) |>
  sf::st_union()

geom_quadras <- sf::st_difference(sf::st_geometry(aoi), leito) |>
  sf::st_collection_extract("POLYGON") |>
  suppressWarnings() |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_cast("POLYGON", warn = FALSE)

quadras <- sf::st_sf(geometry = geom_quadras) |>
  dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::filter(area_m2 >= AREA_MIN_QUADRA)

message("Quadras: ", nrow(quadras))

# 8.6 - Faixas de pedestre
# ------------------------------------------------------------------------------

# Só as faixas mapeadas no OSM (highway = crossing, camada de pontos). Cada
# faixa recebe a direção e a largura do eixo mais próximo, para ser desenhada
# atravessando a via.
pontos_osm <- ler_osm("points")
faixas <- pontos_osm |> dplyr::filter(highway %in% "crossing")

# Ângulo (radianos, a partir do leste) do segmento do eixo mais próximo do ponto
angulo_via <- function(p, linha) {
  xy  <- sf::st_coordinates(linha)[, 1:2, drop = FALSE]
  pp  <- sf::st_coordinates(p)[1, 1:2]
  mid <- (xy[-1, , drop = FALSE] + xy[-nrow(xy), , drop = FALSE]) / 2
  k   <- which.min((mid[, 1] - pp[1])^2 + (mid[, 2] - pp[2])^2)
  atan2(xy[k + 1, 2] - xy[k, 2], xy[k + 1, 1] - xy[k, 1])
}

if (nrow(faixas) > 0) {
  perto <- sf::st_nearest_feature(faixas, eixos)
  faixas <- faixas |>
    dplyr::mutate(
      angulo  = vapply(seq_len(dplyr::n()), function(i)
        angulo_via(sf::st_geometry(faixas)[i], sf::st_geometry(eixos)[perto[i]]), numeric(1)),
      largura = eixos$largura[perto]
    ) |>
    dplyr::select(angulo, largura)
} else {
  faixas <- NULL
}

message("OSM: ", nrow(vias), " classes de via; ",
        ifelse(is.null(agua), 0, nrow(agua)), " feição de água; ",
        ifelse(is.null(verde), 0, nrow(verde)), " feições de verde (marcações do OSM ",
        "e manchas de gramado do Dynamic World)")


# ------------------------------------------------------------------------------
# 9. - Exportação
# ------------------------------------------------------------------------------

# 9.1 - Camadas em UTM para inspeção no QGIS
# ------------------------------------------------------------------------------

arq_gpkg <- file.path(dir_proc, "cena_imperatriz_utm.gpkg")
# As camadas são gravadas com delete_layer = TRUE (no laço abaixo): cada
# camada é substituída dentro do arquivo existente, sem apagar o arquivo.
# Isso evita o erro "Permission denied" quando o .gpkg está preso pela
# própria sessão do R (ocorrido em 01/11/2026).

camadas <- list(
  aoi         = aoi,
  edificacoes = fp,
  vias        = vias,
  eixos       = eixos,
  pedestres   = pedestres,
  quadras     = quadras,
  faixas      = faixas,
  agua        = agua,
  verde       = verde,
  arvoredo    = arvoredo,
  pistas      = pistas,
  patios      = patios,
  rio         = rio
)
camadas <- Filter(Negate(is.null), camadas)

for (nm in names(camadas)) {
  sf::st_write(camadas[[nm]], arq_gpkg, layer = nm,
               delete_layer = TRUE, quiet = TRUE)
}

# 9.2 - Coordenadas locais para o navegador
# ------------------------------------------------------------------------------

# Coordenadas UTM têm 6 a 7 dígitos; em precisão simples (GPU) isso gera
# tremulação. A cena usa metros relativos ao centroide da AOI, arredondado.
origem <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(aoi)))[1, ] |> round()

para_local <- function(x) {
  g <- sf::st_geometry(x) - origem
  x <- sf::st_set_geometry(x, g)
  sf::st_crs(x) <- NA
  x
}

# Atributos enviados à cena: apenas os que a renderização usa
camadas_cena <- list(
  aoi         = aoi |> dplyr::select(nome),
  edificacoes = fp  |> dplyr::select(id, tipo, pav, h_cena, z_base, imputado),
  vias        = vias |> dplyr::select(classe, largura),
  eixos       = eixos |> dplyr::select(classe, largura, mao_unica, pavimento, marcacao),
  pedestres   = if (is.null(pedestres)) NULL else dplyr::select(pedestres, classe, largura),
  quadras     = quadras |> dplyr::select(),
  faixas      = faixas,
  agua        = agua,
  verde       = if (is.null(verde)) NULL else dplyr::select(verde, origem),
  arvoredo    = arvoredo,
  pistas      = pistas,
  patios      = patios,
  rio         = rio
)
camadas_cena <- Filter(Negate(is.null), camadas_cena)

# COORDINATE_PRECISION=1 grava coordenadas com 1 casa decimal (10 cm),
# suficiente para a escala da maquete e menor em tamanho de arquivo
for (nm in names(camadas_cena)) {
  sf::st_write(
    para_local(camadas_cena[[nm]]),
    file.path(dir_cena, paste0(nm, ".geojson")),
    driver        = "GeoJSON",
    layer_options = "COORDINATE_PRECISION=1",
    delete_dsn    = TRUE,
    quiet         = TRUE
  )
}

# 9.3 - Metadados da cena
# ------------------------------------------------------------------------------

# Registro do que a etapa 6 precisa saber e dos créditos obrigatórios
metadados <- list(
  titulo      = "Imperatriz em miniatura",
  crs_origem  = paste0("EPSG:", CRS_UTM),
  origem_utm  = list(x = unname(origem[1]), y = unname(origem[2])),
  unidade     = "metro",
  ano_alturas = ANO,
  parametros  = list(
    conf_min = CONF_MIN, area_min_m2 = AREA_MIN, presenca_min = PRESENCA_MIN,
    cobertura_min_pix = COBERTURA_MIN, pe_direito_m = PE_DIREITO,
    alt_padrao_m = ALT_PADRAO, galpao_area_min_m2 = GALPAO_AREA_MIN,
    galpao_pav_max = GALPAO_PAV_MAX, vertical_pav_min = VERTICAL_PAV_MIN,
    telhado_m = TELHADO_M, margem_agua_m = MARGEM_AGUA_M,
    calcada_m = CALCADA_M, fator_mao_unica = FATOR_MAO_UNICA,
    usar_relevo = USAR_RELEVO, janela_relevo_pix = JANELA_RELEVO
  ),
  contagens   = list(
    edificacoes = nrow(fp),
    imputadas   = sum(fp$imputado),
    por_tipo    = as.list(table(fp$tipo)),
    altura_max_cena_m = max(fp$h_cena),
    desnivel_max_m    = max(fp$z_base),
    km_eixos          = round(sum(eixos$km), 1),
    km_por_pavimento  = as.list(km_por_pavimento),
    pct_pav_informado = pct_pav_informado
  ),
  # Ressalvas exibidas no painel "Fontes" da página; os números vêm desta execução
  ressalvas   = c(
    paste0("Pavimento: o OpenStreetMap não permite distinguir com segurança asfalto, ",
           "laterita e bloquete. Vias sem registro de pavimento (",
           round(100 - pct_pav_informado, 1),
           "% da extensão das vias de carro) aparecem como pavimentadas por convenção; ",
           "terra e bloquete aparecem só onde o OSM os registra (",
           ifelse(is.na(km_por_pavimento["terra"]), 0, km_por_pavimento["terra"]),
           " km de terra)."),
    paste0("Calçadas (", CALCADA_M, " m) e quadras são derivadas das vias; larguras e ",
           "marcações seguem convenção por classe de via, pois o OSM não informa o ",
           "número de faixas na maior parte das vias."),
    paste0("AOI: traçada pela densidade das edificações do Open Buildings V3 (script 01). ",
           "A parte sudoeste do Conjunto Canto da Serra, ao sul do Sebastião Régis, ",
           "fica fora da cena: as últimas casas do conjunto, provavelmente concluídas ",
           "depois das imagens usadas pelo Open Buildings, não constam nos dados."),
    paste0("Árvores e gramados: classificação Dynamic World de ", ANO,
           " (classe mais frequente no ano, 10 m); manchas de árvores e de gramado ",
           "com pelo menos ", AREA_MIN_ARVOREDO_M2 / 1e4, " ha. As árvores da cena são ",
           "ilustrativas (densidade convencional), não um inventário arbóreo.")
  ),
  creditos    = c(
    "Edificações: Google Open Buildings V3 e 2.5D Temporal (CC-BY 4.0)",
    "Relevo: produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and © Airbus Defence and Space GmbH 2014-2018 provided under COPERNICUS by the European Union and ESA; all rights reserved",
    "Vias, água, verde, ferrovia e aeroporto: © colaboradores do OpenStreetMap (ODbL)",
    "Árvores e gramados: Dynamic World V1, produzido para o Dynamic World Project pelo Google em parceria com a National Geographic Society e o World Resources Institute (CC-BY 4.0)",
    "Recorte: densidade das edificações do Open Buildings V3; limite municipal IBGE 2020, via geobr (Ipea)"
  )
)

jsonlite::write_json(metadados, file.path(dir_cena, "metadados_cena.json"),
                     auto_unbox = TRUE, pretty = TRUE)


# ------------------------------------------------------------------------------
# 10. - Diagnóstico
# ------------------------------------------------------------------------------

# 10.1 - Proporção imputada
# ------------------------------------------------------------------------------

# Se a proporção for alta, a cena será dominada por blocos de 1 pavimento
# atribuídos por regra, e não por estimativa; o valor deve ser relatado
message("Edificações com altura imputada: ", sum(fp$imputado), " de ", nrow(fp),
        " (", round(100 * mean(fp$imputado), 1), "%)")

# 10.2 - Tabela de classes e pavimentos
# ------------------------------------------------------------------------------

resumo <- fp |>
  sf::st_drop_geometry() |>
  dplyr::count(tipo, pav, name = "n") |>
  dplyr::arrange(tipo, pav)

print(resumo, n = Inf)
write.csv(resumo, file.path(dir_tab, "03_resumo_classes.csv"), row.names = FALSE)

# 10.3 - Histograma das alturas estimadas
# ------------------------------------------------------------------------------

# Usa só as alturas estimadas (não imputadas), antes da discretização;
# as linhas tracejadas marcam os limites entre pavimentos
g_hist <- fp |>
  sf::st_drop_geometry() |>
  dplyr::filter(!imputado) |>
  ggplot2::ggplot(ggplot2::aes(x = h_med)) +
  ggplot2::geom_histogram(binwidth = 0.5, fill = "#1F4E79", colour = "white") +
  ggplot2::geom_vline(xintercept = seq(TELHADO_M + 1.5 * PE_DIREITO, 30, by = PE_DIREITO),
                      linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  ggplot2::coord_cartesian(xlim = c(0, 30)) +
  ggplot2::labs(
    x = "Altura mediana por edificação (m)",
    y = "Número de edificações",
    title = paste0("Imperatriz: alturas estimadas (Open Buildings 2.5D, ", ANO, ")"),
    subtitle = paste0("Linhas tracejadas: limites entre pavimentos (pé-direito de ",
                      PE_DIREITO, " m, desconto de telhado de ", TELHADO_M, " m)")
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(file.path(dir_fig, "03_hist_alturas.png"), g_hist,
                width = 18, height = 11, units = "cm", dpi = 300)

# 10.4 - Sessão
# ------------------------------------------------------------------------------

writeLines(capture.output(sessionInfo()), file.path(dir_tab, "03_sessionInfo.txt"))

message("Camadas da cena gravadas em: ", dir_cena)
