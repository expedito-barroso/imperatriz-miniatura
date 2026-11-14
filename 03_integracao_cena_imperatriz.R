# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): 03, integração das camadas da cena
# Recorte: AOI do núcleo urbano de Imperatriz (MA), produzida pelo script 01
# Fontes : Google Open Buildings V3 Polygons e 2.5D Temporal V1 (2023), via GEE;
#          Copernicus DEM GLO-30 (2024_1), via GEE; OpenStreetMap, extrato
#          Geofabrik (.osm.pbf), via osmextract
#
# Autor : Antonio Expedito Ferreira Barroso de Carvalho
# Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
# Data  : novembro/2026 (versão 1.1.0)
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
#   data/processed/cena/verde_extra.geojson   canteiros e quadras vazias com grama (v1.1.0)
#   data/processed/cena/arvores_pontos.geojson árvores em canteiros, calçadas, praças e
#                                              casos indicados pelo autor (v1.1.0)
#   data/processed/cena/campo.geojson         campo de futebol do estádio (v1.1.0)
#   data/processed/cena/riacho.geojson        calha do riacho do caso 9 (v1.1.0)
#   data/processed/cena/bairros.geojson       bairros do OSM, para a ficha (v1.1.0)
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

# 2.11 - Vegetação pontual (versão 1.1.0)
# ------------------------------------------------------------------------------

# Árvores em posições calculadas aqui (a página só as desenha), em quatro
# regras, todas ilustrativas e declaradas na ressalva (seção 9.3):
#   canteiros: quadras sem edificação e estreitas viram gramado; as de
#     largura média igual ou maior que ARVORE_LARGURA_MIN_M recebem árvores
#     ao longo do comprimento;
#   calçadas: árvores junto ao meio-fio onde a faixa livre entre a via e a
#     primeira fachada (calçada mais recuo) tem pelo menos
#     ARVORE_LARGURA_MIN_M; só em frentes edificadas (fachada a até
#     DIST_MAX_FACHADA_M), para não enfileirar árvores ao longo de terrenos
#     vazios;
#   praças do OSM (leisure = park ou garden): árvores espalhadas, fora dos
#     caminhos e afastadas das edificações;
#   casos indicados pelo autor (seção 2.12).

# Largura média de uma quadra vazia: 2 x área / perímetro (para uma faixa
# longa, é a própria largura). Abaixo deste limiar, a quadra vazia é canteiro.
# PROVISÓRIO: conferir na tabela impressa pela seção 8.7.2 e no QGIS
# (camada verde_extra) antes de fixar.
CANTEIRO_LARGURA_MAX_M <- 20
ARVORE_LARGURA_MIN_M   <- 4      # decisão do autor (08/11/2026)
ESPACO_ARVORES_M       <- 12     # entre árvores de uma mesma fileira
RECUO_CANTEIRO_M       <- 1.5    # distância das fileiras à borda do canteiro
AFAST_MEIO_FIO_M       <- 1      # árvore de calçada: distância ao leito da via
DIST_MAX_FACHADA_M     <- 15
# só as avenidas (decisão do autor, 14/11/2026): nas ruas residenciais, a
# faixa livre inclui o recuo frontal das casas, que é quintal particular, e a
# regra gerava cerca de 80 mil árvores
CLASSES_ARBORIZADAS    <- c("trunk", "primary", "secondary", "tertiary")
ARVORES_PRACA_HA       <- 60     # árvores por hectare nas praças
ARVORES_CASO_HA        <- 150    # nos casos indicados (seção 2.12), lugares pequenos
FOLGA_LIVRE_M          <- 8      # casos indicados: faixa livre em volta das edificações
AREA_MIN_LIVRE_M2      <- 300    # casos indicados: menor pedaço livre aceito
AFAST_EDIF_ARVORE_M    <- 2      # folga entre árvore e edificação
AFAST_CAMINHO_M        <- 1.5    # folga entre árvore e caminho de pedestre
DIST_MIN_ARVORES_M     <- 5      # duas árvores mais próximas que isso: fica uma
SEMENTE_ARVORES        <- 42

# 2.12 - Casos indicados pelo autor (versão 1.1.0)
# ------------------------------------------------------------------------------

# Lugares conferidos pelo autor na cena, por conhecimento local, a partir do
# duplo clique numa edificação vizinha (âncora; latitude e longitude do ponto
# clicado, SIRGAS 2000). O tratamento de cada caso está na seção 8.7.6.
#   tipo "quadra_arvores": a quadra vazia mais próxima da âncora (até
#     RAIO_ANCORA_M) vira gramado com árvores espalhadas;
#   tipo "fileira_oposta": a avenida que margeia essa quadra (excluída a rua
#     da âncora) recebe uma fileira de árvores do lado oposto, ao longo da
#     quadra vizinha;
#   tipo "campo": o vazio cercado pelas edificações-âncora (arquibancadas)
#     recebe gramado com marcação de campo de futebol;
#   tipo "praca_frente": na praça mais próxima da âncora, árvores só nos
#     N_FRENTE recortes (entre caminhos) mais próximos da âncora;
#   tipo "riacho": na área verde mais próxima da âncora, calha sinuosa no
#     meio e três fileiras de árvores de cada lado.
# escolha: posição do candidato na lista impressa (1 = o mais próximo); se a
# conferência no QGIS mostrar o lugar errado, troque para 2 ou 3.
CASOS_INDICADOS <- data.frame(
  caso    = c("0", "2", "2", "2", "3", "3", "7", "9"),
  tipo    = c("quadra_arvores", "campo", "campo", "campo",
              "quadra_arvores", "fileira_oposta", "praca_frente", "riacho"),
  edificacao_ref = c(77626, 54668, 38159, 67082, 39534, 39534, 36559, 130688),
  lat     = c(-5.520410, -5.524784, -5.525531, -5.525708, -5.525435, -5.525435, -5.534344, -5.486672),
  lon     = c(-47.472002, -47.488661, -47.487907, -47.488946, -47.487276, -47.487276, -47.477685, -47.444718),
  escolha = c(1, 1, 1, 1, 1, 1, 1, 1),
  referencia = c("triângulo junto à marginal oeste da BR-010",
                 "estádio: arquibancada norte", "estádio: arquibancada leste",
                 "estádio: arquibancada oeste",
                 "triângulo em frente à edificação", "avenida do outro lado do triângulo",
                 "praça: só os 3 canteiros da frente", "faixa verde do riacho"),
  stringsAsFactors = FALSE
)
RAIO_ANCORA_M      <- 40
N_FRENTE           <- 3
CAMPO_PROPORCAO    <- 1.55   # comprimento / largura de um campo oficial (105 x 68 m)
CAMPO_COMPR_MAX_M  <- 105
CAMPO_FOLGA_M      <- 3      # distância mínima do campo às arquibancadas
RIACHO_FRACOES     <- c(0.25, 0.55, 0.85)   # fileiras: fração da meia largura
RIACHO_AMPLITUDE   <- 0.18   # sinuosidade: fração da meia largura (máx. 6 m)
RIACHO_ONDA_M      <- 70     # comprimento de onda da sinuosidade

# 2.13 - Árvores indicadas pelo autor (versão 1.1.0)
# ------------------------------------------------------------------------------

# Árvores isoladas, conferidas pelo autor por conhecimento local e na imagem
# de satélite, com altura e diâmetro de copa informados: a página desenha
# cada uma nessas medidas, sem sorteio (seção 8.7.8). Posição em latitude e
# longitude (SIRGAS 2000), obtida a partir do duplo clique em edificações
# vizinhas.
ARVORES_INDICADAS <- data.frame(
  id         = c("uemasul_tamandare_1"),
  lat        = c(-5.535988),
  lon        = c(-47.483042),
  altura_m   = c(15),
  copa_m     = c(13),
  referencia = c("UEMASUL, dentro do muro da Rua Tamandaré: em frente à edificação 55924 e a 22,5 m da 62970 ao longo da rua"),
  stringsAsFactors = FALSE
)


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
  # nome: usado na ficha do duplo clique (via mais próxima, versão 1.1.0)
  dplyr::mutate(nome = name) |>
  dplyr::select(osm_id, nome, classe, mao_unica, largura, pavimento, pav_informado, marcacao) |>
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
    leisure %in% "garden" ~ "leisure=garden",   # versão 1.1.0
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


# 8.7 - Vegetação pontual e casos indicados (versão 1.1.0)
# ------------------------------------------------------------------------------

# 8.7.0 - Funções auxiliares
# ------------------------------------------------------------------------------

# Pontos a cada "espaco" metros ao longo de uma linha (matriz x, y), com a
# direção do segmento (ux, uy) em cada ponto
amostrar_linha <- function(xy, espaco, inicio = espaco / 2) {
  d <- sqrt(rowSums(diff(xy)^2))
  if (length(d) == 0 || sum(d) < inicio) return(NULL)
  ac <- c(0, cumsum(d))
  s  <- seq(inicio, ac[length(ac)], by = espaco)
  i  <- pmin(findInterval(s, ac, rightmost.closed = TRUE), length(d))
  dd <- ifelse(d[i] > 0, d[i], 1)
  f  <- (s - ac[i]) / dd
  cbind(x  = xy[i, 1] + f * (xy[i + 1, 1] - xy[i, 1]),
        y  = xy[i, 2] + f * (xy[i + 1, 2] - xy[i, 2]),
        ux = (xy[i + 1, 1] - xy[i, 1]) / dd,
        uy = (xy[i + 1, 2] - xy[i, 2]) / dd)
}

# Pontos a cada "espaco" metros ao longo de todas as linhas de uma geometria
pontos_em_linhas <- function(geom, espaco) {
  geom <- geom[!sf::st_is_empty(geom)]
  if (length(geom) == 0) return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
  lin <- geom |> sf::st_cast("MULTILINESTRING") |> sf::st_cast("LINESTRING") |> suppressWarnings()
  xy  <- sf::st_coordinates(lin)
  partes <- split(seq_len(nrow(xy)), xy[, "L1"])
  m <- do.call(rbind, lapply(partes, function(k) amostrar_linha(xy[k, 1:2, drop = FALSE], espaco)))
  if (is.null(m)) return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
  m[, c("x", "y"), drop = FALSE]
}

# Matriz x, y -> pontos sf com a origem da árvore
para_pontos <- function(m, origem) {
  if (is.null(m) || nrow(m) == 0) return(NULL)
  sf::st_as_sf(data.frame(x = m[, 1], y = m[, 2], origem = origem),
               coords = c("x", "y"), crs = CRS_UTM)
}

# Distância de cada ponto à edificação mais próxima. st_nearest_feature usa
# índice espacial e é rápido; st_is_within_distance, nesta versão do sf,
# compara todos os pares (cerca de 100 s para 20 mil pontos no teste de
# 09/11/2026) e por isso não é usado.
dist_edificacao <- function(pts) {
  viz <- sf::st_nearest_feature(pts, fp)
  as.numeric(sf::st_distance(pts, fp[viz, ], by_element = TRUE))
}

# Descarta pontos sobre o leito das vias, na água, sobre caminhos de pedestre
# ou a menos de "folga" metros de uma edificação. Os polígonos grandes (vias,
# água) vêm primeiro em st_intersects, para que o GEOS os prepare uma vez.
filtrar_pontos <- function(pts, folga = AFAST_EDIF_ARVORE_M, caminhos = TRUE) {
  if (is.null(pts) || nrow(pts) == 0) return(pts)
  fora <- rep(FALSE, nrow(pts))
  fora[unlist(sf::st_intersects(vias, pts))] <- TRUE
  if (!is.null(agua)) fora[unlist(sf::st_intersects(agua, pts))] <- TRUE
  if (caminhos && !is.null(faixa_caminhos)) fora[unlist(sf::st_intersects(faixa_caminhos, pts))] <- TRUE
  pts <- pts[!fora, ]
  if (nrow(pts) == 0) return(pts)
  pts[dist_edificacao(pts) > folga, ]
}

# Árvores espalhadas ao acaso dentro de polígonos, na densidade das praças
espalhar <- function(poligonos, origem, densidade = ARVORES_PRACA_HA) {
  if (is.null(poligonos) || length(poligonos) == 0) return(NULL)
  area_ha <- as.numeric(sf::st_area(poligonos)) / 1e4
  n <- pmax(2, round(area_ha * densidade * 1.5))   # sobra para os filtros
  pts <- sf::st_sample(poligonos, size = n, type = "random") |> suppressMessages()
  pts <- sf::st_sf(origem = origem, geometry = sf::st_cast(pts, "POINT")) |> suppressWarnings()
  filtrar_pontos(pts)
}

# Fica um ponto por célula de DIST_MIN_ARVORES_M (rareamento simples)
rarear <- function(pts) {
  if (is.null(pts) || nrow(pts) == 0) return(pts)
  xy <- sf::st_coordinates(pts)
  pts[!duplicated(paste(floor(xy[, 1] / DIST_MIN_ARVORES_M), floor(xy[, 2] / DIST_MIN_ARVORES_M))), ]
}

# Caminhos de pedestre com folga, para afastar as árvores (seção 2.11)
faixa_caminhos <- if (is.null(pedestres)) NULL else
  sf::st_buffer(pedestres, pedestres$largura / 2 + AFAST_CAMINHO_M) |> sf::st_union() |> sf::st_sf()

set.seed(SEMENTE_ARVORES)

# 8.7.1 - Quadras vazias e largura média
# ------------------------------------------------------------------------------

# Vazia: nenhuma edificação toca a quadra. Largura média = 2 x área / perímetro
quadras <- quadras |>
  dplyr::mutate(
    perimetro_m     = as.numeric(sf::st_length(sf::st_boundary(geometry))),
    largura_media_m = 2 * area_m2 / perimetro_m,
    vazia           = lengths(sf::st_intersects(geometry, fp)) == 0
  )
vazias <- quadras |> dplyr::filter(vazia)
message("Quadras vazias: ", nrow(vazias), " de ", nrow(quadras))
print(round(stats::quantile(vazias$largura_media_m, c(0.1, 0.25, 0.5, 0.75, 0.9)), 1))

# 8.7.2 - Canteiros: tabela de limiares
# ------------------------------------------------------------------------------

# Quantas quadras vazias seriam canteiro em cada limiar de largura média.
# CANTEIRO_LARGURA_MAX_M (seção 2.11) é provisório até a conferência desta
# tabela e da camada verde_extra no QGIS.
tab_canteiros <- data.frame(limiar_m = c(10, 15, 20, 25, 30))
tab_canteiros$n  <- sapply(tab_canteiros$limiar_m, function(k) sum(vazias$largura_media_m < k))
tab_canteiros$ha <- sapply(tab_canteiros$limiar_m, function(k)
  round(sum(vazias$area_m2[vazias$largura_media_m < k]) / 1e4, 1))
print(tab_canteiros)

canteiros <- vazias |> dplyr::filter(largura_media_m < CANTEIRO_LARGURA_MAX_M)
message("Canteiros (largura média < ", CANTEIRO_LARGURA_MAX_M, " m): ", nrow(canteiros))

# 8.7.3 - Árvores dos canteiros
# ------------------------------------------------------------------------------

# Canteiros com largura média de pelo menos ARVORE_LARGURA_MIN_M: fileiras a
# RECUO_CANTEIRO_M da borda (nos estreitos, as duas bordas recuadas se
# encontram no meio e o rareamento deixa uma fileira só)
arv_canteiro <- NULL
cant_arv <- canteiros |> dplyr::filter(largura_media_m >= ARVORE_LARGURA_MIN_M)
if (nrow(cant_arv) > 0) {
  recuo  <- pmin(RECUO_CANTEIRO_M, cant_arv$largura_media_m / 2 - 0.5)
  miolo  <- sf::st_buffer(sf::st_geometry(cant_arv), -recuo)
  arv_canteiro <- pontos_em_linhas(sf::st_boundary(miolo), ESPACO_ARVORES_M) |>
    para_pontos("canteiro") |> filtrar_pontos() |> rarear()
}
message("Árvores em canteiros: ", ifelse(is.null(arv_canteiro), 0, nrow(arv_canteiro)))

# 8.7.4 - Árvores das calçadas (faixa livre de pelo menos 4 m)
# ------------------------------------------------------------------------------

# Candidatas a cada ESPACO_ARVORES_M dos dois lados de cada eixo, a
# AFAST_MEIO_FIO_M além do leito (largura cheia da classe). Faixa livre =
# AFAST_MEIO_FIO_M + distância até a fachada mais próxima. Ficam as
# candidatas com faixa livre >= ARVORE_LARGURA_MIN_M e fachada a até
# DIST_MAX_FACHADA_M.
base_calc <- eixos |> dplyr::filter(classe %in% CLASSES_ARBORIZADAS)
xy_e   <- sf::st_coordinates(base_calc)
afast  <- unname(LARGURA_EIXO[base_calc$classe]) / 2 + AFAST_MEIO_FIO_M
partes <- split(seq_len(nrow(xy_e)), xy_e[, "L1"])
cand <- do.call(rbind, lapply(names(partes), function(l) {
  a <- amostrar_linha(xy_e[partes[[l]], 1:2, drop = FALSE], ESPACO_ARVORES_M)
  if (is.null(a)) return(NULL)
  w <- afast[as.integer(l)]
  rbind(cbind(a[, "x"] - a[, "uy"] * w, a[, "y"] + a[, "ux"] * w),
        cbind(a[, "x"] + a[, "uy"] * w, a[, "y"] - a[, "ux"] * w))
}))
cand <- para_pontos(cand, "calcada")
message("Candidatas nas calçadas: ", nrow(cand))
cand <- filtrar_pontos(cand, folga = 0)
d_fachada <- dist_edificacao(cand)
faixa_livre <- AFAST_MEIO_FIO_M + d_fachada
arv_calcada <- cand[faixa_livre >= ARVORE_LARGURA_MIN_M & d_fachada <= DIST_MAX_FACHADA_M, ] |> rarear()
message("Árvores em calçadas: ", nrow(arv_calcada), " (faixa livre >= ", ARVORE_LARGURA_MIN_M,
        " m e fachada a até ", DIST_MAX_FACHADA_M, " m)")

# 8.7.5 - Praças do OSM
# ------------------------------------------------------------------------------

pracas <- poligonos_osm |>
  dplyr::filter(leisure %in% c("park", "garden")) |>
  dplyr::select(osm_id, nome = name) |>
  recortar()
if (!is.null(pracas)) {
  pracas <- pracas |>
    sf::st_collection_extract("POLYGON") |> suppressWarnings() |>
    sf::st_cast("MULTIPOLYGON") |> sf::st_cast("POLYGON", warn = FALSE) |>
    dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
    dplyr::filter(area_m2 >= 100) |>
    # identificador próprio: praças mapeadas como contorno fechado (e não
    # como relação) vêm do osmextract sem osm_id
    dplyr::mutate(pid = dplyr::row_number())
}
message("Praças do OSM (leisure = park ou garden): ", ifelse(is.null(pracas), 0, nrow(pracas)))

# 8.7.6 - Casos indicados pelo autor
# ------------------------------------------------------------------------------

casos <- sf::st_as_sf(CASOS_INDICADOS, coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(CRS_UTM)

# Lista os candidatos mais próximos de uma âncora e devolve o escolhido
escolher <- function(ancora, alvos, rotulo, escolha = 1, raio = RAIO_ANCORA_M) {
  if (is.null(alvos) || nrow(alvos) == 0) { warning(rotulo, ": nenhum candidato"); return(NULL) }
  d <- as.numeric(sf::st_distance(ancora, alvos))
  o <- order(d)[seq_len(min(3, length(d)))]
  tab <- data.frame(posicao = seq_along(o), dist_m = round(d[o], 1),
                    area_m2 = round(as.numeric(sf::st_area(alvos[o, ]))),
                    largura_media_m = if ("largura_media_m" %in% names(alvos)) round(alvos$largura_media_m[o], 1) else NA)
  message(rotulo, ": candidatos")
  print(tab)
  if (d[o[escolha]] > raio) { warning(rotulo, ": candidato a mais de ", raio, " m da âncora"); return(NULL) }
  alvos[o[escolha], ]
}

# classes em ordem de importância (para escolher a avenida do caso 3)
ORDEM_CLASSES <- c("trunk", "primary", "secondary", "tertiary", "unclassified",
                   "residential", "living_street", "service", "track")

# lado (+1 esquerda, -1 direita) de um ponto em relação ao segmento mais
# próximo de uma linha
lado_de <- function(p, xy) {
  mid <- (xy[-1, , drop = FALSE] + xy[-nrow(xy), , drop = FALSE]) / 2
  k <- which.min((mid[, 1] - p[1])^2 + (mid[, 2] - p[2])^2)
  sign((xy[k + 1, 1] - xy[k, 1]) * (p[2] - xy[k, 2]) - (xy[k + 1, 2] - xy[k, 2]) * (p[1] - xy[k, 1]))
}

verde_casos   <- list()   # polígonos que viram gramado
arv_casos     <- list()   # árvores dos casos
campo         <- NULL
riacho        <- NULL
pracas_excluidas <- integer(0)   # praças tratadas à parte (caso praca_frente)
quadra_caso   <- list()   # quadra escolhida por caso (usada pela fileira oposta)
registro      <- list()   # geometrias escolhidas, para conferência no QGIS

for (i in seq_len(nrow(casos))) {
  cs <- casos[i, ]
  rot <- paste0("Caso ", cs$caso, " (", cs$referencia, ")")

  if (cs$tipo == "quadra_arvores") {
    # primeiro, as quadras vazias; se nenhuma estiver a até RAIO_ANCORA_M,
    # o espaço livre dentro das quadras vizinhas: a quadra menos uma faixa
    # de FOLGA_LIVRE_M em volta de cada edificação, e o pedaço livre mais
    # próximo da âncora com pelo menos AREA_MIN_LIVRE_M2. Excluída a quadra
    # da própria âncora. Caso típico: lugar vazio cujo lado não tem via de
    # carro no OSM e que, por isso, ficou fundido numa quadra com casas
    # (caso 3 na rodada de 14/11/2026: pedaço de 3.980 m² a 22,3 m)
    q <- suppressWarnings(escolher(cs, vazias, rot, cs$escolha))
    if (is.null(q)) {
      viz <- quadras[as.numeric(sf::st_distance(quadras, cs)) <= RAIO_ANCORA_M &
                       lengths(sf::st_intersects(quadras, cs)) == 0, ]
      if (nrow(viz) > 0) {
        edif  <- fp[lengths(sf::st_intersects(fp, sf::st_union(viz))) > 0, ]
        livre <- sf::st_union(viz)
        if (nrow(edif) > 0) livre <- sf::st_difference(livre, sf::st_union(sf::st_buffer(edif, FOLGA_LIVRE_M)))
        livre <- livre |> sf::st_collection_extract("POLYGON") |> sf::st_cast("POLYGON") |> suppressWarnings()
        livre <- livre[as.numeric(sf::st_area(livre)) >= AREA_MIN_LIVRE_M2]
        if (length(livre) > 0) {
          livre <- sf::st_sf(area_m2 = as.numeric(sf::st_area(livre)), geometry = livre)
          q <- escolher(cs, livre, paste0(rot, " [espaço livre nas quadras vizinhas]"), cs$escolha)
        } else warning(rot, ": nenhum espaço livre nas quadras vizinhas")
      }
    }
    if (is.null(q)) next
    quadra_caso[[cs$caso]] <- q
    verde_casos[[length(verde_casos) + 1]] <- sf::st_sf(origem = paste0("caso_", cs$caso), geometry = sf::st_geometry(q))
    arv_casos[[length(arv_casos) + 1]] <- espalhar(sf::st_geometry(q), paste0("caso_", cs$caso), ARVORES_CASO_HA)
    registro[[length(registro) + 1]] <- sf::st_sf(caso = cs$caso, tipo = cs$tipo, geometry = sf::st_geometry(q))
  }

  if (cs$tipo == "fileira_oposta") {
    tri <- quadra_caso[[cs$caso]]
    if (is.null(tri)) { warning(rot, ": quadra do caso não encontrada"); next }
    # eixos que margeiam a quadra; a rua da âncora é a mais próxima da âncora
    adj <- eixos[lengths(sf::st_is_within_distance(eixos, tri, dist = 12)) > 0, ]
    if (nrow(adj) < 2) { warning(rot, ": menos de duas vias em volta da quadra"); next }
    rua_ancora <- adj$osm_id[which.min(as.numeric(sf::st_distance(cs, adj)))]
    outras <- adj[adj$osm_id != rua_ancora, ]
    outras$ordem <- match(outras$classe, ORDEM_CLASSES)
    faixa_tri <- sf::st_buffer(sf::st_geometry(tri), 12)
    outras$compr <- vapply(seq_len(nrow(outras)), function(k)
      sum(as.numeric(sf::st_length(sf::st_intersection(sf::st_geometry(outras)[k], faixa_tri)))), numeric(1)) |>
      suppressWarnings()
    av_id <- outras$osm_id[order(outras$ordem, -outras$compr)][1]
    aven  <- eixos[eixos$osm_id == av_id, ]
    message(rot, ": avenida osm_id ", av_id, " (", aven$classe[1], ", ", aven$nome[1], ")")
    # quadra do outro lado da avenida, junto ao trecho que margeia o triângulo
    # coordenadas do trecho mais longo da avenida (após a fusão dos pedaços)
    partes_av <- sf::st_union(aven) |> sf::st_cast("MULTILINESTRING") |> sf::st_line_merge() |>
      sf::st_cast("LINESTRING") |> suppressWarnings()
    xy_av <- sf::st_coordinates(partes_av[which.max(as.numeric(sf::st_length(partes_av)))])[, 1:2]
    lado_tri <- lado_de(sf::st_coordinates(sf::st_centroid(tri))[1, ], xy_av)
    perto_av <- quadras[lengths(sf::st_is_within_distance(quadras,
                          sf::st_intersection(sf::st_union(aven), sf::st_buffer(tri, 40)),
                          dist = max(aven$largura) / 2 + CALCADA_M + 2)) > 0, ]
    perto_av <- perto_av[!sf::st_equals(perto_av, tri, sparse = FALSE)[, 1], ]
    perto_av$lado <- vapply(seq_len(nrow(perto_av)), function(k)
      lado_de(sf::st_coordinates(sf::st_point_on_surface(perto_av[k, ]))[1, ], xy_av), numeric(1)) |> suppressWarnings()
    oposta <- perto_av[perto_av$lado == -lado_tri, ]
    if (nrow(oposta) == 0) { warning(rot, ": quadra oposta não encontrada"); next }
    oposta <- oposta[which.min(as.numeric(sf::st_distance(sf::st_centroid(tri), oposta))), ]
    # fileira: o eixo da avenida ao longo da quadra oposta, deslocado para o
    # lado dela, a meia largura cheia da via mais AFAST_MEIO_FIO_M
    w   <- unname(LARGURA_EIXO[aven$classe[1]]) / 2 + AFAST_MEIO_FIO_M
    tre <- sf::st_intersection(sf::st_union(aven), sf::st_buffer(oposta, max(aven$largura) / 2 + CALCADA_M + 1)) |>
      sf::st_cast("MULTILINESTRING") |> sf::st_cast("LINESTRING") |> suppressWarnings()
    xy  <- sf::st_coordinates(tre)
    sinal <- -lado_tri
    fil <- do.call(rbind, lapply(split(seq_len(nrow(xy)), xy[, "L1"]), function(k) {
      a <- amostrar_linha(xy[k, 1:2, drop = FALSE], ESPACO_ARVORES_M)
      if (is.null(a)) return(NULL)
      cbind(a[, "x"] - a[, "uy"] * w * sinal, a[, "y"] + a[, "ux"] * w * sinal)
    }))
    arv_casos[[length(arv_casos) + 1]] <- para_pontos(fil, paste0("caso_", cs$caso)) |> filtrar_pontos(folga = 1)
    message(rot, ": fileira de ", nrow(arv_casos[[length(arv_casos)]]), " árvores ao longo da quadra oposta")
    registro[[length(registro) + 1]] <- sf::st_sf(caso = cs$caso, tipo = cs$tipo, geometry = sf::st_geometry(oposta))
  }

  if (cs$tipo == "campo" && is.null(campo)) {
    anc <- casos[casos$tipo == "campo", ]
    qi  <- unique(unlist(sf::st_intersects(anc, quadras)))
    if (length(qi) == 0) qi <- unique(sf::st_nearest_feature(anc, quadras))
    q_est <- sf::st_union(quadras[qi, ])
    arquib <- fp[lengths(sf::st_intersects(fp, q_est)) > 0, ]
    livre <- sf::st_difference(q_est, sf::st_union(sf::st_buffer(arquib, CAMPO_FOLGA_M))) |>
      sf::st_collection_extract("POLYGON") |> sf::st_cast("POLYGON") |> suppressWarnings()
    livre <- livre[which.max(as.numeric(sf::st_area(livre)))]
    # orientação: lado maior do retângulo mínimo que envolve o vazio
    cm <- sf::st_coordinates(sf::st_minimum_rotated_rectangle(livre))[1:4, 1:2]
    e1 <- cm[2, ] - cm[1, ]; e2 <- cm[3, ] - cm[2, ]
    u  <- if (sqrt(sum(e1^2)) >= sqrt(sum(e2^2))) e1 / sqrt(sum(e1^2)) else e2 / sqrt(sum(e2^2))
    n  <- c(-u[2], u[1])
    retangulo <- function(c0, larg) {
      comp <- min(larg * CAMPO_PROPORCAO, CAMPO_COMPR_MAX_M)
      p <- rbind(c0 - u * comp / 2 - n * larg / 2, c0 + u * comp / 2 - n * larg / 2,
                 c0 + u * comp / 2 + n * larg / 2, c0 - u * comp / 2 + n * larg / 2)
      sf::st_sfc(sf::st_polygon(list(rbind(p, p[1, ]))), crs = CRS_UTM)
    }
    # maior campo que cabe no vazio, testando três centros possíveis
    centros <- rbind(colMeans(cm),
                     sf::st_coordinates(sf::st_centroid(livre))[1, ],
                     sf::st_coordinates(sf::st_point_on_surface(livre))[1, ])
    melhor <- NULL; larg_melhor <- 0
    for (k in seq_len(nrow(centros))) {
      # largura máxima: a do campo oficial (105 / 1,55 = cerca de 68 m)
      lo <- 5; hi <- CAMPO_COMPR_MAX_M / CAMPO_PROPORCAO
      if (!sf::st_covers(livre, retangulo(centros[k, ], lo), sparse = FALSE)[1, 1]) next
      for (it in 1:25) { m <- (lo + hi) / 2
        if (sf::st_covers(livre, retangulo(centros[k, ], m), sparse = FALSE)[1, 1]) lo <- m else hi <- m }
      if (lo > larg_melhor) { larg_melhor <- lo; melhor <- centros[k, ] }
    }
    if (is.null(melhor)) {
      warning("Caso 2 (estádio): o vazio entre as arquibancadas não comporta um campo")
    } else {
      comp <- min(larg_melhor * CAMPO_PROPORCAO, CAMPO_COMPR_MAX_M)
      campo <- sf::st_sf(comprimento = round(comp, 1), largura = round(larg_melhor, 1),
                         angulo = atan2(u[2], u[1]), geometry = retangulo(melhor, larg_melhor))
      message("Caso 2 (estádio): campo de ", round(comp, 1), " x ", round(larg_melhor, 1), " m")
      registro[[length(registro) + 1]] <- sf::st_sf(caso = "2", tipo = "campo", geometry = sf::st_geometry(campo))
    }
  }

  if (cs$tipo == "praca_frente") {
    if (is.null(pracas)) { warning(rot, ": nenhuma praça do OSM"); next }
    pr <- escolher(cs, pracas, rot, cs$escolha, raio = 60)
    if (is.null(pr)) next
    pracas_excluidas <- c(pracas_excluidas, pr$pid)
    # recortes da praça entre os caminhos de pedestre
    rec <- sf::st_geometry(pr)
    if (!is.null(pedestres)) {
      cam <- pedestres[lengths(sf::st_intersects(pedestres, sf::st_buffer(pr, 5))) > 0, ]
      if (nrow(cam) > 0) rec <- sf::st_difference(rec, sf::st_union(sf::st_buffer(cam, cam$largura / 2 + 0.3)))
    }
    rec <- rec |> sf::st_collection_extract("POLYGON") |> sf::st_cast("POLYGON") |> suppressWarnings()
    rec <- rec[as.numeric(sf::st_area(rec)) >= 20]
    d <- as.numeric(sf::st_distance(cs, rec))
    frente <- rec[order(d)[seq_len(min(N_FRENTE, length(rec)))]]
    message(rot, ": ", length(rec), " recortes entre caminhos; árvores nos ", length(frente),
            " mais próximos da âncora (", paste(round(sort(d)[seq_along(frente)], 1), collapse = ", "), " m)")
    arv_casos[[length(arv_casos) + 1]] <- espalhar(frente, paste0("caso_", cs$caso), ARVORES_CASO_HA)
    registro[[length(registro) + 1]] <- sf::st_sf(caso = cs$caso, tipo = cs$tipo, geometry = sf::st_union(frente))
  }

  if (cs$tipo == "riacho") {
    alvos_v <- verde |> sf::st_cast("MULTIPOLYGON") |> sf::st_cast("POLYGON", warn = FALSE) |> suppressWarnings()
    vp <- escolher(cs, alvos_v, rot, cs$escolha, raio = 100)
    if (is.null(vp)) next
    poli <- sf::st_geometry(vp)
    # calha: o curso d'água do OSM dentro da área, se houver; senão, uma linha
    # pelo meio da faixa, com sinuosidade ilustrativa
    curso <- linhas_osm |> dplyr::filter(waterway %in% c("stream", "ditch", "drain", "canal"))
    curso <- if (nrow(curso) > 0) sf::st_intersection(sf::st_geometry(curso), poli) |> suppressWarnings() else NULL
    cm <- sf::st_coordinates(sf::st_minimum_rotated_rectangle(poli))[1:4, 1:2]
    e1 <- cm[2, ] - cm[1, ]; e2 <- cm[3, ] - cm[2, ]
    if (sqrt(sum(e1^2)) >= sqrt(sum(e2^2))) { u <- e1; L <- sqrt(sum(e1^2)); W <- sqrt(sum(e2^2)) } else
                                            { u <- e2; L <- sqrt(sum(e2^2)); W <- sqrt(sum(e1^2)) }
    u <- u / sqrt(sum(u^2)); n <- c(-u[2], u[1]); c0 <- colMeans(cm)
    # cortes transversais a cada ESPACO_ARVORES_M: pontas e meio da faixa
    est <- seq(-L / 2 + ESPACO_ARVORES_M / 2, L / 2 - ESPACO_ARVORES_M / 2, by = ESPACO_ARVORES_M)
    cortes <- lapply(est, function(s) {
      p0 <- c0 + u * s
      seg <- sf::st_sfc(sf::st_linestring(rbind(p0 - n * W, p0 + n * W)), crs = CRS_UTM)
      x <- sf::st_intersection(seg, poli) |> suppressWarnings()
      if (length(x) == 0 || sf::st_is_empty(x)) return(NULL)
      x <- x |> sf::st_cast("MULTILINESTRING") |> sf::st_cast("LINESTRING") |> suppressWarnings()
      x <- x[which.max(as.numeric(sf::st_length(x)))]
      xy <- sf::st_coordinates(x)[, 1:2]
      a <- xy[1, ]; b <- xy[nrow(xy), ]
      if (sum((b - a) * n) < 0) { tmp <- a; a <- b; b <- tmp }
      c(s = s, ax = unname(a[1]), ay = unname(a[2]), bx = unname(b[1]), by = unname(b[2]))
    })
    cortes <- do.call(rbind, cortes)
    if (is.null(cortes) || nrow(cortes) < 3) { warning(rot, ": faixa curta demais"); next }
    meio <- cbind((cortes[, "ax"] + cortes[, "bx"]) / 2, (cortes[, "ay"] + cortes[, "by"]) / 2)
    meia <- sqrt((cortes[, "bx"] - cortes[, "ax"])^2 + (cortes[, "by"] - cortes[, "ay"])^2) / 2
    # suavização do meio (média móvel de 5 cortes)
    suave <- function(v) { k <- stats::filter(v, rep(1 / 5, 5), sides = 2); ifelse(is.na(k), v, k) }
    meio <- cbind(suave(meio[, 1]), suave(meio[, 2]))
    amp  <- pmin(RIACHO_AMPLITUDE * meia, 6)
    desl <- amp * sin(2 * pi * cortes[, "s"] / RIACHO_ONDA_M)
    calha_xy <- meio + outer(desl, n)
    if (!is.null(curso) && length(curso) > 0 && sum(as.numeric(sf::st_length(curso))) > 0.5 * L) {
      riacho <- sf::st_sf(origem = "osm", geometry = sf::st_union(curso))
      message(rot, ": calha pelo curso d'água do OSM")
    } else {
      riacho <- sf::st_sf(origem = "ilustrativa",
                          geometry = sf::st_sfc(sf::st_linestring(calha_xy), crs = CRS_UTM))
      message(rot, ": calha ilustrativa pelo meio da faixa (", round(L), " m; meia largura mediana ",
              round(stats::median(meia), 1), " m)")
    }
    # três fileiras de cada lado, em frações da distância da calha à borda
    fil <- do.call(rbind, lapply(seq_len(nrow(cortes)), function(k) {
      ca <- calha_xy[k, ]
      dl <- sqrt(sum((ca - cortes[k, c("ax", "ay")])^2)); dr <- sqrt(sum((cortes[k, c("bx", "by")] - ca)^2))
      rbind(t(sapply(RIACHO_FRACOES, function(f) ca - n * f * dl)),
            t(sapply(RIACHO_FRACOES, function(f) ca + n * f * dr)))
    }))
    arv_casos[[length(arv_casos) + 1]] <- para_pontos(fil, paste0("caso_", cs$caso)) |> filtrar_pontos()
    registro[[length(registro) + 1]] <- sf::st_sf(caso = cs$caso, tipo = cs$tipo, geometry = poli)
  }
}

# 8.7.7 - Árvores das praças (regra geral) e reunião dos pontos
# ------------------------------------------------------------------------------

arv_pracas <- NULL
if (!is.null(pracas) && nrow(pracas) > 0) {
  pr_geral <- pracas[!pracas$pid %in% pracas_excluidas, ]
  arv_pracas <- espalhar(sf::st_geometry(pr_geral), "praca")
  # onde o Dynamic World já pôs arvoredo, a página planta as próprias árvores
  if (!is.null(arvoredo) && nrow(arvoredo) > 0 && nrow(arv_pracas) > 0) {
    sob_arvoredo <- unique(unlist(sf::st_intersects(arvoredo, arv_pracas)))
    if (length(sob_arvoredo)) arv_pracas <- arv_pracas[-sob_arvoredo, ]
  }
  arv_pracas <- rarear(arv_pracas)
  # a densidade final não passa de ARVORES_PRACA_HA
  alvo_n <- round(sum(as.numeric(sf::st_area(pr_geral))) / 1e4 * ARVORES_PRACA_HA)
  if (nrow(arv_pracas) > alvo_n) arv_pracas <- arv_pracas[sort(sample(nrow(arv_pracas), alvo_n)), ]
}

# rbind (e não bind_rows) preserva a classe sf; as listas vazias são retiradas
juntar <- function(lista) {
  lista <- Filter(function(x) !is.null(x) && nrow(x) > 0, lista)
  if (length(lista) == 0) NULL else do.call(rbind, lista)
}
arvores_pontos <- juntar(c(list(arv_canteiro, arv_calcada, arv_pracas), arv_casos)) |> rarear()
message("Árvores pontuais por origem:")
print(table(arvores_pontos$origem))

# 8.7.8 - Árvores indicadas pelo autor (seção 2.13)
# ------------------------------------------------------------------------------

# Entram depois do rareamento, para não serem descartadas; as árvores
# automáticas a menos de DIST_MIN_ARVORES_M de uma indicada saem. Altura e
# copa só são preenchidas nas indicadas; nas demais ficam vazias e a página
# sorteia as medidas.
if (!is.null(arvores_pontos)) {
  arvores_pontos$altura_m <- NA_real_
  arvores_pontos$copa_m   <- NA_real_
}
if (nrow(ARVORES_INDICADAS) > 0) {
  arv_indicadas <- sf::st_as_sf(ARVORES_INDICADAS, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(CRS_UTM) |>
    dplyr::transmute(origem = "indicada", altura_m, copa_m)
  if (!is.null(arvores_pontos)) {
    # árvores automáticas próximas demais de uma indicada
    perto <- unique(unlist(sf::st_is_within_distance(arv_indicadas, arvores_pontos, DIST_MIN_ARVORES_M)))
    if (length(perto)) arvores_pontos <- arvores_pontos[-perto, ]
  }
  arvores_pontos <- juntar(list(arvores_pontos, arv_indicadas))
  message("Árvores indicadas pelo autor: ", nrow(arv_indicadas))
}

# Gramado extra: canteiros e quadras dos casos indicados (sem repetir quadras)
verde_extra <- juntar(c(list(if (nrow(canteiros)) sf::st_sf(origem = "canteiro", geometry = sf::st_geometry(canteiros)) else NULL),
                       verde_casos))
if (!is.null(verde_extra)) verde_extra <- verde_extra[!duplicated(sf::st_as_text(sf::st_geometry(verde_extra))), ]
casos_registro <- juntar(registro)

# 8.8 - Bairros (ficha do duplo clique)
# ------------------------------------------------------------------------------

# Polígonos de bairro do OSM (limite administrativo de nível 10 ou place =
# suburb/neighbourhood/quarter); sem polígonos, os pontos place da camada de
# pontos servem de aproximação (a página mostra o bairro do ponto mais próximo)
bairros <- poligonos_osm |>
  dplyr::filter((boundary %in% "administrative" & admin_level %in% "10") |
                place %in% c("suburb", "neighbourhood", "quarter")) |>
  dplyr::filter(!is.na(name)) |>
  dplyr::select(nome = name) |>
  recortar()
if (!is.null(bairros)) bairros <- bairros |> sf::st_collection_extract("POLYGON") |> suppressWarnings()
bairros_pontos <- pontos_osm |>
  dplyr::filter(place %in% c("suburb", "neighbourhood", "quarter"), !is.na(name)) |>
  dplyr::select(nome = name)
if (nrow(bairros_pontos) == 0) bairros_pontos <- NULL
message("Bairros do OSM: ", ifelse(is.null(bairros), 0, nrow(bairros)), " polígonos; ",
        ifelse(is.null(bairros_pontos), 0, nrow(bairros_pontos)), " pontos")


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
  rio         = rio,
  verde_extra = verde_extra,
  arvores_pontos = arvores_pontos,
  campo       = campo,
  riacho      = riacho,
  bairros     = bairros,
  bairros_pontos = bairros_pontos,
  casos_indicados = casos_registro,
  ancoras     = casos
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
  eixos       = eixos |> dplyr::select(nome, classe, largura, mao_unica, pavimento, marcacao),
  pedestres   = if (is.null(pedestres)) NULL else dplyr::select(pedestres, classe, largura),
  quadras     = quadras |> dplyr::select(),
  faixas      = faixas,
  agua        = agua,
  verde       = if (is.null(verde)) NULL else dplyr::select(verde, origem),
  arvoredo    = arvoredo,
  pistas      = pistas,
  patios      = patios,
  rio         = rio,
  verde_extra = verde_extra,
  arvores_pontos = arvores_pontos,
  campo       = campo,
  riacho      = riacho,
  bairros     = bairros,
  bairros_pontos = bairros_pontos
)
# camadas opcionais da versão 1.1.0 que esta execução não produziu: o arquivo
# de uma execução anterior é apagado, para a página não mostrar dado antigo
for (nm in c("verde_extra", "arvores_pontos", "campo", "riacho", "bairros", "bairros_pontos")) {
  arq <- file.path(dir_cena, paste0(nm, ".geojson"))
  if (is.null(camadas_cena[[nm]]) && file.exists(arq)) file.remove(arq)
}
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
    usar_relevo = USAR_RELEVO, janela_relevo_pix = JANELA_RELEVO,
    canteiro_largura_max_m = CANTEIRO_LARGURA_MAX_M, arvore_largura_min_m = ARVORE_LARGURA_MIN_M,
    espaco_arvores_m = ESPACO_ARVORES_M, dist_max_fachada_m = DIST_MAX_FACHADA_M,
    arvores_praca_ha = ARVORES_PRACA_HA
  ),
  contagens   = list(
    edificacoes = nrow(fp),
    imputadas   = sum(fp$imputado),
    por_tipo    = as.list(table(fp$tipo)),
    altura_max_cena_m = max(fp$h_cena),
    desnivel_max_m    = max(fp$z_base),
    km_eixos          = round(sum(eixos$km), 1),
    km_por_pavimento  = as.list(km_por_pavimento),
    pct_pav_informado = pct_pav_informado,
    canteiros         = nrow(canteiros),
    arvores_pontuais  = if (is.null(arvores_pontos)) list() else as.list(table(arvores_pontos$origem)),
    campo_m           = if (is.null(campo)) NULL else c(campo$comprimento, campo$largura)
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
           "ilustrativas (densidade convencional), não um inventário arbóreo."),
    paste0("Canteiros, calçadas e praças (versão 1.1.0): canteiros são quadras sem edificação ",
           "com largura média abaixo de ", CANTEIRO_LARGURA_MAX_M, " m, pintadas como gramado; ",
           "recebem árvores os canteiros com pelo menos ", ARVORE_LARGURA_MIN_M, " m e as calçadas das ",
           "avenidas (vias trunk, primary, secondary e tertiary do OSM) com faixa livre (calçada mais recuo até a fachada) de pelo menos ", ARVORE_LARGURA_MIN_M,
           " m; as praças do OSM recebem árvores espalhadas. Posições e espaçamentos são convenções ",
           "visuais. Alguns lugares (praças, um estádio e a faixa de um riacho) foram indicados pelo ",
           "autor por conhecimento local; a calha do riacho é ilustrativa quando o OSM não registra ",
           "o curso d'água.")
  ),
  creditos    = c(
    "Edificações: Google Open Buildings V3 e 2.5D Temporal (CC-BY 4.0)",
    "Relevo: produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and © Airbus Defence and Space GmbH 2014-2018 provided under COPERNICUS by the European Union and ESA; all rights reserved",
    "Vias e nomes de vias, água, verde, praças, bairros, ferrovia e aeroporto: © colaboradores do OpenStreetMap (ODbL)",
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
