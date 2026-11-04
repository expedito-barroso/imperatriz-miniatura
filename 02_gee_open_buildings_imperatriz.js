// =============================================================================
// Imperatriz em miniatura (estilo tiny city): 02, extração no Google Earth Engine
// Recorte: AOI do núcleo urbano de Imperatriz (MA), produzida pelo script 01
// Fontes : Google Research Open Buildings V3 Polygons (2023)
//          Google Research Open Buildings 2.5D Temporal V1 (ano 2023)
//          Copernicus DEM GLO-30, versão 2024_1
//          Dynamic World V1 (cobertura do solo, Sentinel-2, 10 m), ano 2023
//
// Autor : Antonio Expedito Ferreira Barroso de Carvalho
// Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
// Data  : novembro/2026
// Ambiente: Google Earth Engine, Code Editor (JavaScript)
// Email : expebarroso@uemasul.edu.br
// =============================================================================
//
// Objetivo deste script
//   Recortar pela AOI e exportar para o Google Drive quatro arquivos:
//     (a) contornos das edificações (GeoJSON), com área e confiança;
//     (b) raster de altura e presença de edificações em 2023 (GeoTIFF, 4 m);
//     (c) relevo de base GLO-30 (GeoTIFF, 30 m);
//     (d) cobertura do solo do Dynamic World em 2023 (GeoTIFF, 10 m), para
//         as árvores e os gramados da cena (seção 5.1, acrescentada em
//         03/11/2026; nas execuções seguintes basta rodar a tarefa (d)).
//
// Execução
//   1. Ajustar ASSET_AOI na seção 1 com o caminho do asset criado na etapa 2.
//   2. Clicar em Run. Conferir o Console (contagens) e o mapa (camadas).
//   3. Abrir a aba Tasks e clicar em RUN em cada uma das exportações.
//
// Referências dos conjuntos (catálogo do GEE)
//   https://developers.google.com/earth-engine/datasets/catalog/GOOGLE_Research_open-buildings_v3_polygons
//   https://developers.google.com/earth-engine/datasets/catalog/GOOGLE_Research_open-buildings-temporal_v1
//   https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_DEM_GLO30_2024_1
//   https://developers.google.com/earth-engine/datasets/catalog/GOOGLE_DYNAMICWORLD_V1


// -----------------------------------------------------------------------------
// 1. - Parâmetros
// -----------------------------------------------------------------------------

// Caminho do asset da AOI. Substituir SEU_PROJETO pelo ID do projeto Cloud
// que aparece na aba Assets (ex.: projects/ee-expebarroso/assets/imperatriz_aoi).
var ASSET_AOI = 'projects/ee-expebarroso/assets/imperatriz_aoi';

// Ano de referência das alturas. 2023 é o último ano do conjunto Temporal.
var ANO = 2023;

// Projeção e resoluções de saída.
// EPSG:31983 = SIRGAS 2000 / UTM 23S, a mesma projeção usada no R.
// 4 m é a resolução efetiva declarada do conjunto Temporal; exportar a
// 0,5 m multiplicaria o arquivo por 64 sem ganho de informação.
var CRS_SAIDA   = 'EPSG:31983';
var ESCALA_ALT  = 4;
var ESCALA_DEM  = 30;

// Pasta do Google Drive que receberá os três arquivos
var PASTA_DRIVE = 'tiny_imperatriz';


// -----------------------------------------------------------------------------
// 2. - Área de interesse
// -----------------------------------------------------------------------------

// Lê o asset e funde as feições em uma única geometria
var aoi  = ee.FeatureCollection(ASSET_AOI);
var geom = aoi.geometry();

// Centraliza o mapa e mostra o contorno da AOI
Map.centerObject(geom, 12);
Map.setOptions('SATELLITE');
Map.addLayer(ee.Image().paint(aoi, 0, 2), {palette: ['ffff00']}, 'AOI');


// -----------------------------------------------------------------------------
// 3. - Contornos das edificações (Open Buildings V3 Polygons)
// -----------------------------------------------------------------------------

// 3.1 - Filtragem espacial e seleção de atributos
// -----------------------------------------------------------------------------

// filterBounds retém os polígonos que tocam a AOI. Não se aplica aqui um
// limiar de confiança: todos os polígonos (confiança de 0,65 a 1,0) são
// exportados e o limiar é decidido no R (script 03), onde pode ser revisto
// sem nova exportação.
// select() mantém só os atributos úteis; a geometria principal é preservada
// e a propriedade longitude_latitude (centroide, segunda geometria) é
// descartada porque complica a escrita em formatos vetoriais.
var poligonos = ee.FeatureCollection('GOOGLE/Research/open-buildings/v3/polygons')
  .filterBounds(geom)
  .select(['area_in_meters', 'confidence', 'full_plus_code']);

// 3.2 - Conferência
// -----------------------------------------------------------------------------

// Número de edificações encontradas; zero indica AOI fora da cobertura
print('Edificações (V3 Polygons) na AOI:', poligonos.size());

// Visualização por classe de confiança, como no exemplo do catálogo
Map.addLayer(poligonos.filter('confidence >= 0.65 && confidence < 0.70'),
             {color: 'FF0000'}, 'Edificações conf. [0,65; 0,70)', false);
Map.addLayer(poligonos.filter('confidence >= 0.70 && confidence < 0.75'),
             {color: 'FFFF00'}, 'Edificações conf. [0,70; 0,75)', false);
Map.addLayer(poligonos.filter('confidence >= 0.75'),
             {color: '00FF00'}, 'Edificações conf. >= 0,75', false);


// -----------------------------------------------------------------------------
// 4. - Altura e presença das edificações (Open Buildings 2.5D Temporal)
// -----------------------------------------------------------------------------

// 4.1 - Seleção das imagens do ano de referência sobre a AOI
// -----------------------------------------------------------------------------

// A coleção é formada por blocos (tiles) por célula S2 e por ano.
// filterDate pega o ano inteiro; filterBounds pega os blocos que tocam a AOI.
var temporal = ee.ImageCollection('GOOGLE/Research/open-buildings-temporal/v1')
  .filterDate(ANO + '-01-01', (ANO + 1) + '-01-01')
  .filterBounds(geom);

// Conferência de cobertura. getInfo() traz o número para o lado do cliente,
// o que permite interromper o script se não houver dado para Imperatriz.
var nImagens = temporal.size().getInfo();
print('Blocos do conjunto Temporal (' + ANO + ') sobre a AOI:', nImagens);
if (nImagens === 0) {
  throw new Error('Sem cobertura do Open Buildings Temporal para a AOI em ' + ANO +
                  '. Interrompido antes das exportações.');
}

// 4.2 - Mosaico com projeção nativa
// -----------------------------------------------------------------------------

// mosaic() perde a projeção nativa dos blocos; ela é recuperada do primeiro
// bloco (0,5 m, UTM) para que a agregação da seção 4.3 parta da grade correta.
// A AOI de Imperatriz cabe em uma única zona UTM (23S), então todos os
// blocos compartilham o mesmo sistema.
var projNativa = temporal.first().select('building_height').projection();

var mosaico = temporal
  .select(['building_height', 'building_presence'])
  .mosaic()
  .setDefaultProjection(projNativa);

// 4.3 - Agregação para 4 m
// -----------------------------------------------------------------------------

// reduceResolution calcula a média dos pixels de 0,5 m que compõem cada
// pixel de 4 m (8 x 8 = 64 pixels). maxPixels = 256 dá folga para pixels
// de borda que cruzam a grade. A grade final (CRS e escala) é fixada na
// exportação, seção 6.
var altura4m = mosaico
  .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 256})
  .clip(geom)
  .toFloat();

// 4.4 - Visualização
// -----------------------------------------------------------------------------

// O mapa mostra o mosaico original, sem reduceResolution. No mapa, cada
// nível de zoom pede pixels de tamanho diferente (cerca de 38 m no zoom 12);
// com reduceResolution, isso exigiria agregar milhares de pixels de 0,5 m
// por pixel exibido, acima do limite de 256, e o mapa devolveria
// "Too many input pixels per output pixel". A exportação da seção 6.2 não
// é afetada, porque lá a escala é fixa em 4 m (64 pixels de entrada).
var visMapa = mosaico.clip(geom);

Map.addLayer(visMapa.select('building_presence'),
             {min: 0, max: 1, palette: ['000000', 'ffffff']},
             'Presença de edificação (' + ANO + ')', false);
Map.addLayer(visMapa.select('building_height'),
             {min: 0, max: 30, palette: ['1a1a2e', 'e94560', 'ffd460']},
             'Altura das edificações, m (' + ANO + ')');


// -----------------------------------------------------------------------------
// 5. - Relevo de base (Copernicus DEM GLO-30, 2024_1)
// -----------------------------------------------------------------------------

// O GLO-30 é um Modelo Digital de Superfície: inclui edificações e copas.
// No script 03 ele é suavizado e usado só como deslocamento do terreno.
// Segue o padrão do catálogo: mosaico com a projeção nativa do primeiro bloco.
var demCol   = ee.ImageCollection('COPERNICUS/DEM/GLO30_2024_1').filterBounds(geom);
var projDem  = demCol.first().projection();

var dem = demCol
  .select('DEM')
  .mosaic()
  .setDefaultProjection(projDem)
  .clip(geom)
  .toFloat();

Map.addLayer(dem, {min: 80, max: 200, palette: ['2b83ba', 'ffffbf', 'd7191c']},
             'GLO-30 (m)', false);

// 5.1 - Cobertura do solo (Dynamic World V1, 2023)
// -----------------------------------------------------------------------------

// O Dynamic World classifica cada imagem Sentinel-2 em 9 classes, a 10 m.
// A banda label guarda a classe mais provável de cada pixel em cada imagem:
// 0 água, 1 árvores, 2 grama, 3 vegetação alagada, 4 cultivo, 5 arbustos,
// 6 construído, 7 solo exposto, 8 neve e gelo. A moda do ano (classe mais
// frequente entre as imagens de 2023) dá um mapa estável, menos sensível a
// uma imagem isolada. O script 03 transforma as classes 1 (árvores) e
// 2, 3, 4 e 5 (gramados e demais coberturas verdes) em pisos da cena.
var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filterDate(ANO + '-01-01', (ANO + 1) + '-01-01')
  .filterBounds(geom);

print('Imagens do Dynamic World (' + ANO + ') sobre a AOI:', dw.size());

var coberturaDW = dw.select('label').mode().clip(geom).toByte();

Map.addLayer(coberturaDW,
             {min: 0, max: 8, palette: ['419bdf', '397d49', '88b053', '7a87c6', 'e49635',
                                        'dfc35a', 'c4281b', 'a59b8f', 'b39fe1']},
             'Dynamic World, moda ' + ANO, false);


// -----------------------------------------------------------------------------
// 6. - Exportações para o Google Drive
// -----------------------------------------------------------------------------

// 6.1 - Contornos das edificações
// -----------------------------------------------------------------------------

// GeoJSON preserva a geometria em WGS 84 e os três atributos selecionados
Export.table.toDrive({
  collection:     poligonos,
  description:    'ob_v3_poligonos_imperatriz',
  folder:         PASTA_DRIVE,
  fileNamePrefix: 'ob_v3_poligonos_imperatriz',
  fileFormat:     'GeoJSON'
});

// 6.2 - Altura e presença, 4 m
// -----------------------------------------------------------------------------

// region usa o retângulo envolvente da AOI; os pixels fora da AOI ficam
// sem dado por causa do clip() da seção 4.3
Export.image.toDrive({
  image:          altura4m,
  description:    'ob_temporal_' + ANO + '_imperatriz',
  folder:         PASTA_DRIVE,
  fileNamePrefix: 'ob_temporal_' + ANO + '_imperatriz',
  region:         geom.bounds(),
  crs:            CRS_SAIDA,
  scale:          ESCALA_ALT,
  maxPixels:      1e10,
  fileFormat:     'GeoTIFF'
});

// 6.3 - Relevo GLO-30, 30 m
// -----------------------------------------------------------------------------

Export.image.toDrive({
  image:          dem,
  description:    'dem_glo30_imperatriz',
  folder:         PASTA_DRIVE,
  fileNamePrefix: 'dem_glo30_imperatriz',
  region:         geom.bounds(),
  crs:            CRS_SAIDA,
  scale:          ESCALA_DEM,
  maxPixels:      1e10,
  fileFormat:     'GeoTIFF'
});

// 6.4 - Cobertura do solo Dynamic World, 10 m
// -----------------------------------------------------------------------------

Export.image.toDrive({
  image:          coberturaDW,
  description:    'dw_' + ANO + '_imperatriz',
  folder:         PASTA_DRIVE,
  fileNamePrefix: 'dw_' + ANO + '_imperatriz',
  region:         geom.bounds(),
  crs:            CRS_SAIDA,
  scale:          10,
  maxPixels:      1e10,
  fileFormat:     'GeoTIFF'
});

print('Exportações criadas. Abra a aba Tasks e clique em RUN em cada uma.');
