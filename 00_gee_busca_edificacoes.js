// =============================================================================
// Imperatriz em miniatura (estilo tiny city): 00, busca das edificações do município
// Recorte: município de Imperatriz (MA), pelo limite do FAO GAUL ampliado em 3 km
// Fonte  : Google Research Open Buildings V3 Polygons (2023)
//          FAO GAUL 2025, nível 2 (limite municipal, só para delimitar a busca)
//
// Autor : Antonio Expedito Ferreira Barroso de Carvalho
// Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
// Data  : novembro/2026
// Ambiente: Google Earth Engine, Code Editor (JavaScript)
// Email : expebarroso@uemasul.edu.br
// =============================================================================
//
// Objetivo deste script
//   Exportar para o Google Drive uma tabela leve (CSV) com o centroide, a
//   área e a confiança de todas as edificações do Open Buildings V3 no
//   município de Imperatriz. O script 01 usa essa tabela para traçar a AOI
//   pela densidade de edificações de 2023, em vez da mancha urbana do IBGE
//   de 2015, que não contém os loteamentos e condomínios recentes.
//
// Por que FAO GAUL
//   O limite do GAUL serve só para não buscar edificações no estado inteiro;
//   o recorte exato pelo limite do IBGE é feito no R (script 01). A ampliação
//   de 3 km cobre diferenças de traçado entre o GAUL e o IBGE.
//
// Execução
//   1. Colar no Code Editor, salvar e clicar em Run.
//   2. Conferir no Console: 1 município encontrado e o número de edificações.
//   3. Aba Tasks: RUN na tarefa ob_v3_centroides_municipio.
//   4. Baixar o CSV do Drive (pasta tiny_imperatriz) para data/raw/.


// -----------------------------------------------------------------------------
// 1. - Parâmetros
// -----------------------------------------------------------------------------

var NOME_MUNI   = 'Imperatriz';      // GAUL2_NAME no FAO GAUL 2025
var PAIS        = 'Brazil';          // GAUL0_NAME no FAO GAUL 2025
var AMPLIACAO_M = 3000;              // folga em torno do limite do GAUL
var PASTA_DRIVE = 'tiny_imperatriz';


// -----------------------------------------------------------------------------
// 2. - Limite de busca
// -----------------------------------------------------------------------------

// 2.1 - Município no FAO GAUL
// -----------------------------------------------------------------------------

// FAO GAUL 2025 (a versão 2015 está marcada como obsoleta no catálogo do GEE)
var muni = ee.FeatureCollection('FAO/GAUL/2025/level2')
  .filter(ee.Filter.eq('GAUL0_NAME', PAIS))
  .filter(ee.Filter.eq('GAUL2_NAME', NOME_MUNI));

// Deve haver exatamente um município com esse nome no Brasil; o script para
// se houver zero ou mais de um, antes de criar a exportação
var nMuni = muni.size().getInfo();
print('Municípios encontrados no GAUL:', nMuni);
if (nMuni !== 1) {
  // ajuda a corrigir a grafia: nomes de nível 2 do país que contêm o início do nome
  print('Nomes parecidos no GAUL:', ee.FeatureCollection('FAO/GAUL/2025/level2')
    .filter(ee.Filter.stringContains('GAUL2_NAME', NOME_MUNI.slice(0, 6)))
    .aggregate_array('GAUL2_NAME'));
  throw new Error('Esperado 1 município "' + NOME_MUNI + '" no FAO GAUL; encontrados: ' + nMuni);
}

// 2.2 - Área de busca: município ampliado
// -----------------------------------------------------------------------------

// buffer() com margem de erro de 100 m (segundo argumento) para acelerar o
// cálculo; a precisão do contorno não importa aqui
var busca = muni.geometry().buffer(AMPLIACAO_M, 100);

Map.centerObject(busca, 10);
Map.setOptions('SATELLITE');
Map.addLayer(ee.Image().paint(ee.FeatureCollection([ee.Feature(busca)]), 0, 2),
             {palette: ['ffff00']}, 'Área de busca');


// -----------------------------------------------------------------------------
// 3. - Centroides das edificações
// -----------------------------------------------------------------------------

// 3.1 - Filtragem espacial
// -----------------------------------------------------------------------------

// Todas as confianças (0,65 a 1,0) são exportadas; o limiar é aplicado no R
var poligonos = ee.FeatureCollection('GOOGLE/Research/open-buildings/v3/polygons')
  .filterBounds(busca);

print('Edificações (V3 Polygons) na área de busca:', poligonos.size());

// 3.2 - Tabela sem geometria
// -----------------------------------------------------------------------------

// A propriedade longitude_latitude é o centroide de cada contorno. A tabela
// guarda só longitude, latitude, área e confiança, o que deixa o arquivo
// muito menor que o GeoJSON dos contornos.
var centroides = poligonos.map(function (f) {
  var c = ee.Geometry(f.get('longitude_latitude')).coordinates();
  return ee.Feature(null, {
    lon:  c.get(0),
    lat:  c.get(1),
    area: f.get('area_in_meters'),
    conf: f.get('confidence')
  });
});


// -----------------------------------------------------------------------------
// 4. - Exportação para o Google Drive
// -----------------------------------------------------------------------------

// selectors fixa as colunas e a ordem; sem ele, o CSV traria system:index
Export.table.toDrive({
  collection:     centroides,
  description:    'ob_v3_centroides_municipio',
  folder:         PASTA_DRIVE,
  fileNamePrefix: 'ob_v3_centroides_municipio',
  fileFormat:     'CSV',
  selectors:      ['lon', 'lat', 'area', 'conf']
});

print('Exportação criada. Abra a aba Tasks e clique em RUN.');
