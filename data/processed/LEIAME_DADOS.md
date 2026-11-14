# Dados derivados: fontes e licenças

As camadas em `cena/` (coordenadas locais, em metros, relativas à origem UTM
gravada em `cena/metadados_cena.json`) e `aoi_imperatriz.gpkg` foram geradas
pelos scripts 01 e 03 a partir das fontes abaixo. Cada camada mantém a licença
da fonte de que deriva; a licença MIT do repositório cobre só o código.

| Camada | Fonte | Licença |
|---|---|---|
| `edificacoes.geojson` | Google Open Buildings V3 Polygons e 2.5D Temporal V1 (2023) | CC-BY 4.0 |
| `vias`, `eixos`, `pedestres`, `quadras`, `faixas`, `agua`, `verde` (marcações do OSM), `pistas`, `patios`, `rio` | © colaboradores do OpenStreetMap, extrato Geofabrik | ODbL 1.0 |
| `bairros`, `bairros_pontos` e nomes de vias em `eixos` (versão 1.1.0) | © colaboradores do OpenStreetMap, extrato Geofabrik | ODbL 1.0 |
| `verde_extra`, `arvores_pontos`, `campo`, `riacho` (versão 1.1.0) | derivadas das quadras e vias do OSM, dos contornos do Open Buildings e, nas praças, do OSM; posições por regra do script 03 (seção 8.7) e casos indicados pelo autor | ODbL 1.0 (base derivada do OSM) |
| `arvoredo.geojson` e manchas `dynamic_world_grama` de `verde.geojson` | Dynamic World V1, produzido para o Dynamic World Project pelo Google em parceria com a National Geographic Society e o World Resources Institute | CC-BY 4.0 |
| cota de base (`z_base` em `edificacoes`) | produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and © Airbus Defence and Space GmbH 2014-2018 provided under COPERNICUS by the European Union and ESA; all rights reserved | licença Copernicus WorldDEM-30 |
| `aoi_imperatriz.gpkg`, `aoi.geojson` | densidade das edificações do Open Buildings V3; limite municipal IBGE 2020, via geobr (Ipea) | CC-BY 4.0; dados públicos IBGE |

As camadas derivadas do OpenStreetMap são uma base derivada sob a ODbL 1.0:
quem as redistribuir ou adaptar deve manter a atribuição e oferecer a base
derivada sob a mesma licença (https://opendatacommons.org/licenses/odbl/).
