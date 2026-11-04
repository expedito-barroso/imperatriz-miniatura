# Imperatriz em miniatura

Maquete digital isométrica, no estilo *tiny city*, do núcleo urbano de Imperatriz (Maranhão, Brasil): 136.824 edificações extrudadas até a altura estimada para 2023, sobre o piso vetorial das vias, quadras, calçadas, áreas verdes, água e aeroporto, com relógio do dia, árvores ao vento e veículos em movimento.

**Ver a cena:** https://expedito-barroso.github.io/imperatriz-miniatura/

*An isometric "tiny city" model of Imperatriz (Maranhão, Brazil), built from Google Open Buildings 2.5D heights, OpenStreetMap and Dynamic World, processed in R and rendered with three.js. Text in Portuguese.*

Versão 1.0.0 · novembro de 2026

---

## O que a cena mostra

- **Edificações:** contornos do Google Open Buildings V3 e alturas do Open Buildings 2.5D Temporal (2023), discretizadas em pavimentos; três classes visuais (casa, galpão, vertical) definidas por altura e área.
- **Piso vetorial:** eixos das vias de carro com largura por classe, calçadas e quadras derivadas, marcações, faixas de pedestre, caminhos, ferrovia e aeroporto, a partir do OpenStreetMap.
- **Vegetação:** árvores onde a classificação Dynamic World (2023) indica arvoredo com pelo menos 0,25 ha; gramados nas demais manchas verdes.
- **Animações ilustrativas:** relógio do dia (luz, céu e sombras das árvores), balanço das copas, carros, caminhões na BR-010, barcos no Tocantins e um avião na pista.
- **Interação:** girar, arrastar e aproximar; duplo clique numa edificação mostra classe, pavimentos, coordenadas e links para o Google Maps e o Street View; filtro opcional de pixel art; tecla H esconde a interface para gravação de vídeo.

## Ressalvas

- Alturas e classes são estimativas de modelos aplicados a imagens de satélite, sem levantamento de campo. Na rodada de 03/11/2026, 14,5% das edificações receberam altura imputada (1 pavimento) por falta de estimativa.
- O OpenStreetMap não permite distinguir com segurança asfalto, laterita e bloquete em Imperatriz: cerca de 80% da extensão das vias de carro não tem registro de pavimento e aparece como pavimentada por convenção. Calçadas, larguras e marcações seguem convenção por classe de via.
- A AOI segue as edificações do Open Buildings V3; construções posteriores às imagens do modelo ficam fora (por exemplo, as fileiras do sudoeste do Conjunto Canto da Serra).
- Árvores, veículos, vento e avião são ilustrativos: não correspondem a inventário arbóreo, contagem de tráfego nem voos reais.
- A ressalva de cada execução, com os números daquela rodada, fica gravada em `data/processed/cena/metadados_cena.json` e aparece no painel *Fontes* da página.

## Estrutura

```
Imperatriz_miniatura.Rproj
00_gee_busca_edificacoes.js        GEE: centroides das edificações do município
01_aoi_imperatriz.R                R: AOI por densidade de edificações
02_gee_open_buildings_imperatriz.js GEE: contornos, alturas, relevo e Dynamic World pela AOI
03_integracao_cena_imperatriz.R    R: integração das camadas da cena
04_servidor_cena.R                 R: servidor local e abertura da página
diag_osm_piso.R                    R: diagnóstico das etiquetas do OSM
diag_osm_pavimento.R               R: diagnóstico do pavimento no OSM
web/index.html                     página da cena (three.js 0.160.0)
web/brasao_uemasul.png             brasão da UEMASUL na fachada
data/processed/cena/               camadas GeoJSON e metadados lidos pela página
data/processed/aoi_imperatriz.gpkg AOI em SIRGAS 2000 / UTM 23S
data/processed/LEIAME_DADOS.md     fontes e licenças dos dados derivados
output/                            figuras e tabelas de diagnóstico do script 03
```

`data/raw/` (exportações do GEE e extrato do OpenStreetMap) não está no repositório; os scripts e o tutorial descrevem como obtê-la.

## Como reproduzir

1. **GEE:** rodar `00_gee_busca_edificacoes.js` no Code Editor e baixar `ob_v3_centroides_municipio.csv` do Google Drive para `data/raw/`.
2. **OpenStreetMap:** baixar o extrato `nordeste-latest.osm.pbf` do Geofabrik para `data/raw/osm/`.
3. **R:** abrir `Imperatriz_miniatura.Rproj` no RStudio e rodar `01_aoi_imperatriz.R`; conferir `data/processed/aoi_inspecao.gpkg` no QGIS.
4. **GEE:** subir `data/processed/aoi_imperatriz_gee.zip` como asset `imperatriz_aoi`, rodar `02_gee_open_buildings_imperatriz.js` e baixar os quatro arquivos para `data/raw/`.
5. **R:** rodar `03_integracao_cena_imperatriz.R` e depois `04_servidor_cena.R`, que abre a página em `http://127.0.0.1:4321/web/index.html`.

Programas: R 4.6.1 com sf, terra, exactextractr, osmextract, geobr, dplyr, here, jsonlite, ggplot2, servr e zip; Google Earth Engine (projeto de uso não comercial); navegador com WebGL. O passo a passo detalhado, com as decisões e os números de cada etapa, está no tutorial do projeto.

## Dados e licenças

O código está sob a licença MIT (`LICENSE`). Os dados derivados mantêm as licenças das fontes:

- Edificações: Google Open Buildings V3 e 2.5D Temporal (CC-BY 4.0).
- Vias, água, verde, ferrovia, aeroporto e rio: © colaboradores do OpenStreetMap (ODbL 1.0).
- Árvores e gramados: Dynamic World V1, produzido para o Dynamic World Project pelo Google em parceria com a National Geographic Society e o World Resources Institute (CC-BY 4.0).
- Relevo: produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and © Airbus Defence and Space GmbH 2014-2018 provided under COPERNICUS by the European Union and ESA; all rights reserved.
- Limite municipal: IBGE, malha municipal 2020, via geobr (Ipea).
- Brasão: UEMASUL, símbolo institucional, fora da licença MIT.

Detalhes por camada em `data/processed/LEIAME_DADOS.md`.

## Referências

- Sirko, W. et al. *Continental-scale building detection from high resolution satellite imagery*. arXiv:2107.12283, 2021.
- Sirko, W. et al. *High-Resolution Building and Road Detection from Sentinel-2*. arXiv:2310.11622, 2023.
- Brown, C.F. et al. Dynamic World, Near real-time global 10 m land use land cover mapping. *Scientific Data* 9, 251, 2022. https://doi.org/10.1038/s41597-022-01307-4

## Inspiração

Conceito *tiny[city]*, como isometric.nyc (Andy Coenen), tiny.london (Boris Starkov), tinygyn.com e tinyrj.com (Thomas M) e tiny.voltaredonda (Caio Campos).

## Como citar

Use o arquivo `CITATION.cff` (botão *Cite this repository* do GitHub). O DOI do Zenodo será acrescentado após o depósito da versão 1.0.0.

## Autoria

© 2026 Expedito Barroso (Antonio Expedito Ferreira Barroso de Carvalho) · UEMASUL · expebarroso@uemasul.edu.br

Desenvolvido com assistência Anthropic - Claude LLM Opus 5.5.
