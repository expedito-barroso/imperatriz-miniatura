# ==============================================================================
# Imperatriz em miniatura (estilo tiny city): 04, servidor local da cena 3D
# Recorte: camadas de data/processed/cena/ (script 03) e página web/index.html
# Fonte  : arquivos locais do projeto; three.js 0.160.0 carregado pela página
#
# Autor : Antonio Expedito Ferreira Barroso de Carvalho
# Filia : UEMASUL — Universidade Estadual da Região Tocantina do Maranhão
# Data  : novembro/2026
# R     : 4.6.1
# Email : expebarroso@uemasul.edu.br
# ==============================================================================
#
# Objetivo deste script
#   Ligar um servidor web local na raiz do projeto e abrir a cena 3D no
#   navegador. O servidor é necessário porque a página lê os GeoJSON com
#   fetch(), o que os navegadores bloqueiam em arquivos abertos por duplo
#   clique (endereços file://).
#
# Entradas
#   web/index.html                         página da cena (three.js)
#   data/processed/cena/*.geojson          camadas geradas pelo script 03
#   data/processed/cena/metadados_cena.json
#
# Execução
#   Rodar o script inteiro (Ctrl+Shift+Enter no RStudio). O navegador padrão
#   abre em http://127.0.0.1:4321/web/index.html. O servidor roda em segundo
#   plano e o console do R continua livre. Para desligá-lo, rodar a seção 5.


# ------------------------------------------------------------------------------
# 1. - Pacotes
# ------------------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  servr,  # servidor web estático (httd) em segundo plano
  here    # caminhos relativos à raiz do projeto (.Rproj)
)


# ------------------------------------------------------------------------------
# 2. - Parâmetros
# ------------------------------------------------------------------------------

# Porta do servidor local. 4321 é o padrão do servr; se estiver ocupada por
# outro programa, trocar por outro número entre 1024 e 65535.
PORTA <- 4321

# Raiz servida: a pasta do projeto, de modo que a página (web/) alcance os
# dados (data/processed/cena/) pelo caminho relativo ../data/processed/cena/
RAIZ <- here::here()


# ------------------------------------------------------------------------------
# 3. - Conferência dos arquivos
# ------------------------------------------------------------------------------

# 3.1 - Página da cena
# ------------------------------------------------------------------------------

arq_pagina <- file.path(RAIZ, "web", "index.html")
if (!file.exists(arq_pagina)) {
  stop("Página não encontrada: ", arq_pagina,
       ". Coloque o index.html na pasta web/ da raiz do projeto.")
}

# 3.2 - Camadas da cena
# ------------------------------------------------------------------------------

# Obrigatórias: sem elas a página para com erro. Opcionais: a página segue sem
# elas (por exemplo, se não houver áreas verdes na AOI).
dir_cena <- file.path(RAIZ, "data", "processed", "cena")

obrigatorias <- c("aoi.geojson", "edificacoes.geojson", "vias.geojson")
opcionais    <- c("agua.geojson", "verde.geojson", "metadados_cena.json",
                  # piso vetorial; sem eixos.geojson, a página usa o chão em textura
                  "eixos.geojson", "pedestres.geojson", "quadras.geojson", "faixas.geojson",
                  # versão 1.1.0: vegetação pontual, estádio, riacho e bairros
                  "verde_extra.geojson", "arvores_pontos.geojson", "campo.geojson",
                  "riacho.geojson", "bairros.geojson", "bairros_pontos.geojson")

faltam <- obrigatorias[!file.exists(file.path(dir_cena, obrigatorias))]
if (length(faltam) > 0) {
  stop("Camadas obrigatórias ausentes em ", dir_cena, ": ",
       paste(faltam, collapse = ", "), ". Rode o script 03.")
}

ausentes_opc <- opcionais[!file.exists(file.path(dir_cena, opcionais))]
if (length(ausentes_opc) > 0) {
  message("Camadas opcionais ausentes (a cena abre sem elas): ",
          paste(ausentes_opc, collapse = ", "))
}

# Tamanho dos arquivos, para registro (o maior é o das edificações)
tamanhos <- file.size(file.path(dir_cena, c(obrigatorias, setdiff(opcionais, ausentes_opc))))
message("Camadas da cena: ", round(sum(tamanhos) / 1024^2, 1), " MB no total")


# ------------------------------------------------------------------------------
# 4. - Servidor e navegador
# ------------------------------------------------------------------------------

# 4.1 - Servidor em segundo plano
# ------------------------------------------------------------------------------

# Desliga servidores deixados por execuções anteriores nesta sessão, para
# liberar a porta
servr::daemon_stop()

# httd: servidor de arquivos estáticos. daemon = TRUE mantém o console livre;
# browser = FALSE porque o navegador é aberto na página certa no passo 4.2
# (o servr abriria a raiz do projeto)
servr::httd(
  dir     = RAIZ,
  port    = PORTA,
  host    = "127.0.0.1",
  browser = FALSE,
  daemon  = TRUE
)

# 4.2 - Abertura da página no navegador padrão
# ------------------------------------------------------------------------------

url_cena <- sprintf("http://127.0.0.1:%d/web/index.html", PORTA)
utils::browseURL(url_cena)

message("Cena aberta em: ", url_cena)
message("Para desligar o servidor, rode a seção 5 (servr::daemon_stop()).")


# ------------------------------------------------------------------------------
# 5. - Desligar o servidor
# ------------------------------------------------------------------------------

# Rodar apenas quando terminar de usar a cena (selecionar a linha e Ctrl+Enter).
# Fechar o RStudio também encerra o servidor.
# servr::daemon_stop()
