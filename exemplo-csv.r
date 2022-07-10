# Instala os pacotes necessários
# this.path: here()
pckg=c("this.path")

for(i in 1:length(pckg)) {
  print(pckg[i])
  if (!is.element(pckg[i], installed.packages()[,1]))
    install.packages(pckg[i], dep = TRUE)
  require(pckg[i], character.only = TRUE)
}

# Função para concatenar strings
concat <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# Função para carregar os dados, convertendo .CSV em data.frame
# carrega os arquivos cujo nome segue o formato "^<tipo_dado>_<evento>[YYYY].csv$"
# gerando uma lista: resultado$<evento>[YYYY] = data.frame()
carregar_dados = function(path, tipo_dado, evento) {
  result = list()
  substr_start = nchar(concat(tipo_dado, "_"))
  substr_stop = substr_start + nchar(concat(evento, "YYYY"))
  dados_padrao = concat("^", tipo_dado, "_", evento, "[0-9]{4}.csv$")
  for (arquivo in dir(data_dir, pattern = dados_padrao, full.names = TRUE, ignore.case = TRUE)) {
    edicao = substr(
      basename(arquivo),
      substr_start + 1,
      substr_stop
    )
    df_edicao = read.csv(
      file = arquivo,
      sep = ";",
      header=TRUE,
      stringsAsFactors=FALSE,
      #row.names = 1                # Descomentar se quiser usar o nome do artigo como nome da linha
    )
    result[[edicao]] = df_edicao
  }
  return(result)
}

# Diretório contendo os arquivos CSV
data_dir = here("dados")

termos = carregar_dados(data_dir, "termos", "WSCAD")
head(termos$WSCAD2018)

metricas = carregar_dados(data_dir, "metricas", "WSCAD")
head(metricas$WSCAD2018)

testes = carregar_dados(data_dir, "testes", "WSCAD")
head(testes$WSCAD2018)
