library(devtools)

# Caso nao tenha o devtools (Rtools)
# No Windows -> Baixe e instale a versão .exe do Rtools no link http://cran.r-project.org/bin/windows/Rtools/.
# Configurar nas variáveis de ambiente, o PATH para o diretório da instalação, Ex: C:/Rtools/
install.packages("pkgbuild") # pkgbuild is not available (for R version 3.5.0)
install.packages("devtools") # make sure you have the latest version from CRAN
library(devtools) # load package
devtools::install_github("r-lib/pkgbuild") # install updated version of pkgbuild from GitHub
library(pkgbuild) # load package
find_rtools() # should be TRUE, assuming you have Rtools 3.5
# No MAC OS -> Baixe e instale o Xcode command line tools
# No Linux ->  Instalar o R development package, chamado de r-devel ou r-base-dev

if(!is.element("devtools", installed.packages()[, 1]))
  install.packages("devtools", repos = 'http://cran.us.r-project.org')
require(devtools)
if(!is.element("RCurl", installed.packages()[, 1]))
  install.packages("RCurl", repos = 'http://cran.us.r-project.org')
if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
if(!is.element("rtweet", installed.packages()[,1]))
  install.packages("rtweet", repos = 'http://cran.us.r-project.org')


library(rtweet)
library(tidyverse) 
library(tidytext)
library(magrittr)  
library(lubridate)
library(stringr) 
library(ggExtra)
library(ggplot2)
library(wordcloud)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(widyr)
library(igraph)
library(ggraph)

require(sentiment)
ls("package:sentiment")

library(lexiconPT)
ls("package:lexiconPT")

# Obter token e chave do twitter no site apps.twitter.com / http://dev.twitter.com
# https://www.oficinadanet.com.br/artigo/php/como_obter_as_chaves_de_acesso_para_utilizar_a_api_do_twitter
api_key             <- ""
api_secret          <- ""
access_token        <- ""
access_token_secret <- ""
twitter_app         <- "Twitter Sentiment Analysis."

# Accedemos a Twitter a través de los datos del token
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

tweets_alvaro    <- search_tweets("Alvaro Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_daciolo   <- search_tweets("Daciolo Presidente"  , n = 1000, include_rts = FALSE, lang="pt")
tweets_ciro      <- search_tweets("Ciro Presidente"     , n = 1000, include_rts = FALSE, lang="pt")
tweets_eymael    <- search_tweets("Eymael Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_haddad    <- search_tweets("Haddad Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_alckmin   <- search_tweets("Alckmin Presidente"  , n = 1000, include_rts = FALSE, lang="pt")
tweets_boulos    <- search_tweets("Boulos Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_meirelles <- search_tweets("Meirelles Presidente", n = 1000, include_rts = FALSE, lang="pt")
tweets_bolsonaro <- search_tweets("Bolsonaro Presidente", n = 1000, include_rts = FALSE, lang="pt")
tweets_amoedo    <- search_tweets("Amoedo Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_marina    <- search_tweets("Marina Presidente"   , n = 1000, include_rts = FALSE, lang="pt")
tweets_vera      <- search_tweets("Vera Presidente"     , n = 1000, include_rts = FALSE, lang="pt")
tweets_goulart   <- search_tweets("Goulart Presidente"  , n = 1000, include_rts = FALSE, lang="pt")

# Função para limpeza dos tweets
f_clean_tweets <- function (tweets) {
  
  clean_tweets <- tweets$text
  # remove retweet 
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', ' ', clean_tweets)
  # remove nomes pessoas
  clean_tweets = gsub('@\\w+', ' ', clean_tweets)
  # remove pontuação
  clean_tweets = gsub('[[:punct:]]', ' ', clean_tweets)
  # remove números
  clean_tweets = gsub('[[:digit:]]', ' ', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', ' ', clean_tweets)
  # remove espaços desnecessários
  clean_tweets = gsub('[ \t]{2,}', ' ', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', ' ', clean_tweets)
  # remove emojis e caracteres especiais
  clean_tweets = gsub('<.*>', ' ', enc2native(clean_tweets))
  # remove quebra de linha
  clean_tweets = gsub('\\n', ' ', clean_tweets)
  # remove espaços desnecessários
  clean_tweets = gsub('[ \t]{2,}', ' ', clean_tweets)
  # coloca tudo em minúsculo
  clean_tweets = tolower(clean_tweets)
  
  # remove tweets duplicados
  tweets$text <- clean_tweets
  tweets <- tweets[!duplicated(tweets$text),]
  tweets
}

tweets_alvaro    <- f_clean_tweets(tweets_alvaro)
tweets_daciolo   <- f_clean_tweets(tweets_daciolo)
tweets_ciro      <- f_clean_tweets(tweets_ciro)
tweets_eymael    <- f_clean_tweets(tweets_eymael)
tweets_haddad    <- f_clean_tweets(tweets_haddad)
tweets_alckmin   <- f_clean_tweets(tweets_alckmin)
tweets_boulos    <- f_clean_tweets(tweets_boulos)
tweets_meirelles <- f_clean_tweets(tweets_meirelles)
tweets_bolsonaro <- f_clean_tweets(tweets_bolsonaro)
tweets_amoedo    <- f_clean_tweets(tweets_amoedo)
tweets_marina    <- f_clean_tweets(tweets_marina)
tweets_vera      <- f_clean_tweets(tweets_vera)
tweets_goulart   <- f_clean_tweets(tweets_goulart)

nt <-  c(length(tweets_alvaro$text),
         length(tweets_daciolo$text),
         length(tweets_ciro$text),
         length(tweets_eymael$text),
         length(tweets_haddad$text),
         length(tweets_alckmin$text),
         length(tweets_boulos$text),
         length(tweets_meirelles$text),
         length(tweets_bolsonaro$text),
         length(tweets_amoedo$text),
         length(tweets_marina$text),
         length(tweets_vera$text),
         length(tweets_goulart$text))

nc <- c('Alvaro', 'Daciolo', 'Ciro', 'Eymael', 'Haddad', 'Alckmin', 'Boulos', 'Meirelles', 'Bolsonaro', 'Amoedo', 'Marina', 'Vera', 'Goulart')

names(nt) <- nc

nt

pct <- rep(0,13)

names(pct) <-  nc

cria_pct <- function(pct, nt) {
  for (i in 1:13) {
    
    pct[i] <- nt[i]/sum(nt)
  }
  return (pct)
}

pct <- cria_pct(pct, nt)

paleta <- brewer.pal(7, "Blues")
barplot(pct, names.arg = nc, col = paleta, main = "Percentual de Menções no Tweets por Candidato", xlab = "Candidatos a Presidente", ylab = "Percentual de Tweets", cex.names = 0.8)

# Juntando todos os tweets em um único dataframe
df_tweets <- rbind (tweets_alvaro,
                    tweets_daciolo,
                    tweets_ciro,
                    tweets_eymael,
                    tweets_haddad,
                    tweets_alckmin,
                    tweets_boulos,
                    tweets_meirelles,
                    tweets_bolsonaro,
                    tweets_amoedo,
                    tweets_marina,
                    tweets_vera,
                    tweets_goulart)

# Obtendo o texto dos tweets
tweets_candidatos <- df_tweets$text

# 1) Pacote Sentiment

# Utilizando o pacote sentiment para classificar as emoções
emotions <- classify_emotion(tweets_candidatos, algorithm='bayes', prior=1.0)

# Utilizando o pacote sentiment para classificar as polaridades 
polarities = classify_polarity(tweets_candidatos, algorithm='bayes')

df = data.frame(text=tweets_candidatos, emotion=emotions[,'BEST_FIT'],
                polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."

# Plotando a polaridade dos tweets
plot_ly(df, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Count'), title="Análise de Sentimentos: Polaridade")

# Plotando as emoções dos tweets
tweet_pie <- ggplot(as.data.frame(emotions), aes(x = factor(1), fill = factor(BEST_FIT)))  + geom_bar(width = 1)
tweet_pie + coord_polar(theta = "y") + 
  ggtitle("Análide de Sentimentos", subtitle = "Candidatos a Presidente da República") + 
  ylab("Y") + xlab("X") + scale_fill_brewer(palette = "RdYlGn") + 
  theme(plot.title = element_text(size=12, face='bold'))

# Visualiza os tweets  por polaridade
df <- df %>%
  group_by(polarity) %>%
  summarise(pasted=paste(text, collapse=" "))

# Removendo stopwords  
df$pasted = removeWords(df$pasted, stopwords(kind='pt'))

# Criando o corpus
corpus = Corpus(VectorSource(df$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = df$polarity

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(3, 'Dark2'),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)


# 2) Pacote LexiconPT

# carregar datasets
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

str(op30)
table(op30$type)
table(op30$polarity)
table(op30$polarity_revision)

# Criar id unico para cada tweet
df_tweets %<>% mutate(tweet_id = row_number())

# Criar uma linha para cada palavra de um tweet
df_tweets_words <- df_tweets %>% unnest_tokens(term, text)

# Listando palavras dos primeiros tweets
df_tweets_words %>%
  select(tweet_id, term) %>%
  head(20)

# Verificando correlações entre palavras
correlacao <- df_tweets_words %>%
  group_by(term) %>%
  filter(n() > 20) %>%
  pairwise_cor(term, tweet_id, sort = TRUE)
correlacao

correlacao %>%
    filter(correlation > .50) %>%
    graph_from_data_frame() %>%
    ggraph(layout = 'fr') + 
    guides(edge_alpha = "none", edge_width = "none") +
    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
    geom_node_point(color = 'lightblue', size = 5) + 
    geom_node_text(aes(label = name), repel = TRUE) + 
    theme_graph() +
    labs(title = "Correlação de Palavras no Twitter")

# Verificando a polaridade das palavras
df_tweets_words %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(tweet_id, term, polarity, lex_polarity) %>% 
  head(20)

df_tweets_words <- df_tweets_words %>% 
  inner_join(op30, by = "term") %>% 
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(tweet_id) %>% 
  summarise(
    tweet_sentiment_op = sum(polarity),
    tweet_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(tweet_sentiment_lex, tweet_sentiment_op),
    most_pos = max(tweet_sentiment_lex, tweet_sentiment_op)
  )

head(df_tweets_words)

# Gerando um gráfico de polaridade entre os dois léxicos
df_tweets_words %>% 
  ggplot(aes(x = tweet_sentiment_op, y = tweet_sentiment_lex)) +
  geom_point(aes(color = n_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")


df_tweets_words %<>% filter(between(tweet_sentiment_op, -10, 10))

# comentario mais positivo e negativo dos tweets
most_pos <- which.max(df_tweets_words$most_pos)
most_neg <- which.min(df_tweets_words$most_neg)

# mais positivo
cat(df_tweets$text[df_tweets$tweet_id == df_tweets_words$tweet_id[most_pos]])

# mais negativo
cat(df_tweets$text[df_tweets$tweet_id == df_tweets_words$tweet_id[most_neg]])

# utilizando a análise de sentimento do Op Lexico
df_tweets %<>% inner_join(
  df_tweets_words %>% select(tweet_id, sentiment = tweet_sentiment_op),
  by = "tweet_id"
)

# criar coluna de data (variavel da classe Date)
df_tweets$data <- as.Date(df_tweets$created_at)

df_tweets_wide <- df_tweets %>% 
  # filtrar fora palavras neutras
  filter(sentiment != 0) %>% 
  # converter numerico para categorico
  mutate(sentiment = ifelse(sentiment < 0, "negativo", "positivo")) %>% 
  # agrupar os dados
  count(data, substr(text,1,40), sentiment) %>% 
  # converter para formato wide
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(data)

head(df_tweets_wide,50) %>% knitr::kable()

# Classificando os tweets para verificar o mais positivo e o mais negativo
df_tweets_wide %>% 
  arrange(sentimento) %>% 
  filter(row_number() == 1 | row_number() == nrow(df_tweets_wide)) %>% 
  knitr::kable()

# Evolução do sentimento dos tweets ao longo do tempo
df_tweets_wide %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(x = index, y = sentimento)) +
  geom_col(aes(fill = sentimento)) +
  scale_y_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  labs(x = "Índice da publicação", y = "Sentimento",
       fill = NULL, title = "Evolução do sentimento nos tweets dos Candidatos")

# Qual o candidato com menções mais positivas e qual o candidato com menções mais negativas no Twitter?
df_tweets %>% 
  mutate(
    alvaro    = str_detect(str_to_lower(text), "alvaro"),
    daciolo   = str_detect(str_to_lower(text), "daciolo"),
    ciro      = str_detect(str_to_lower(text), "ciro"),
    eymael    = str_detect(str_to_lower(text), "eymael"),
    haddad    = str_detect(str_to_lower(text), "haddad"),
    alckmin   = str_detect(str_to_lower(text), "alckmin"),
    boulos    = str_detect(str_to_lower(text), "boulos"),
    meirelles = str_detect(str_to_lower(text), "meirelles"),
    bolsonaro = str_detect(str_to_lower(text), "bolsonaro"),
    amoedo    = str_detect(str_to_lower(text), "amoedo"),
    marina    = str_detect(str_to_lower(text), "marina"),
    vera      = str_detect(str_to_lower(text), "vera"),
    goulart   = str_detect(str_to_lower(text), "goulart")
  ) %>% 
  gather(termo, eh_presente, alvaro:goulart) %>% 
  filter(eh_presente) %>% 
  group_by(termo) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = reorder(termo, -sentiment), y = sentiment)) + 
  geom_col(fill = "#4292C6") + 
  labs(x = "Candidatos a Presidente", y = "Sentimentos dos Tweets",
       fill = NULL, title = "Análise de Sentimentos dos Tweets com Menção de cada Candidato a Presidente")



