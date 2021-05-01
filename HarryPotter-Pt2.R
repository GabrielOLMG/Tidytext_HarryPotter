### Pacotes:
require(tidytext)
require(tidyverse)
require(harrypotter)
install.packages("widyr")
require("widyr")
install.packages("igraph")
require(igraph)
install.packages("ggraph")
require(ggraph)
install.packages("ggplot2")
#Cap4
### Passando Para Uma Tabela E Logo Em Seguida Vamos Separar As Palavras
  ##TAbela
cap=c("The Boy Who Lived",
      "The Vanishing Glass",
      "The Letters from No One",
      "The Keeper of Keys",
      "Diagon Alley",
      "The Journey from Platform Nine and Three Quarters",
      "The Sorting Hat",
      "The Potions Master",
      "The Midnight Duel",
      "Halloween",
      "Quidditch",
      "The Mirror of Erised",
      "Nicholas Flamel",
      "Norbert the Norwegian Ridgeback",
      "The Forbidden Forest",
      "Through the Trapdoor",
      "The Man with Two Faces")
PedraFilosofal = tibble(Capitulo = cap,
                        Texto = philosophers_stone,
                        Livro = "PedraFilosofal")

  ##Letras

  LetraPF = PedraFilosofal %>% 
    group_by(Capitulo) %>% 
    unnest_tokens(bigram,Texto, token = "ngrams",n=2) %>% # Fazendo uma analise de 2 em 2 palavras
    ungroup()

  LetraPF%>% #  Vendo Quantas Vezes um grupo de palavras juntas aparecem
    count(bigram,sort=TRUE)

  Separado = LetraPF %>%  # Separando as palavras juntas em colunas para poder filtrar
    group_by(Capitulo) %>% 
    separate(bigram,c("Palavra1","Palavra2"),sep=" ") %>% 
    ungroup()
  
  Separado_Filtrado = Separado %>% # tirando linhas onde possuem palavras "inuteis"
    filter(!Palavra1 %in% stop_words$word) %>% 
    filter(!Palavra2 %in% stop_words$word)
  
    # Exemplo Do que Fazer
      Separado_Filtrado %>% 
        filter(Palavra1 %in% c("voldemort"))
  
  Bigrama_Unico = Separado_Filtrado %>% # Unindo as palavras em uma unica coluna
    unite(bigram,Palavra1,Palavra2,sep = " ")

### Calculando e plontando tf_idf
  ## Calculando td_idf
    Bigram_tfidf = Bigrama_Unico %>% 
      count(Capitulo,bigram) %>% 
      bind_tf_idf(bigram,Capitulo,n) %>% 
      arrange(desc(tf_idf))
  ## Grafico
    Bigram_tfidf %>% 
      group_by(Capitulo) %>% 
      top_n(2) %>% 
      ggplot(aes(bigram,tf_idf, fill = Capitulo))+
      geom_col(show.legend = FALSE)+
      labs(x = NULL,y = "tf_idf")+
      facet_wrap(~Capitulo, ncol = 2, scale = "free")+
      coord_flip()

    
### Grafico pelo ggraph(igraph) (Assim da para ver a relação entre as palavras)
    BigramCont = LetraPF %>% 
      count(bigram,sort = TRUE)
    BigramCont = BigramCont %>% 
      separate(bigram,c("p1","p2"),sep = " ")
    BigramCont = BigramCont %>% 
      filter(!p1 %in% stop_words$word) %>% 
      filter(!p2 %in% stop_words$word)
    
    Bigram_graph = BigramCont %>% 
      filter(n>5) %>% 
      graph_from_data_frame()
    
    ggraph(Bigram_graph, layout = "fr")+
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label = name),vjust = 1, hjust = 1)
    