        #####Analise De Sentimentos Do Livro Harry Potter#####

### Pacotes:
install.packages("tidyverse") # Pacote Base Manipulação De Dados
require(tidyverse)
install.packages("tidytext") # Pacote Base Mineração De Texto
require(tidytext)
devtools::install_github("bradleyboehmke/harrypotter") # Pacote Do Github coom Harry Potter
require(harrypotter)
                              #Cap1
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
    group_by(Capitulo,Livro) %>% 
    unnest_tokens(Letra,Texto)

### Tirando Palavras "desnecessarias"
data("stop_words")
SemSWPF = LetraPF %>% 
  anti_join(stop_words,by=c("Letra"="word"))

### Palavras Que Se Repetem Por Capitulo
Contando=SemSWPF %>% 
  group_by(Capitulo,Livro) %>% 
  count(Letra,sort=TRUE)

### Grafico das Palavras Que repetem mais de 100 vezes
Contando %>% 
  filter(n>50,n<100) %>% 
  mutate(Letra = reorder(Letra,n)) %>% 
  ggplot(aes(Letra,n)) +
  geom_col(fill="red") + 
  xlab("Palavras")+
  ylab("Quantidades")+
  coord_flip()
                              #cap 2
### Sentimentos (afinn,bing,nrc)
  get_sentiments("afinn") # Da Uma Pontuação Entre -5 e 5 para cada Palavra
  get_sentiments("nrc") # Classifica como positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
  get_sentiments("bing")  # Classifica Como Positivo Ou Negativo
  
    # Palavras Q TEm Como Sentimento Medo(nrc)
      PFear = get_sentiments("nrc") %>% 
        filter(sentiment == "fear")
      Medo = SemSWPF %>% 
        inner_join(PFear,by = c("Letra" = "word")) %>% 
        count(Letra,sort = TRUE)

    # Palavras Com Pontuação Menores Que -3(afinn)
      PM3 = get_sentiments("afinn") %>% 
        filter(score < -3)
      M3 = SemSWPF %>% 
        inner_join(PM3,by = c("Letra" = "word")) %>% 
        count(Letra,sort = TRUE)

    # Palavras Negativo (bing)
      PP = get_sentiments("bing") %>% 
        filter(sentiment == "negative")
      P = SemSWPF %>% 
        inner_join(PP,by = c("Letra" = "word")) %>% 
        count(Letra,sort = TRUE)

### Grafico De Sentimentos
  ## Tabela
     TabelaSentimentosCap = SemSWPF %>% 
       inner_join(get_sentiments("bing"),by = c("Letra" = "word")) %>% 
       count(Livro,sentiment) %>% #Esta Separando Por Livo e contando quantos Pos e Neg
       spread(sentiment,n,fill=0) %>%  # Separando Em positivo e Negativo  por Capitulo
       mutate(Sentimento = positive-negative) # Cria um Coluna Sentimento q é Calculado atraves do pos-neg
  ## Grafico
     TabelaSentimentosCap %>% 
       ggplot(aes(Capitulo,Sentimento)) +
       geom_bar(stat = "identity", aes(fill = Livro )) +
       theme_classic() + 
       theme(axis.text.x = element_text(angle = 90))+
       coord_flip() + 
       ylim(-250, 250) 
       
                            #Cap3
### Preparação para Ver a Frequencia
     Cont = LetraPF %>%
       count(Letra,sort=TRUE) 
     Total = Cont %>% 
       summarise(total = sum(n))
     Total = left_join(Cont, Total)
  ## GRafico Com frequencia de palavras por capitulo 
     ggplot(Total, aes(n/total, fill=Capitulo))+
       geom_histogram(show.legend = FALSE)+
       xlim(NA,0.009)+
       facet_wrap(~Capitulo, ncol=2, scale="free_y") # o facet_wrap permite a separação em varios graficos...nessa caso em graficos por capitulo
  ## Grafico com eixos com base log!0(permite uma melhor visualizaçao de dados grandes)   
     RankFreq = Total %>% 
       group_by(Capitulo) %>% 
       mutate(rank = row_number(), freq = n/total)
     RankFreq %>% 
       ggplot(aes(rank,freq, color = Capitulo))+
       geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE)+
       scale_x_log10()+
       scale_y_log10()
  ## Criando "zipf's law"(Quanto maior a freq, menor o rank da palavra)
     RankP = RankFreq %>% 
       filter(rank<500,rank>10)
     lm(log10(freq)~log10(rank), data = RankP) 
     RankFreq %>% 
      ggplot(aes(rank,freq, color=Capitulo))+
      geom_abline(intercept = -0.8697, slope = -0.9560, color="gray50",linetype = 1)+
      geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE)+
      scale_x_log10()+
      scale_y_log10()
        