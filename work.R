setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
codigos = read.csv("C:\\Users\\belokurowsr\\OneDrive - Kantar\\Desktop\\Kantar\\Puntos Sondeo\\codigos_postais.csv",fileEncoding = "UTF8")

codigos %>% filter(desig_postal == "PORTO") %>% head(10) %>% View()


codigos %>% filter(desig_postal == "PORTO") %>% count(nome_arteria)

if(!require("devtools")) install.packages("devtools")
devtools::install_github("meirelesff/genderBR")
library(genderBR)

install.packages("tidywikidatar")
library(tidywikidatar)
get_gender("maria")
get_gender("Cantor Zeca Afonso")
map_gender("ivani") %>% View()
get_gender("ivani",  state = "SP")

codigos %>% filter(desig_postal == "PORTO") %>% count(nome_arteria) %>%
  mutate(genero =get_gender(nome_arteria)) %>%
  View()
dfteste = codigos %>% filter(desig_postal == "PORTO") %>% count(nome_arteria) %>%
  mutate(genero =get_gender(nome_arteria)) %>% filter(is.na(genero)) %>%
  slice(69:89)

# dfteste %>%
#   mutate(sexoWiki = tw_get_label(tw_get_property(tw_search(search = nome_arteria,limit = 1)$id,p="P21")$value))
library("tidywikidatar")
tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)
library(tictoc)
tic()
df2 = codigos %>% filter(desig_postal == "PORTO") %>% count(nome_arteria) %>%
  mutate(sexoCensoBR =get_gender(nome_arteria))%>%
  mutate(temnumero = parse_number(nome_arteria)) %>%
  mutate(sexoWiki = case_when(is.na(temnumero)~tw_get_label(tw_get_property(tw_search(search = nome_arteria,limit = 1)$id,p="P21")$value),
                              TRUE~NA_character_))
toc()

#2270.11 sec elapsed
df2 %>% filter(is.na(sexoCensoBR) & !is.na(sexoWiki ))
df2  %>% filter(tolower(sexoCensoBR) != tolower(sexoWiki ))
df2 %>% filter(is.na(sexoCensoBR) & is.na(sexoWiki)) %>% clipr::write_clip()
modificar = c("Aires Borges",
              "Aires de Gouveia Osório",
              "Almeida Valente",
              "Barbosa de Castro",
              "Barbosa Du Bocage",
              "Barros Lima",
              "Burnay",
              "Câmara Sinval",
              "Carrilho Videira",
              "Cantor Zeca Afonso",
              "Carrington da Costa"
              "Carvalho Barbosa",
              "Coelho Lousada",
              "Coelho Neto",
              "Correia de Sá",
              "Correia Pinto",
              "Costa Barreto",
              "Costa e Almeida",
              "Costa Mendes",
              "Deniz Jacinto",
              "Ferreira Borges",
              "Ferreira Cardoso",
              "Ferreira dos Santos",
              "Ferreira Lapa",
              "Forrester",
              "Historiador Robert Smith",
              "Melo Adrião",
              "Melo Leote",
              "Moreira de Assunção",
              "Panorâmica Edgar Cardoso",
              "Particular Borges e Irmão",
              "Particular de Santo Isidro",
              "Particular Justino Teixeira",
              "Particular Maria Albertina",
              "Particular Meneses Russel",
              "Particular Novais da Cunha",
              "Pereira de Novais",
              "Pereira Reis",
              "Pirmin Treku",
              "Rebelo da Costa",
              "Ribeiro de Sousa",
              "Rocha Peixoto",
              "Rocha Pereira",
              "Sá da Bandeira",
              "Santos Pousada",
              "São Cosme",
              "Saraiva de Carvalho",
              "Serpa Pinto",
              "Silva Monteiro",
              "Silva Porto",
              "Silva Ramos",
              "Silva Tapada",
              "Soares Correia",
              "Soares dos Reis",
              "Sousa Ávides",
              "Sousa Júnior",
              "Sousa Rosa",
              "Teixeira de Vasconcelos")
df3 = df2 %>% mutate(sexoWiki = case_when(str_detect(nome_arteria  ,paste(modificar, collapse = "|"))~"male",
                                    TRUE~sexoWiki))
#total = 1768
df3 %>% filter(is.na(sexoCensoBR) & !is.na(sexoWiki )) %>% mutate(sexo=sexoWiki)#144
df3 %>% filter(!is.na(sexoCensoBR) & is.na(sexoWiki )) %>% mutate(sexo=sexoCensoBR)#480
df3 %>% filter(tolower(sexoCensoBR) == sexoWiki) %>% mutate(sexo=sexoCensoBR)#423
df3 %>% filter(tolower(sexoCensoBR) != sexoWiki) %>% mutate(sexo=sexoWiki)#5
df3 %>% filter(is.na(sexoCensoBR) & is.na(sexoWiki )) %>% mutate(sexo=NA)#716

ruascomSexo = bind_rows(df3 %>% filter(is.na(sexoCensoBR) & !is.na(sexoWiki )) %>% mutate(sexo=sexoWiki),#144
df3 %>% filter(!is.na(sexoCensoBR) & is.na(sexoWiki )) %>% mutate(sexo=sexoCensoBR),#480
df3 %>% filter(tolower(sexoCensoBR) == sexoWiki) %>% mutate(sexo=sexoCensoBR),#423
df3 %>% filter(tolower(sexoCensoBR) != sexoWiki) %>% mutate(sexo=sexoWiki),
df3 %>% filter(is.na(sexoCensoBR) & is.na(sexoWiki )) %>% mutate(sexo=NA)
) %>% #5
mutate(sexo=str_to_title(sexo))

saveRDS(df2,"codigos.rds")
saveRDS(ruascomSexo,"20221202 1052 ruas.rds")
