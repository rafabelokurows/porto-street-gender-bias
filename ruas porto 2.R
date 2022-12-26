library(stringr)
library(tidywikidatar)
library(tidyverse)
library(osmdata)
#if(!require("devtools")) install.packages("devtools")
#devtools::install_github("meirelesff/genderBR")
library(sf)
library(genderBR)
library(extrafont)
# install.packages("emojifont")
# library(emojifont)
# load.fontawesome()
remotes::install_github("hrbrmstr/waffle")
library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(waffle)
library(leaflet)
tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

#https://www.listendata.com/2019/06/create-infographics-with-r.html
#https://stackoverflow.com/questions/51508415/waffle-package-on-r-icon/74589250#74589250


# city_coords <- matrix(data = c(-8.71902,41.10626,-8.54393,41.20758),
#                       nrow = 2, ncol = 2)

city_coords <- matrix(data = c(-8.698254,41.136520,-8.565388,41.189377),
                      nrow = 2, ncol = 2)

limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

streets <- opq(bbox = limits)  %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
gardens <- opq("Porto Portugal")  %>%
  add_osm_feature(key = "leisure",value = c("park","garden")) %>%
  osmdata_sf()
squares <- opq("Porto Portugal")  %>%
  add_osm_feature(key = "place",value = c("square")) %>%
  osmdata_sf()
#available_features()
# osmdata::available_tags("highway")
unique(squares$osm_polygons$name) %>%   sort() %>% as.data.frame() %>%
  rename(rua=1) %>%
  mutate(rua=iconv(rua,from="UTF-8", to="LATIN1")) %>%
  mutate(nome = case_when(str_starts(rua,"Praceta |Jardins |Alameda ")~str_sub(rua,9),
                          str_starts(rua,"Jardim |Parque |Quinta ")~str_sub(rua,8),
                          str_starts(rua,"Pra?a |Largo |Fonte ")~str_sub(rua,7)),
         clean=case_when(str_starts(nome,"^de\\s|^ da\\s|^da\\s|^do\\s")~str_sub(nome,4),
                         str_starts(nome,"^das\\s|^dos\\s")~str_sub(nome,5),
                         TRUE~nome)) -> pracas

unique(gardens$osm_polygons$name) %>%   sort() %>% as.data.frame() %>%
  rename(rua=1) %>%
  mutate(rua=iconv(rua,from="UTF-8", to="LATIN1")) %>%
  mutate(nome = case_when(str_starts(rua,"Praceta |Jardins |Alameda ")~str_sub(rua,9),
                          str_starts(rua,"Jardim |Parque |Quinta ")~str_sub(rua,8),
                          str_starts(rua,"Pra?a |Largo |Fonte ")~str_sub(rua,7)),
         clean=case_when(str_starts(nome,"^de\\s|^ da\\s|^da\\s|^do\\s")~str_sub(nome,4),
                         str_starts(nome,"^das\\s|^dos\\s")~str_sub(nome,5),
                         TRUE~nome)) -> parques

unique(streets$osm_lines$name) %>% sort() %>% as.data.frame() %>%
  rename(rua=1) %>%
  mutate(nome = case_when(str_starts(rua,"Nó ")~str_sub(rua,4),
                          str_starts(rua,"Rua |Via ")~str_sub(rua,5),
                          str_starts(rua,"Muro |Cais |Beco |Vila ")~str_sub(rua,6),
                          str_starts(rua,"Pátio |Campo |Praça |Largo |Viela |Rampa |Túnel |Ponte ")~str_sub(rua,7),
                          str_starts(rua,"Jardim |Rossio |Escada |Parque |Bairro |Quinta |Vereda ")~str_sub(rua,8),
                          str_starts(rua,"Ribeira |Recanto |Estrada |Avenida |Viaduto |Praceta |Calçada |Alameda |Rotunda |Escadas |Passeio |Ladeira |Caminho ")~str_sub(rua,9),
                          str_starts(rua,"Travessa |Ciclovia |Tribunal ")~str_sub(rua,10),
                          str_starts(rua,"Miradouro |Escadaria ")~str_sub(rua,11))
         
  ) %>%
  mutate(teste=str_starts(nome,"^de\\s|^da\\s|^ da\\s|^do\\s"),
         clean=case_when(str_starts(nome,"^de\\s|^ da\\s|^da\\s|^do\\s")~str_sub(nome,4),
                         str_starts(nome,"^das\\s|^dos\\s")~str_sub(nome,5),
                         TRUE~nome)
  ) %>%
  filter(!is.na(clean)) %>%
  mutate(genero =get_gender(clean)) -> ruas

parques %>% clipr::write_clip()
pracas %>% clipr::write_clip()
pracas2 = clipr::read_clip_tbl()
parques2 = clipr::read_clip_tbl()
parques$sexoFinal2 = NULL
pracas = pracas %>% bind_cols(pracas2)%>%
  mutate(sexoFinal2 = case_when(sexoFinal2=="Nada"~NA_character_,
                                TRUE~sexoFinal2))
parques = parques %>% bind_cols(parques2) %>%
  mutate(sexoFinal2 = case_when(sexoFinal2=="Nada"~NA_character_,
                             TRUE~sexoFinal2))
# saveRDS(pracas,"pracas.rds")
# saveRDS(parques,"parques.rds")
#saveRDS(ruas,"ruas.rds")
ruas2 = ruas %>%
  mutate(sexoWiki = case_when(!is.na(clean)~tw_get_label(tw_get_property(tw_search(search = clean,limit = 1)$id,p="P21")$value),
                              TRUE~NA_character_))
#saveRDS(ruas2,"ruas2.rds")
ruas2 %>%
  mutate(sexoFinal = case_when(tolower(genero)==sexoWiki~genero,
                               !is.na(genero)& is.na(sexoWiki)~genero,
                               is.na(genero)&!is.na(sexoWiki)~stringr::str_to_title(sexoWiki))) %>%
  clipr::write_clip()

# 
# codigos = readRDS("C:\\Users\\rafae\\Downloads\\20221205 codigos ajustados.rds")
# codigos
# codigos2 = readRDS("C:\\Users\\rafae\\Downloads\\20221205 1041 ruas.rds")
# codigos2 %>% clipr::write_clip()

ruas2 %>% filter(!clean %in% codigos2$nome_arteria) %>% clipr::write_clip()

ruas3 = openxlsx::read.xlsx("C:\\Users\\rafae\\OneDrive\\Área de Trabalho\\ruas.xlsx",sheet = "Sheet6")
ruas4 = ruas3 %>% mutate(sexoFinal2 = case_when(ajustar=="Nada"~NA_character_,
                           ajustar=="Male"~"Male",
                           ajustar=="Female"~"Female",
                           is.na(ajustar)~sexoFinal))



streets2 = streets
streets2$osm_lines = streets$osm_lines %>% left_join(ruasfinal %>% select(rua,sexoFinal2),by=c("name"="rua"))

ruasAjustar = ruas %>% anti_join(ruas4 %>% select(rua,sexoFinal2),by=c("nome"="rua"))  %>%
  filter(!rua %in% ruas4$rua)
ruasAjustar %>% openxlsx::write.xlsx("ruasAjustar.xlsx")

ruasajustadas = openxlsx::read.xlsx("ruasAjustar.xlsx") %>%
  mutate(sexoFinal2 = case_when(ajustar=="Nada"~NA_character_,
  ajustar=="Male"~"Male",
  ajustar=="Female"~"Female",
  is.na(ajustar)~genero))

ruasfinal = bind_rows(ruas4 %>% select(-c(sexoWiki,sexoFinal)),
ruasajustadas) %>%
  mutate(sexoFinal2 = case_when(clean %in% c("Escritor Costa Barreto","Furriel Guilherme Dantas",
                                             "Moraes Caldas",
                                             "Pintor Júlio Resende",
                                             "Sá da Bandeira",
                                             "Marquês Sá da Bandeira",
                                             "São João do Porto",
                                             "São Paio",
                                             "São Pedro",
                                             "São Salvador",
                                             "Silva Porto",
                                             "Silva Ramos",
                                             "Silva Tapada",
                                             "Sousa Aroso",
                                             "Actor António Silva")~"Male",
                                clean %in% c("Nova do Fontão","Nova do Picão","Bela Vista","Navegantes")~NA_character_,
                                TRUE~sexoFinal2))

blankbg <-theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position = "none",
                plot.background=element_blank(),
                panel.grid.minor=element_blank(),
                panel.background=element_blank(),
                panel.grid.major=element_blank(),
                plot.margin = unit(c(t=1,r=1,b=1,l=1), "cm"),
                plot.caption = element_text(color = "grey20", size = 15,
                                            hjust = .5, face = "plain",
                                            family = "Didot"),
                panel.border = element_blank()
)
ggplot() +
  blankbg +
  geom_sf(data = streets2$osm_lines %>%
            filter(sexoFinal2 == "Male"),
          size = .8,
          color = "limegreen") +
  geom_sf(data = streets2$osm_lines %>%
            filter(sexoFinal2 == "Female"),
          size = .8,
          color = "brown1") +
  geom_sf(data = streets2$osm_lines %>%
            filter(is.na(sexoFinal2)),
          size = .35,
          color = "black") +
  labs(caption = 'Porto') +
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = FALSE)

ruasajustadas %>%
  mutate(sexoFinal2 = case_when(clean %in% c("Escritor Costa Barreto","Furriel Guilherme Dantas",
                                             "Moraes Caldas",
                                             "Pintor Júlio Resende",
                                             "Sá da Bandeira",
                                             "São João do Porto",
                                             "São Paio",
                                             "São Pedro",
                                             "São Salvador",
                                             "Silva Porto",
                                             "Silva Ramos",
                                             "Silva Tapada",
                                             "Sousa Aroso",
                                             "Actor António Silva")~"Male")) %>%
  filter(str_detect(clean,"Actor António Silva"))



streets2 = readRDS("C:\\Users\\belokurowsr\\Downloads\\streets2.rds")
ruasfinal = readRDS("C:\\Users\\belokurowsr\\Downloads\\ruasfinal.rds")
setwd("C:\\Users\\belokurowsr\\OneDrive - Kantar\\Desktop\\Kantar\\C?digoPostaisDistrito\\PT_adm2")
pt_adm2 = st_read("jt394dz4777.shp")
porto = pt_adm2 %>% filter(name_2 %in% c("Porto")) %>% mutate(name_3 = iconv(name_3,"latin1", "UTF-8"))

intersec = sf::st_intersection(streets2$osm_lines %>% select(osm_id,geometry), st_union(porto))
intersecParque = sf::st_intersection(gardens$osm_polygons %>% select(osm_id,geometry), st_union(porto))
intersec2 = sf::st_intersection(streets2$osm_lines %>% select(osm_id,geometry,name), st_union(porto))
intersecPraca = sf::st_intersection(squares$osm_polygons %>% select(osm_id,geometry), st_union(porto))

streets2$osm_lines = streets2$osm_lines %>%
  mutate(porto = case_when(osm_id %in% intersec$osm_id~1,
                           TRUE~0))

squares2 = squares
squares2$osm_polygons = squares2$osm_polygons %>%
  left_join(pracas %>% select(rua,sexoFinal2),by=c("name"="rua")) %>%
  mutate(porto = case_when(osm_id %in% intersecPraca$osm_id~1,
                           TRUE~0)) %>%
  mutate(name=iconv(name, "UTF-8","latin1"))

gardens2 = gardens
gardens2$osm_polygons = gardens2$osm_polygons %>%
  left_join(parques %>% select(rua,sexoFinal2),by=c("name"="rua")) %>%
  mutate(porto = case_when(osm_id %in% intersecParque$osm_id~1,
                           TRUE~0)) %>%
  mutate(name=iconv(name,"latin1", "UTF-8"))

parques %>% distinct(rua,sexoFinal2) %>%
  select(rua,sexoFinal2) %>% count(sexoFinal2)


gardens3 = gardens2$osm_polygons %>% bind_rows(squares2$osm_polygons)
pracasParques = gardens3 %>% filter(name %in% c(parques$rua,pracas$rua)) %>% distinct(name) %>%
  left_join(parques %>% bind_rows(pracas),by=c("name"="rua")) %>% count(sexoFinal2)


gardens3 %>% filter(porto==1) %>% as.data.frame() %>%
  count(sexoFinal2)

#save(porto,gardens3,streets2,file="dadosMapa.rdata")


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 40px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Nomes de ruas, praças e parques por gênero no Concelho do Porto")
)  

m= leaflet(options = leafletOptions(
  attributionControl=FALSE)) %>%
  addProviderTiles(provider = "CartoDB.Positron",group="Carto DB - Light",
                   options = providerTileOptions(attribution = "\u00A9 CartoDB")) %>%
  addProviderTiles("CartoDB.DarkMatter", group="Dark",
                   options = providerTileOptions(noWrap = F,
                                                 updateWhenZooming = FALSE,
                                                 updateWhenIdle = FALSE,opacity = 0.9,
                                                 attribution = ""
                                                 )) %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner - Light",
                   options = providerTileOptions(attribution = "")
                   ) %>%
  addPolygons(data = porto,stroke = F,color="white",opacity = 0.28,fillOpacity = 0.07,
              group = "Perímetro Porto") %>%
  addPolygons(data = gardens3 %>%
                filter(sexoFinal2 == "Male" & porto == 1 ),
              stroke = F,color="#1282A2",opacity = 0.7,fillOpacity = 0.70,
              group = "Parques - Homens",
              label = gardens3 %>%
                filter(sexoFinal2 == "Male" & porto == 1 )%>% pull(name)) %>%
  addPolygons(data = gardens3 %>%
                filter(sexoFinal2 == "Female" & porto == 1 ),
              stroke = F,color="#DC493A",opacity = 0.7,fillOpacity = 0.70,
              group = "Parques - Mulheres",
              label = gardens3 %>%
                filter(sexoFinal2 == "Female" & porto == 1 )%>% pull(name)) %>%
  leaflet::addPolylines(
    data = streets2$osm_lines %>%
      filter(is.na(sexoFinal2) & porto == 1),
    color = "#AAAAAA",#648767
    smoothFactor = 1,
    weight = 1.2,
    group="Ruas - Outros",
    label=streets2$osm_lines %>%
      filter(is.na(sexoFinal2) & porto == 1) %>% pull(name)
  ) %>%
  leaflet::addPolylines(
    data = streets2$osm_lines %>%
      filter(sexoFinal2 == "Male"& porto == 1),
    color = "#1282A2",
    smoothFactor = 0.8,
    weight = 1.8,
    group="Ruas - Homens",
    label = streets2$osm_lines %>%
      filter(sexoFinal2 == "Male"& porto == 1) %>% pull(name)
  ) %>%
  leaflet::addPolylines(
    data = streets2$osm_lines %>%
      filter(sexoFinal2 == "Female" & porto == 1)      ,
    color = "#DC493A",
    smoothFactor = 0.8,
    weight = 2.5,
    group="Ruas - Mulheres",
    label = streets2$osm_lines %>%
      filter(sexoFinal2 == "Female" & porto == 1) %>% pull(name)

  ) %>%
  addLayersControl( baseGroups = c("Carto DB - Light","Dark","Toner - Light"),
                    
                    overlayGroups = c("Ruas - Homens","Ruas - Mulheres","Ruas - Outros",
                                      "Perímetro Porto",
                                      "Parques - Homens","Parques - Mulheres"),
                   options = layersControlOptions(collapsed = T)) %>%
  addControl(title, position = "bottomleft") %>% 
  setView(lat=41.160922,lng = -8.624279, zoom=13)

m

mapshot(x=m,url="map.html",title="Map title")
mapshot(x=m,file ="map.png",vwidth = 1000, vheight = 533)

ruasintersec = intersec2 %>% as.data.frame() %>% count(name)

sexos = ruasfinal %>%
  filter(rua %in% ruasintersec$name) %>%
  count(rua,sexoFinal2) %>%
  count(sexoFinal2)

xdf  = pracasParques %>%
  mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
                                sexoFinal2=="Female"~"Nomes Femininos",
                                sexoFinal2=="Male"~"Nomes Masculinos"
  )) %>% mutate(prop = round(n/sum(n),2)*100)

#save.image("dados.rdata")
load("dados.rdata")

plotTotais = sexos %>% mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
                                        sexoFinal2=="Female"~"Nomes Femininos",
                                        sexoFinal2=="Male"~"Nomes Masculinos"
                                        )) %>%

  ggplot(aes(x=sexoFinal2, y=n,fill=sexoFinal2)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=n),hjust=1.2,
            #vjust=1.6,
            color="white",
            #position = position_dodge(0.9),
            size=6)+
  scale_fill_manual(values=c("#DC493A", "#1282A2","#949494"))+#DC493A #648767 ##FF5F15
  coord_flip()+
  labs(x="",y="",title="Nomes de espaços públicos do Porto por gênero")+
  theme_minimal()+theme(legend.position = "None")+
  theme(plot.title = element_text(size = 26,face="bold"),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18))
ggsave(plotTotais,filename = "test3.png", type = "cairo", width =140,
       height = 90, dpi = 300,units = "mm")
#Dados parques
#Mudar o azul
#Ver se as cores s?o colorblind-people-friendly
#aumentar fonte dos nomes no eixo y
#remover espa?o nas margens internas
#trocar provider dark Leaflet

plot=sexos %>% mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
                                        sexoFinal2=="Female"~"Nomes Femininos",
                                        sexoFinal2=="Male"~"Nomes Masculinos"
)) %>% mutate(prop = round(n/sum(n),2)*100) %>%
  ggplot(aes(fill = sexoFinal2, values = prop)) +
  geom_waffle(n_rows = 10, size = 1.3, colour = "white",flip=T) +
  scale_fill_manual(name = NULL,
                    values = c("#DC493A", "#1282A2","#949494"),
                    labels = c("Nomes Femininos", "Nomes Masculinos",
                               "Outros")) +
  coord_equal() +
  theme_void()+
  labs(title = "Nomes de espaços públicos por gênero - Porto (Portugal) ") +
  theme(plot.title = element_text(size = 28,face="bold"),
        legend.text=element_text(size=19)) +  
  annotate("text", x = 3, y = 1, label ="              913    (5%)    ",color="white",
           size=5) +
  annotate("text", x = 5, y = 3, label = "               913    (44%)    ",color="white",
           size=5,halign="left")+
  annotate("text", x = 5, y = 8, label = "              1073    (51%)    ",color="white",
           size=5,halign="left") +
  theme_enhance_waffle()

ggsave(plot,filename = "test.png", type = "cairo", width = 90,
       height = 90, dpi = 300,units = "mm")





#saveRDS(xdf,"xdf.rds")
#xdf=readRDS("C:\\Users\\rafae\\OneDrive\\Área de Trabalho\\xdf.rds")


plot2 = ggplot(xdf, aes(label = sexoFinal2, values = prop)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE,
                 aes(color=sexoFinal2),flip=T,size=14) +
  scale_color_manual(
    name = NULL,
    values = c("#DC493A", "#1282A2","#949494"),
    labels = c("Nomes Femininos", "Nomes Masculinos", "Outros")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      Masc = "tree",
      Fem = "tree",
      Outros = "tree"
    ),
    labels=c("Nomes Femininos", "Nomes Masculinos", "Outros")
  ) +
  coord_equal() +
  theme_void()+
  labs(title = "Nomes de praças e parques públicos por gênero") +
  theme(plot.title = element_text(size = 26,face="bold"),
        legend.text=element_text(size=19),
        legend.position = "none")+
  theme_enhance_waffle()  +  
  annotate("text", x = 2, y = 0.4, label ="5 (4%)",color="black",
           size=6)+
  annotate("text", x = 10.8, y = 3, label ="41 (31%)",color="black",
           size=6)+
  annotate("text", x = 10.8, y = 8, label ="88 (66%)",color="black",
           size=6)
ggsave(plot2,filename = "test2.png", type = "cairo", width = 120,
       height = 90, dpi = 300,units = "mm")

sexos %>% mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
                                        sexoFinal2=="Female"~"Nomes Femininos",
                                        sexoFinal2=="Male"~"Nomes Masculinos"
)) %>% filter(!str_detect(sexoFinal2,"Outros")) %>% 
  mutate(prop = round(n/sum(n),2)*100) %>%
  ggplot(aes(fill = sexoFinal2, values = prop)) +
  geom_waffle(n_rows = 10, size = 1.3, colour = "white",flip=T) +
  scale_fill_manual(name = NULL,
                    values = c("#DC493A", "#1282A2","#648767"),
                    labels = c("Femininos", "Masculinos", "Outros")) +
  coord_equal() +
  theme_void()+
  theme_enhance_waffle()




# sexos %>% mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
#                                         sexoFinal2=="Female"~"Nomes Femininos",
#                                         sexoFinal2=="Male"~"Nomes Masculinos"
# )) %>% mutate(prop = round(n/sum(n),2)*100) %>% 
sexos = gardens3 %>% filter(name %in% c(parques$rua,pracas$rua)) %>% distinct(name) %>%
  left_join(parques %>% bind_rows(pracas),by=c("name"="rua")) %>% 
  bind_rows(ruasfinal) %>%
  filter(rua %in% c(ruasintersec$name,intersecParque$name,intersecPraca$name)) %>%
  count(rua,sexoFinal2) %>%
  count(sexoFinal2)
  
  sexos %>% mutate(sexoFinal2 = case_when(is.na(sexoFinal2)~"Outros nomes",
                                          sexoFinal2=="Female"~"Nomes Femininos",
                                          sexoFinal2=="Male"~"Nomes Masculinos"
  )) %>% filter(!str_detect(sexoFinal2,"Outros")) %>% mutate(prop = round(n/sum(n),2)*100) %>% 
  ggplot(aes(label = sexoFinal2, values = prop)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE,aes(color=sexoFinal2),
                 show.legend = F) +
  scale_color_manual(
    name = NULL,
    values = c("#DC493A", "#1282A2","#648767"),
    labels = c("Nomes Femininos", "Nomes Masculinos", "Outros")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      Masc = "user",
      Fem = "user",
      Outros = "user"
    ),
    labels=c("Nomes Femininos", "Nomes Masculinos", "Outros")
  ) +
  coord_equal() +
  hrbrthemes::theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))
  