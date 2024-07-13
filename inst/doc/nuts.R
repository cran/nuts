## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  dpi=300,
  fig.width = 8,
  fig.height = 5,
  out.width = "100%",
  R.options = list(width = 70)
)

# Needed to fix issues with phantom.js
Sys.setenv(OPENSSL_CONF="/dev/null")

## ----out.width='150px', out.extra='style="float:left; padding:30px"', echo=FALSE, fig.alt ="Package logo of a squirrel holdling a walnut colored with the flag of Europe"----
knitr::include_graphics("logo.png")

## ----echo = FALSE, message=F, warning=F-----------------------------
library(nuts)
library(stringr)
library(ggplot2)
library(ggrepel)
library(sf)
library(terra)
library(raster)
library(kableExtra)
library(ggpubr)
library(formatR)
library(ggalluvial)
library(dplyr)

## ----echo = FALSE , warning=F , message=F,results='hide'------------

data(manure, package = "nuts")

manure_indic <- manure %>%
  filter(nchar(geo) == 4) %>%
  filter(indic_ag == "I07A_EQ_Y") %>%
  dplyr::select(-indic_ag ) %>%
  filter( str_detect(geo, "^DE")) %>%
  filter( time == 2003 )

class <- manure_indic %>%
  distinct(geo, .keep_all = T) %>%
  nuts_classify(
    data = ., 
    nuts_code = "geo"
    )

transf <- class %>% 
  nuts_convert_version( 
    to_version = "2010", 
    variables = c('values'='absolute'), 
    weight = 'artif_surf12' 
    )

small <- manure_indic %>% 
  filter( geo %in% c( 'DED1' , 'DED3' ) )
small <- small %>% 
  mutate( artif_surf12 = case_when( geo == "DED1" ~ 98648
                                             , geo == "DED3" ~ 66786 + 5574
))

shape_06_n3 <- read_sf("shapefiles/NUTS_RG_20M_2006_3857_DE.shp") %>%
  filter(LEVL_CODE == 3) %>%
  full_join( manure_indic , by = c("NUTS_ID" = "geo")) %>%
  filter( str_detect( NUTS_ID , '^DED1|^DED3' ))

shape_06_n2 <- read_sf("shapefiles/NUTS_RG_20M_2006_3857_DE.shp") %>%
  filter(LEVL_CODE == 2) %>%
  full_join( manure_indic , by = c("NUTS_ID" = "geo")) %>%
  filter( str_detect( NUTS_ID , '^DED1|^DED3' ))

shape_06_n2_centr <- shape_06_n2 %>% 
  st_centroid( )  %>% 
  left_join( small , by = c("NUTS_ID" = "geo"))

p_initial = ggplot() +
  geom_sf(data = shape_06_n2 ,  linewidth = 1 , aes( fill = NUTS_ID )) +
  geom_sf(data = shape_06_n3 , color = 'grey' , linewidth = .5 , fill = NA ) +
  geom_sf_label( data = shape_06_n2_centr
                 , aes( label = paste0( NUTS_ID , ': ' , NUTS_NAME , '\n' , values.x , ' facilties' , '\n' , 'BU: ' , artif_surf12 ))
                 , size = 3
  ) +
  scale_fill_manual( values = c( "#177e89" , "#ffc857" )) +
  theme_minimal( ) +
  facet_wrap(~"2003 data\n\n(NUTS VERSION 2006)") +
  theme(legend.position = "none"
        , axis.text = element_blank( )
        , axis.ticks = element_blank( )
        , axis.title = element_blank( )
  )

DED33_centr <- shape_06_n3 %>% 
  filter( NUTS_ID == "DED33" )  %>% 
  st_centroid( ) %>% 
  mutate( artif_surf12 = 5574 )

shape_06_n2_step <- shape_06_n2 %>%
  mutate( artif_surf12 = case_when( NUTS_ID == 'DED3' ~ 66786
                            , NUTS_ID == 'DED1' ~ 98648 )
          , NUTS_ID = case_when( NUTS_ID == 'DED3' ~ "DED5"
                                 , NUTS_ID == 'DED1' ~ "DED4" )
  )

p_step = ggplot() +
  geom_sf(data = shape_06_n2 ,  linewidth = 1 , aes( fill = NUTS_ID )) +
  geom_sf(data = shape_06_n3 , color = 'grey' , fill = NA , linewidth = .5 ) +

  geom_sf_label( data = shape_06_n2_step , aes( label = paste0( NUTS_ID , ': ' , NUTS_NAME , '\n' , '??? facilities' , '\n' , 'BU: ' , artif_surf12 ))
                 , size = 3) +
  geom_sf(data = shape_06_n3 %>% filter( NUTS_ID %in% c("DED33")), color = 'red' , fill = NA , linewidth = 1 ) +
  geom_sf_text( data = DED33_centr , aes( label = paste0( "BU: \n" ,  artif_surf12 )), size = 3 , lineheight = .75 ) +
  scale_fill_manual( values = c( "#177e89" , "#ffc857" )) +
  theme_minimal( ) +
  facet_wrap(~"2003 data\n\n(NUTS VERSION 2006 → 2010)") +
  theme(legend.position = "none"
        , axis.text = element_blank( )
        , axis.ticks = element_blank( )
        , axis.title = element_blank( ))

moved <- 700 * (5574 / 72360)
DED5 <- 700 - 54
DED4 <- 2600 + 54

shape_06_n2_final <- shape_06_n2_step %>%
  mutate( artif_surf12 = case_when( NUTS_ID == 'DED5' ~ 66786
                            , NUTS_ID == 'DED4' ~ 98648 + 5574 )
          , values.x = case_when( NUTS_ID == 'DED5' ~ DED5
                                  , NUTS_ID == 'DED4' ~ DED4 )
  )

p_final = ggplot() +
  geom_sf(data = shape_06_n2 ,  linewidth = 1 , aes( fill = NUTS_ID )) +
  geom_sf(data = shape_06_n3 , color = 'grey' , fill = NA , linewidth = .5 ) +
  geom_sf_label( data = shape_06_n2_final
                 , aes( label = paste0( NUTS_ID , ': ' , NUTS_NAME , '\n' , round( values.x , 1 ) , ' facilties' , '\n' , 'BU: ' , artif_surf12 ))
                 , size = 3 ) +
  geom_sf(data = shape_06_n3 %>% filter( NUTS_ID %in% c("DED33")), color = 'red' , fill = "#177e89" , linewidth = 1 ) +
  scale_fill_manual( values = c( "#177e89" , "#ffc857" )) +
  theme_minimal( ) +
  facet_wrap(~"2003 data\n\n(NUTS VERSION 2010)") +
  theme(legend.position = "none"
        , axis.text = element_blank( )
        , axis.ticks = element_blank( )
        , axis.title = element_blank( ))

## ----echo=FALSE, fig.cap="Holdings with Manure Storage Facilities; BU = Built-up area in square meters; Sources: [Shapefiles](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts) and [data](https://ec.europa.eu/eurostat/databrowser/view/PAT_EP_RTOT/default/table) are from EUROSTAT; Created using the [sf](https://r-spatial.github.io/sf/) package.", fig.alt ="Maps of NUTS 3 regions Chemnitz and Leipzig in NUTS version 2003, between 2003 and 2006 and 2006. They visualize the example in the text in which Chemnitz contributes a part of its area to Leipzig."----
gridExtra::grid.arrange( p_initial, p_step , p_final , nrow = 1 )

## ----echo=FALSE, out.width='60%', fig.align="center", fig.cap="Sequential workflow to convert regional NUTS data", fig.alt ="Flow diagram that shows that conversion functions are run after classification."----
knitr::include_graphics("flow.png")

## -------------------------------------------------------------------
# Load packages
library(nuts)
library(dplyr)
library(stringr)

# Loading and subsetting Eurostat data
data(patents, package = "nuts")

pat_n2 <- patents %>% 
  filter(nchar(geo) == 4) # NUTS-2 values

pat_n2_mhab_12_no <- pat_n2 %>%
  filter(unit == "P_MHAB") %>% # Patents per one million inhabitants
  filter(time == 2012) %>% # 2012
  filter(str_detect(geo, "^NO")) %>%  # Norway
  dplyr::select(-unit)

# Classifying the Data
pat_classified <- nuts_classify(
  data = pat_n2_mhab_12_no,
  nuts_code = "geo"
  )

## -------------------------------------------------------------------
# pat_classified$data # Call list item directly or...
nuts_get_data(pat_classified) # ...use helper function

## -------------------------------------------------------------------
# pat_classified$versions_data # Call list item directly or...
nuts_get_version(pat_classified) # ...use helper function

## -------------------------------------------------------------------
# pat_classified$missing_data # Call list item directly or...
nuts_get_missing(pat_classified) # ...use helper function

## -------------------------------------------------------------------
# Converting Data to 2021 NUTS version
pat_converted <- nuts_convert_version(
  data = pat_classified,
  to_version = "2021",
  variables = c("values" = "relative")
)

## ----echo=FALSE,include=FALSE---------------------------------------
no_2006 <-
  read_sf("shapefiles/NUTS_RG_20M_2016_3857_NO.shp") %>%
  filter(LEVL_CODE == 2) %>%
  full_join(pat_n2_mhab_12_no , by = c("NUTS_ID" = "geo"))

no_changes <- cross_walks %>%
  filter(
    nchar(from_code) == 4,
    from_version == 2016,
    to_version == 2021,
    grepl("^NO", from_code),
    from_code != to_code
  )

no_changes <- unique(c(no_changes$from_code, no_changes$to_code))

gg_2006 = ggplot() +
  geom_sf(
    data = no_2006,
    aes(fill = values) ,
    color = 'grey' ,
    linewidth = .5
  ) +
  geom_sf(
    data = filter(no_2006, NUTS_ID %in% no_changes),
    color = 'red' ,
    fill = "#00000000"
  ) +
  scale_fill_continuous(high = "#132B43",
                        low = "#56B1F7",
                        name = "Patents per 1 M habitants") +
  theme_minimal() +
  facet_wrap( ~ "Original 2012 data\n\n(NUTS VERSION 2016)")

no_2021 <-
  read_sf("shapefiles/NUTS_RG_20M_2021_3857_NO.shp") %>%
  filter(LEVL_CODE == 2) %>%
  full_join(pat_converted , by = c("NUTS_ID" = "to_code")) %>%
  filter(NUTS_ID != "NO0B")

gg_2021 = ggplot() +
  geom_sf(
    data = no_2021,
    aes(fill = values) ,
    color = 'grey' ,
    linewidth = .5
  ) +
  geom_sf(
    data = filter(no_2021, NUTS_ID %in% no_changes),
    color = 'red' ,
    fill = "#00000000"
  ) +
  scale_fill_continuous(high = "#132B43",
                        low = "#56B1F7",
                        name = "Patents per 1 M habitants") +
  theme_minimal() +
  facet_wrap( ~ "Transformed data\n\n(NUTS VERSION 2021)")


## ----echo=FALSE, fig.cap="Converting patent data between versions; Sources: [Shapefiles](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts) and [data](https://ec.europa.eu/eurostat/databrowser/view/PAT_EP_RTOT/default/table?lang=en) are from EUROSTAT; Created using the [sf](https://r-spatial.github.io/sf/) package.", fig.alt ="Two maps of patents per 1M habitants in Norwegian NUTS 2 regions in NUTS version 2016 and converted to NUTS version 2021"----
ggarrange( gg_2006 , gg_2021 , nrow = 1 , common.legend = T, legend = "bottom")

## -------------------------------------------------------------------
pat_n2_mhab_12_no

## -------------------------------------------------------------------
pat_converted

## -------------------------------------------------------------------
# Converting Multiple Variables
pat_n2_mhab_12_no %>%
  mutate(values_per_thous = values * 1000) %>%
  nuts_classify(
    data = .,
    nuts_code = "geo"
    ) %>%
  nuts_convert_version(
    data = .,
    to_version = "2021",
    variables = c("values" = "relative",
                  "values_per_thous" = "relative")
  )

## ----tidy=TRUE, tidy.opts=list(width.cutoff=60)---------------------
# Classifying grouped data (time)
pat_n2_mhab_sesihr <- pat_n2 %>%
  filter(unit == "P_MHAB") %>%
  filter(str_detect(geo, "^SE|^SI|^HR"))

pat_classified <- nuts_classify(
  nuts_code = "geo",
  data = pat_n2_mhab_sesihr,
  group_vars = "time"
  )

## ----tidy=TRUE, tidy.opts=list(width.cutoff=60)---------------------
nuts_get_data(pat_classified) %>%
  group_by(country, from_version) %>%
  tally()

## -------------------------------------------------------------------
# Converting grouped data (Time)
pat_converted <- nuts_convert_version(
  data = pat_classified,
  to_version = "2021",
  variables = c("values" = "relative")
)

## -------------------------------------------------------------------
# Classifying and converting multi-group data
pat_n2_mhabmact_12_sesihr <- pat_n2 %>%
  filter(unit %in% c("P_MHAB", "P_MACT")) %>%
  filter(str_detect(geo, "^SE|^SI|^HR"))

pat_converted <- pat_n2_mhabmact_12_sesihr %>%
  nuts_classify(
    data = .,
    nuts_code = "geo",
    group_vars = c("time", "unit")
  ) %>%
  nuts_convert_version(
    data = .,
    to_version = "2021",
    variables = c("values" = "relative")
  )

## -------------------------------------------------------------------
data("patents", package = "nuts")
# Aggregating data from NUTS-3 to NUTS-2 and NUTS-1
pat_n3 <- patents %>% 
  filter(nchar(geo) == 5)

pat_n3_nr_12_se <- pat_n3 %>%
  filter(unit %in% c("NR")) %>%
  filter(time == 2012) %>%
  filter(str_detect(geo, "^SE"))

pat_classified <- nuts_classify(
  data = pat_n3_nr_12_se,
  nuts_code = "geo"
  )

pat_level2 <- nuts_aggregate(
  data = pat_classified,
  to_level = 2,
  variables = c("values" = "absolute")
)

pat_level1 <- nuts_aggregate(
  data = pat_classified,
  to_level = 1,
  variables = c("values" = "absolute")
)

## ----echo=FALSE, fig.cap="Aggregating patents from NUTS 3 to NUTS 2 and NUTS 1; Sources: [Shapefiles](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts) and [data](https://ec.europa.eu/eurostat/databrowser/view/PAT_EP_RTOT/default/table?lang=en) are from EUROSTAT; Created using the [sf](https://r-spatial.github.io/sf/) package.", fig.alt = "Three maps of Sweden with patent applications at the NUTS 3 level and aggregated to NUTS level 2 and 1."----
eu_nuts3 <-
  read_sf("shapefiles/NUTS_RG_20M_2016_3857_SE.shp") %>%
  filter(LEVL_CODE == 3) %>%
  full_join(pat_n3_nr_12_se , by = c("NUTS_ID" = "geo"))

eu_nuts2 <-
  read_sf("shapefiles/NUTS_RG_20M_2016_3857_SE.shp") %>%
  filter(LEVL_CODE == 2) %>%
  full_join(pat_level2 , by = c("NUTS_ID" = "to_code"))

eu_nuts1 <-
  read_sf("shapefiles/NUTS_RG_20M_2016_3857_SE.shp") %>%
  filter(LEVL_CODE == 1) %>%
  full_join(pat_level1 , by = c("NUTS_ID" = "to_code"))

gg_nuts3 = ggplot() +
  geom_sf(
    data = eu_nuts3,
    aes(fill = values) ,
    color = 'grey' ,
    linewidth = .25
  ) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_minimal() + theme(legend.position = "bottom") +
  facet_wrap( ~ "NUTS-3 data")

gg_nuts2 = ggplot() +
  geom_sf(
    data = eu_nuts2,
    aes(fill = values) ,
    color = 'grey' ,
    linewidth = .25
  ) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_minimal() + theme(legend.position = "bottom") +
  facet_wrap( ~ "NUTS-2 data")

gg_nuts1 = ggplot() +
  geom_sf(
    data = eu_nuts1,
    aes(fill = values) ,
    color = 'grey' ,
    linewidth = .25
  ) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_minimal() + theme(legend.position = "bottom") +
  facet_wrap( ~ "NUTS-1 data")

gg = ggpubr::ggarrange(gg_nuts3 , gg_nuts2 , gg_nuts1 , nrow = 1)
annotate_figure(gg, top = text_grob("Patent applications across Swedish NUTS regions"))

## -------------------------------------------------------------------
pat_n3.nr.12.dk <- pat_n3 %>%
  filter(unit %in% c("NR")) %>%
  filter(time == 2012) %>%
  filter(str_detect(geo, "^DK"))

pat_classified <- nuts_classify(
  data = pat_n3.nr.12.dk, 
  nuts_code = "geo"
  )

## -------------------------------------------------------------------
pat_n3_nr_12_si <- pat_n3 %>%
  filter(unit %in% c("NR")) %>%
  filter(time == 2012) %>%
  filter(str_detect(geo, "^SI"))

pat_classified <- nuts_classify(
  data = pat_n3_nr_12_si, 
  nuts_code = "geo"
  )

## -------------------------------------------------------------------
nuts_get_missing(pat_classified)

## -------------------------------------------------------------------
nuts_convert_version(
  data = pat_classified, 
  to_version = "2021", 
  variables = c("values" = "absolute")
  ) %>% 
  filter(is.na(values))

## -------------------------------------------------------------------
nuts_convert_version(
  data = pat_classified, 
  to_version = "2021", 
  weight = "pop18",
  variables = c("values" = "absolute"),
  missing_weights_pct = TRUE
  ) %>% 
  arrange(desc(values_na_w))

## -------------------------------------------------------------------
nuts_convert_version(
  data = pat_classified, 
  to_version = "2021", 
  weight = "pop18",
  variables = c("values" = "absolute"),
  missing_weights_pct = TRUE,
  missing_rm = TRUE
  ) %>% 
  filter(to_code %in% c("SI031", "SI036", "SI037")) %>% 
  mutate(values_imp = ifelse(values_na_w < 1, values, NA))

## ----error=TRUE-----------------------------------------------------
patents %>% 
  filter(nchar(geo) %in% c(4, 5), grepl("^EL", geo)) %>% 
  distinct(geo, .keep_all = T) %>% 
  nuts_classify(nuts_code = "geo", data = .)

## -------------------------------------------------------------------
man_deit <- manure %>% 
  filter(grepl("^DE|^IT", geo)) %>%
  filter(nchar(geo) == 4, ) %>% 
  distinct(geo, .keep_all = T) %>% 
  nuts_classify(nuts_code = "geo", data = .)

nuts_get_data(man_deit) %>% 
  group_by(country, from_version) %>% 
  tally()

## -------------------------------------------------------------------
man_deit_converted <- nuts_convert_version(
  data = man_deit,
  to_version = 2021,
  variables = c("values" = "relative"),
  multiple_versions = "most_frequent"
)

man_deit_converted %>% 
  group_by(country, to_version) %>% 
  tally()

## ----echo=FALSE, message = FALSE, warning = FALSE, fig.cap="Norwegian NUTS-2 regions with boundary changes; Sources: [Shapefiles](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts) from EUROSTAT; Created using the [sf](https://r-spatial.github.io/sf/) package.", fig.alt ="Two maps of Norwegian NUTS-2 regions in version 2016 and 2021. The most Eastern and Southern regions have been affected most by administrative redistricting."----
no_2016 <- read_sf("shapefiles/NUTS_RG_20M_2016_3857_NO.shp") %>%
  filter(nchar(NUTS_ID) == 4) %>%
  mutate(nuts = paste0(NUTS_ID, "\n", NUTS_NAME))

no_2021 <- read_sf("shapefiles/NUTS_RG_20M_2021_3857_NO.shp") %>%
  filter(nchar(NUTS_ID) == 4, NUTS_ID != "NO0B") %>%
  mutate(nuts = paste0(NUTS_ID, "\n", NUTS_NAME))

no_codes <- unique(c(no_2016$NUTS_ID, no_2021$NUTS_ID))
colorz = RColorBrewer::brewer.pal(length(no_codes), "Set3")
names(colorz) <- no_codes

b = 1700000
gg_2016 = ggplot() +
  geom_sf(data = no_2016, aes(fill = NUTS_ID ) , color = 'grey' , linewidth = .5 ) +
  scale_fill_manual(values = colorz) +
  geom_sf_text(data = no_2016, aes(label = NUTS_ID)) +
  theme_minimal( ) +
  labs(subtitle = "2016 version") +
  xlab("") + ylab("") +
  coord_sf(xlim = c(504756.9,3441975-b), ylim = c(7965649,11442790-b))

gg_2021 = ggplot() +
  geom_sf(data = no_2021, aes(fill = NUTS_ID ) , color = 'grey' , linewidth = .5 ) +
  scale_fill_manual(values = colorz) +
  geom_sf_text(data = no_2021, aes(label = NUTS_ID)) +
  theme_minimal( ) +
  labs(subtitle = "2021 version") +
  xlab("") + ylab("") +
  coord_sf(xlim = c(504756.9,3441975-b), ylim = c(7965649,11442790-b))

gg = ggarrange( gg_2016 , gg_2021 , nrow = 1 ,  legend = "none")
annotate_figure(gg) #, top = text_grob("Norwegian NUTS-2 regions with boundary changes"))

## -------------------------------------------------------------------
no_walks <- cross_walks %>%
  filter(nchar(from_code) == 4,
         from_version == 2016,
         to_version == 2021,
         grepl("^NO", from_code))

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
kable(no_walks, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'azure') %>%
  column_spec(2, background = 'aquamarine') %>%
  column_spec(3, background = 'azure') %>%
  column_spec(4, background = 'aquamarine') %>%
  scroll_box(width = "100%") 

## ----echo=FALSE, message = FALSE, warning = FALSE, fig.cap= "Alluvial plot illustrating area size flows; Created using the [ggalluvial](https://corybrunson.github.io/ggalluvial/) package.", fig.alt ="The alluvial plot shows population flows from NUTS version 2016 to 2021."----
# Add names
no_2016_names <- read_sf("shapefiles/NUTS_RG_20M_2016_3857_NO.shp") %>%
  dplyr::select(from_code = NUTS_ID, from_name = NUTS_NAME) %>%
  st_set_geometry(NULL)
no_2021_names <- read_sf("shapefiles/NUTS_RG_20M_2021_3857_NO.shp") %>%
  dplyr::select(to_code = NUTS_ID, to_name = NUTS_NAME) %>%
  st_set_geometry(NULL)

no_walks <- no_walks %>%
  inner_join(no_2016_names) %>%
  inner_join(no_2021_names)

gg_pop_flows <- no_walks %>%
  mutate(from = paste0(from_code, "\n", from_name),
         to = paste0(to_code, "\n", to_name)) %>%
  arrange(desc(to_code)) %>%
  ggplot(data = .,
       aes(axis1 = from, axis2 = to,
           y = areaKm ^ 0.3)) +
  geom_alluvium(aes(fill = from)) +
  geom_stratum() +
    scale_x_discrete(limits = c("v2016", "v2021")) +
    ggfittext::geom_fit_text(stat = "stratum", width = 1/4, min.size = 3, aes(label = after_stat(stratum))) +
    theme_minimal() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ylab("Area (sqkm)") +
  theme(legend.position = "none")+
  labs(title = "NUTS-2 Area Flows in Norway from versions 2016 to 2021",
       caption = "Flow size is scaled for improved readability")
gg_pop_flows

## ----echo=FALSE, message = FALSE, warning = FALSE, out.width = "100%", fig.width = 7, fig.cap= "Spatial distribution of population and boundary changes; Sources: [Shapefiles](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts) and [population raster](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat) from EUROSTAT; Created using the [sf](https://r-spatial.github.io/sf/) and the [terra](https://rspatial.github.io/terra/reference/terra-package.html) packages.", fig.alt ="Two maps of Southern Norway with very granular population density and administrative boundaries of the 2016 and 2021 NUTS version. The region with the capital Olso and its adjacent region are highlighted in version 2016 that both contribute to a larger single region in version 2021."----

# pop <- raster("JRC_1K_POP_2018.tif")
# no_2016_1 <- no_2016 %>% st_transform(crs(pop))
# saveRDS( no_2016_1 , 'JRC_1K_POP_2018_2016_transformed_NO.rds'  )
# no_2021_1 <- no_2021 %>% st_transform(crs(pop))
# saveRDS( no_2021_1 , 'JRC_1K_POP_2018_2021_transformed_NO.rds'  )
# no_pop <- crop(x = pop, y = as_Spatial(no_2016_1))
# no_pop <- mask(no_pop, as_Spatial(no_2016_1))
# no_pop_df <- as.data.frame(no_pop, xy = TRUE) %>%
#   filter(!is.na(JRC_1K_POP_2018))
no_pop_df <- readRDS( 'JRC_1K_POP_2018_NO.rds' )
no_2016_1 <- readRDS( 'JRC_1K_POP_2018_2016_transformed_NO.rds' )
no_2021_1 <- readRDS( 'JRC_1K_POP_2018_2021_transformed_NO.rds' )

c=500000
d=500000
no_2016_no01 <- filter(no_2016_1, NUTS_ID %in% c( 'NO01' , 'NO03' ))
no_2021_no08 <- filter(no_2021_1, NUTS_ID %in% c( 'NO08' ))
gg_pop = ggplot() +
  geom_raster(data = no_pop_df, aes(x = x, y = y, fill = JRC_1K_POP_2018)) +
  geom_sf(data = no_2016_1, color = "#636363", fill = NA, lwd = .5 ) +
  geom_sf(data = no_2016_no01 , aes( color = NUTS_ID ) , fill = NA, lwd = .5 ) +
  scale_fill_gradientn(name = "2018 Population", colors = terrain.colors(10),
                       na.value = NA) +
  geom_label_repel(data = no_2016_no01, aes(label = NUTS_ID, geometry = geometry , color = NUTS_ID )
                   , stat = "sf_coordinates", size = 3.5
                   , point.padding = 15
                   , fill = alpha(c("white"),0.9)) +
  scale_color_manual( values = c( 'orange' , 'red' )) +
  coord_sf(xlim = c(4023000,5130000-c), ylim = c(3879000,5411000-d))+
  theme_minimal()+
  theme(legend.position = "none"
        , axis.text = element_blank( )
        , axis.ticks = element_blank( )
        , axis.title = element_blank( ))+
  xlab("") + ylab("")+
  labs(title = "2016 NUTS version")

gg_pop2 = ggplot() +
  geom_raster(data = no_pop_df, aes(x = x, y = y, fill = JRC_1K_POP_2018)) +
  geom_sf(data = no_2021_1, color = "#636363", fill = NA, lwd = .5 ) +
  geom_sf(data = no_2021_1 %>% filter( NUTS_ID %in% c( 'NO08' )), color = "blueviolet", fill = NA, lwd = .5 ) +
  scale_fill_gradientn(name = "2018 Population", colors = terrain.colors(10),
                       na.value = NA) +
  geom_label_repel(data = no_2021_no08, aes(label = NUTS_ID, geometry = geometry )
                   , stat = "sf_coordinates", size = 3.5
                   , point.padding = 15
                   , color = "blueviolet"
                   , fill = alpha(c("white"),0.9)) +
  coord_sf(xlim = c(4023000,5130000-c), ylim = c(3879000,5411000-d))+
  theme_minimal()+
  theme(legend.position = "none"
        , axis.text = element_blank( )
        , axis.ticks = element_blank( )
        , axis.title = element_blank( ))+
  xlab("") + ylab("")+
  labs(title = "2021 NUTS version")
ggpubr::ggarrange( gg_pop , gg_pop2 )

## ----include = F----------------------------------------------------
pat_n2_nrmhab_12_no <- patents %>%
  filter(nchar(geo) == 4) %>%  # NUTS-2 values
  filter(unit %in% c("NR", "P_MHAB")) %>%
  filter(time == 2012) %>% # 2012
  filter(str_detect(geo, "^NO")) %>%  # Norway
  dplyr::select(-"time") %>%
  tidyr::pivot_wider(id_cols = c("geo"), names_from = "unit",
              values_from = "values")

classification <- pat_n2_nrmhab_12_no %>%
  nuts_classify(nuts_code = "geo")

conversion_m_long <- nuts_get_data(classification) %>%
  filter(!is.na(from_version)) %>%
  inner_join(filter(cross_walks, to_version == 2021),
             by = c("from_code", "from_version")) %>%
  mutate(pop18 = round(pop18 / 1000, 2)) %>%
  mutate_at(vars(NR, P_MHAB, pop18), list(~as.integer(.))) %>%
  dplyr::select(from_code, to_code, from_version , to_version, NR, P_MHAB, pop18)

convert_abs <- conversion_m_long %>%
  group_by(from_code, from_version) %>%
  mutate(w = round(pop18 / sum(pop18), 2)) %>%
  ungroup()

# Illustrate calculation
from_code_vec = unique(convert_abs$from_code)
calcs = list()
for(i in seq_along(from_code_vec)){
  print(i)
  convert_abs_sub = convert_abs %>%
  filter(from_code %in% from_code_vec[i])
  calcs[[i]] = paste(convert_abs_sub$pop18, collapse = " + ")
}
calcs <- data.frame(from_code = from_code_vec, denom = unlist(calcs))
convert_abs_calc <- convert_abs %>%
  inner_join(calcs) %>%
  mutate(w = paste0(pop18, "/(", denom, ") = ", w)) %>%
  dplyr::select(-denom, -P_MHAB)

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
kable(convert_abs_calc, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'azure') %>%
  column_spec(2, background = 'aquamarine') %>%
  column_spec(3, background = 'azure') %>%
  column_spec(4, background = 'aquamarine') %>%
  column_spec(7, background = '#FFBBFF') %>%
  scroll_box(width = "100%") 

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
to_code_vec = unique(convert_abs$to_code)
calcs = list()
for (i in seq_along(to_code_vec)) {
  convert_abs_sub = convert_abs %>%
    filter(to_code %in% to_code_vec[i])
  calcs[[i]] = paste(paste0(convert_abs_sub$NR, " x ", convert_abs_sub$w),
                     collapse = " + ")
}
calcs <- data.frame(to_code = to_code_vec, NR = unlist(calcs))

converted_abs <- convert_abs %>%
  group_by(to_code, to_version) %>%
  summarise(NR_res = sum(NR * w)) %>%
  ungroup()

converted_abs_calc <- inner_join(converted_abs, calcs) %>%
  mutate(NR = paste0(NR, " = " , NR_res)) %>%
  dplyr::select(-NR_res)

kable(converted_abs_calc, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'aquamarine') %>%
  column_spec(2, background = 'aquamarine') %>%
  column_spec(3, background = '#FFF0F5') %>%
  scroll_box(width = "100%")

## ----include = F----------------------------------------------------
convert_rel <- conversion_m_long %>%
  group_by(to_code, to_version) %>%
  summarise(P_MHAB_conv = round(sum(P_MHAB * pop18) / sum(pop18), 0)) %>%
  ungroup()

# Illustrate calculation
to_code_vec = unique(convert_abs$to_code)
nums = list()
denoms = list()
for (i in seq_along(to_code_vec)) {
  print(i)
  convert_abs_sub = convert_abs %>%
    filter(to_code %in% to_code_vec[i])
  nums[[i]] = paste0(paste0(convert_abs_sub$pop18, " x ", convert_abs_sub$P_MHAB) ,
                     collapse = " + ")
  denoms[[i]] = paste0(convert_abs_sub$pop18 , collapse = " + ")
}

calcs <- data.frame(to_code = to_code_vec,
                    denom = unlist(denoms),
                    num = unlist(nums))

converted_rel_calc <- convert_rel %>%
  inner_join(calcs) %>%
  mutate(P_MHAB_calc = paste0("(", num, ")/(" , denom, ") = ", P_MHAB_conv)) %>%
  dplyr::select(-denom,-num,-P_MHAB_conv) %>%
  rename(P_MHAB = P_MHAB_calc)

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
kable(converted_rel_calc, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'aquamarine') %>%
  column_spec(2, background = 'aquamarine') %>%
  column_spec(3, background = '#FFF0F5') %>%
  scroll_box(width = "100%")

## ----include = FALSE------------------------------------------------
pat_n3_nrmhab_12_no <- patents %>%
  filter(nchar(geo) == 5) %>%  # NUTS-2 values
  filter(unit %in% c("NR", "P_MHAB")) %>%
  filter(time == 2012) %>% # 2012
  filter(str_detect(geo, "^NO")) %>%  # Norway
  dplyr::select(-"time") %>%
  tidyr::pivot_wider(
    id_cols = c("geo"),
    names_from = "unit",
    values_from = "values"
  )

classification <- pat_n3_nrmhab_12_no %>%
  nuts_classify(nuts_code = "geo")
nuts_get_data(classification) %>%
  group_by(from_version) %>%
  tally()

conversion_m_long <- classification[["data"]] %>%
  filter(!is.na(from_version)) %>%
  inner_join(filter(cross_walks, to_version == 2021),
             by = c("from_code", "from_version")) %>%
  mutate(pop18 = round(pop18 / 1000, 2)) %>%
  mutate_at(vars(NR, P_MHAB, pop18), list( ~ as.integer(.))) %>%
  dplyr::select(from_code, to_code, from_version, to_version, NR, P_MHAB, pop18)

convert_rel <- conversion_m_long %>%
  group_by(from_code) %>%
  summarise(pop18 = sum(pop18)) %>%
  ungroup() %>%
  mutate(nuts_3 = from_code,
         nuts_2 = substr(from_code, 1, 4)) %>%
  dplyr::select(nuts_3, nuts_2, pop18) %>%
  inner_join(dplyr::select(pat_n3_nrmhab_12_no, nuts_3 = geo, P_MHAB)) %>%
  mutate_at(vars(P_MHAB, pop18), list( ~ as.integer(.)))

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
kable(convert_rel, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'azure') %>%
  column_spec(2, background = 'aquamarine') %>%
  scroll_box(width = "100%")

## ----echo=FALSE, message = FALSE, warning = FALSE-------------------
converted_rel <- convert_rel %>%
  group_by(nuts_2) %>%
  summarize(P_MHAB_conv = as.integer(sum(P_MHAB * pop18) / sum(pop18))) %>%
  ungroup()

# Illustrate calculation
nuts_2_vec = unique(convert_rel$nuts_2)
nums = list()
denoms = list()
for (i in seq_along(nuts_2_vec)) {
  convert_rel_sub = convert_rel %>%
    filter(nuts_2 %in% nuts_2_vec[i])
  nums[[i]] = paste0(paste0(convert_rel_sub$pop18, " x ", convert_rel_sub$P_MHAB) ,
                     collapse = " + ")
  denoms[[i]] = paste0(convert_rel_sub$pop18 , collapse = " + ")
}

calcs <- data.frame(nuts_2 = nuts_2_vec,
                    denom = unlist(denoms),
                    num = unlist(nums))

convert_rel_calc <- converted_rel %>%
  inner_join(calcs) %>%
  mutate(P_MHAB_calc = paste0("(", num, ")/(" , denom, ") = ", P_MHAB_conv)) %>%
  dplyr::select(-denom,-num,-P_MHAB_conv) %>%
  rename(P_MHAB = P_MHAB_calc)

kable(convert_rel_calc, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, background = 'aquamarine') %>%
  column_spec(2, background = '#FFF0F5') %>%
  scroll_box(width = "100%")

