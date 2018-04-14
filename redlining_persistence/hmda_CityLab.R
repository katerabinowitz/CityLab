require(tidyverse)
require(rgdal)
require(leaflet)
here()

tractHMDA <- read.csv("2_census_tract_loan_figures.csv", stringsAsFactors = FALSE, strip.white = TRUE)

### national data ###
### national data ###
### national data ###
# standardize tract
tractHMDA <-  tractHMDA %>% mutate(TRACTCE = str_pad(gsub("\\.", "",sprintf("%.2f", 
                                                              round(census_tract,2))), 6, pad = 0))

metroSummary <- tractHMDA %>% group_by(msamd_name) %>% summarise(blackPop = sum(black_population), 
                                                                 totalPop=sum(total_population),
                                                                 totalApp = sum(total_applications),
                                                                 totalDen = sum(total_denials),
                                                                 totalLoans = sum(total_loans)) %>%
                                                        mutate(blackPer = blackPop / totalPop, 
                                                               denPer = totalDen / totalApp,
                                                               loanPer = totalLoans / totalApp)

corTable <- tractHMDA %>% filter(!(is.na(black_pop_pct)) & !(is.na(tract_denials_rate))) %>% 
                          group_by(msamd_name) %>%
                          summarise(corr=cor(black_pop_pct, tract_denials_rate), 
                                    apps = sum(total_applications),
                                    blackPop = sum(black_population), 
                                    totalPop = sum(total_population)) %>%
                          filter(apps > 15000) %>% 
                          mutate(blackPer = blackPop / totalPop) %>% arrange(desc(corr))

### duval county ###
### duval county ###
### duval county ###
duvalHMDA <- tractHMDA %>% filter(msamd_name == "JACKSONVILLE, FL")

# msa summary
duvalHMDA %>% group_by(county) %>% 
  summarise(totalPop = sum(total_population),
            allLoans = sum(total_loans),
            blackShareLoan = (sum(black_loans) / allLoans) * 100,
            blackSharePop = (sum(black_population) / sum(total_population))*100) %>%
  arrange(desc(blackSharePop))

duvalHMDA <- duvalHMDA %>% filter(county=="Duval County") 
                          

# add housing units
duvalUnits <- read.csv("jacksonville_housing.csv", stringsAsFactors = FALSE, strip.white = TRUE)
duvalUnits <- duvalUnits %>% mutate(TRACTCE = substr(GEO.id2,6,11)) %>% 
                             select(TRACTCE, estimate) %>% 
                             rename(units = estimate)

duval <-  merge(duvalHMDA, duvalUnits, by="TRACTCE") %>% 
          mutate(loansPerHousing = (total_loans / units)*1000) %>% 
          arrange(desc(black_pop_pct))

duval75 <- duval %>% mutate(black75 = ifelse(black_pop_pct > 75, 1, 0))

duvalCompare <- duval75 %>% group_by(black75) %>% summarise(totalPop = sum(total_population),
                                                            totalLoans = sum(total_loans),
                                                            totalDenials = sum(total_denials),
                                                            totalUnits = sum(units), 
                                                            totalApps = sum(total_applications)) %>%
                                                   mutate(appPerUnit = totalApps / totalUnits,
                                                          denialRate = totalDenials / totalApps,
                                                          loansPerUnit = totalLoans / totalUnits)
# mapping
fl <- readOGR(dsn="/Users/katerabinowitz/Documents/DataLensDCOrg/CityLab:HMDA/fl.geojson", layer="OGRGeoJSON")
duvalGeo <- fl[fl$COUNTYFP=="031",]

duvalGeo@data <- left_join(duvalGeo@data, duval, by="TRACTCE")
writeOGR(duvalGeo, 'duval.geojson','duval', driver='GeoJSON',check_exists = FALSE)

bins <- c(0, 25, 50, 75, 120)
pal <- colorBin("BuPu", bins = bins)

leaflet(data = duvalGeo) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(black_pop_pct), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = duval$TRACTCE) %>% 
  addLegend(pal = pal, values = ~black_pop_pct)

leaflet(data = duvalGeo) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(loansPerHousing), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1)%>% 
  addLegend(pal = pal, values = ~loansPerHousing)

### st. louis ###
### st. louis ###
### st. louis ###
tractHMDA %>% filter(msamd_name == "ST. LOUIS, MO-IL") %>% 
              group_by(county) %>% 
              summarise(totalPop = sum(total_population),
                        allLoans = sum(total_loans),
                        blackShareLoan = (sum(black_loans) / allLoans) * 100,
                        blackSharePop = (sum(black_population) / sum(total_population))*100) %>%
              arrange(desc(blackSharePop))

louisUnits <- read.csv("stlouis_housing.csv", stringsAsFactors = FALSE, strip.white = TRUE)
louisUnits <- louisUnits %>% mutate(TRACTCE = substr(GEO.id2,6,11)) %>% select(TRACTCE, estimate) %>% rename(units = estimate)

stl <- tractHMDA %>% filter(msamd_name == "ST. LOUIS, MO-IL" & county == "St. Louis city") %>%
                     merge(louisUnits, by="TRACTCE") %>%
                     mutate(blackQuant = case_when(black_pop_pct < 26 ~ "0-25%",
                                                   black_pop_pct < 51 ~ "26-50%",
                                                   black_pop_pct < 76 ~ "51-75%",
                                                   black_pop_pct < 101 ~ "76-100%"),
                            TRACTCE = str_pad(gsub("\\.", "",sprintf("%.2f", 
                                                  round(census_tract,2))), 6, pad = 0),
                            blackQuant = factor(blackQuant,levels=c("76-100%", "51-75%", "26-50%", "0-25%"),ordered=TRUE),
                            loansPerHousing = (total_loans / units)*1000)

blackQ <- stl %>% group_by(blackQuant) %>% 
                  summarise(denials = sum(total_denials),
                            loans = sum(total_loans),
                            apps = sum(total_applications),
                            units = sum(units),
                            pop = sum(total_population)) %>%
                  mutate(denial_rate = (denials/apps)*100,
                         appsPerUnit = (apps / units)*1000)
# geodata
missouri = readOGR(dsn="/Users/katerabinowitz/Documents/DataLensDCOrg/CityLab:HMDA/miss.geojson", 
                   layer="OGRGeoJSON")
stLouis <- missouri[missouri$COUNTYFP == "510",]

stLouis@data <- merge(stl, stLouis@data, by="TRACTCE") 

# viz and maps
ggplot(data=blackQ, aes(x=blackQuant, y=appsPerUnit)) +
  geom_bar(stat = "identity", fill = "#71C9CE") +
  theme_bw() +
  coord_flip() +
  theme(axis.ticks=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        legend.position="none") +   
  labs(x="Proportion of Black Residents", y="Mortgage Applications Per 1,000 Housing Units")
ggsave("mortgageApps.pdf", width = 4, height = 2)

ggplot(data=blackQ, aes(x=blackQuant, y=denial_rate)) +
  geom_bar(stat = "identity", fill = "#71C9CE") +
  theme_bw() +
  coord_flip() +
  theme(axis.ticks=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        legend.position="none") +   
  labs(x="Proportion of Black Residents", y="Convential Mortgage Denial Rate (%)")
ggsave("mortgageDenials.pdf", width = 4, height = 2)

bbins <- c(0, 20, 40, 60, 80, 100)
bpal <- colorBin("BuPu", domain = stLouis$black_pop_pct, bins = bbins)
leaflet(data = stLouis) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~bpal(black_pop_pct), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1) %>% 
  addLegend(pal = bpal, values = ~black_pop_pct)

lbins <- c(0, 10, 30, 50, 70, 200)
lpal <- colorBin("BuPu", domain = stLouis$total_loans, bins = bbins)
leaflet(data = stLouis) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~lpal(total_loans), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1)%>% 
  addLegend(pal = lpal, values = ~total_loans)