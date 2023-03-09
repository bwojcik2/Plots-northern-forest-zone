
#####################################################
## Deer trends in the north
## plots for NOD pop model mtg
## NORTHERN FOREST - SAKN
## NORTHERN FOREST - density (TA)
## NORTHERN FOREST - A & AL
#####################################################

library(tidyverse)
library(zoo)
library(scales)
library(haven)

####################### NORTHERN FOREST - SAKN #######################

## 2002 - 2022 data
pop = read.csv("C:\\Users\\wojcib\\Documents\\OAS-deer-research\\Population-estimates\\SAK2002-2022_update2023-02-17.csv")
zonekey <- read.csv("zonekey.csv") %>% select(-X)
pop <- merge(pop, zonekey, by = "DMU")

pop2 <- pop %>% 
  filter(DMU != "Madeline Island Forest",
         DMZ == "Northern Forest Zone") %>% 
  group_by(Year, DMZ) %>% 
  summarize(SAKN = sum(SAK, na.rm =T),
            TA = sum(TotalArea, na.rm=T)) %>% 
  mutate(den = SAKN/TA)

## calculate 3 year average
pop3 <- pop2 %>%
  group_by(DMZ) %>% 
  mutate(roll_mean = rollmean(SAKN, 3, na.pad = T), 
         y_max = SAKN*1.2) %>% 
  select(Year, DMZ, SAKN, roll_mean, y_max)

## create plot
png(filename = "NorthernForestSAKN_2002-2022.jpg", width = 10, height = 6, units = "in", res = 600)
par(mar = c(5, 5, 5, 2))

p1 <- ggplot(data = pop3, aes(x = Year, y = SAKN)) + 
  geom_point(size = 3) + 
  geom_line(aes(y = roll_mean), size = 1) + 
  geom_blank(aes(y = y_max)) + # to increase the max of y-axis
  scale_y_continuous(breaks = scales::pretty_breaks(5), 
                     limits = c(0,NA), 
                     labels = label_number(scale = 1/1000)) +
  scale_x_continuous(breaks = seq(2002, 2022, 1),
                     limits = c(2002, 2022)) + 
  xlab("Year") +
  ylab("Posthunt Population (x 1,000)") + 
  ggtitle("Northern Forest Zone Posthunt Population 2002-2022") + 
  theme_bw()

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"),
           axis.text.y = element_text(face = "bold"),
           axis.title = element_text(face="bold"),
           plot.title = element_text(face = "bold"))

legend()
dev.off()

####################### NORTHERN FOREST - density (TA) #######################

## calculate 3 year average
pop4 <- pop2 %>%
  group_by(DMZ) %>% 
  mutate(roll_mean = rollmean(den, 3, na.pad = T), 
         y_max = den*1.2) %>% 
  select(Year, DMZ, den, roll_mean, y_max)

## create plot
png(filename = "NorthernForestDensityTA_2002-2022.jpg", width = 10, height = 6, units = "in", res = 600)
par(mar = c(5, 5, 5, 2))

p2 <- ggplot(data = pop4, aes(x = Year, y = den)) + 
  geom_point(size = 3) + 
  geom_line(aes(y = roll_mean), size = 1) + 
  geom_blank(aes(y = y_max)) + # to increase the max of y-axis
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     limits = c(0,NA)) +
  scale_x_continuous(breaks = seq(2002, 2022, 1),
                     limits = c(2002, 2022)) + 
  xlab("Year") +
  ylab("Density (population / total area)") + 
  ggtitle("Northern Forest Zone Posthunt Population Density 2002-2022") + 
  theme_bw()

p2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"),
           axis.text.y = element_text(face = "bold"),
           axis.title = element_text(face="bold"),
           plot.title = element_text(face = "bold"))

legend()
dev.off()

####################### NORTHERN FOREST - A & AL  #######################

dat <- read_sas("L:/SCISRV/rs434/deer/data/newforest.sas7bdat")
colnames(dat)
# county lookup
CTY = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,41.5,33.5,2.25,2.5,2.75,33.6,51.5,99)
county = c('Adams','Ashland','Barron','Bayfield','Brown','Buffalo','Burnett','Calumet','Chippewa','Clark','Columbia','Crawford','Dane','Dodge','Door','Douglas','Dunn','Eau Claire','Florence','Fond du Lac','Forest','Grant','Green','Green Lake','Iowa','Iron','Jackson','Jefferson','Juneau','Kenosha','Kewaunee','La Crosse','Lafayette','Langlade','Lincoln','Manitowoc','Marathon','Marinette','Marquette','Menominee','Milwaukee','Monroe','Oconto','Oneida','Outagamie','Ozaukee','Pepin','Pierce','Polk','Portage','Price','Racine','Richland','Rock','Rusk','St Croix','Sauk','Sawyer','Shawano','Sheboygan','Taylor','Trempealeau','Vernon','Vilas','Walworth','Washburn','Washington','Waukesha','Waupaca','Waushara','Winnebago','Wood','Unknown','Fort McCoy','Lac Courte Oreilles','Apostle Islands','Bad River','Madeline Island','Lac du Flambeau','Red Cliff','Unknown')
ctylookup = as.data.frame(cbind(CTY, county))
ctylookup = ctylookup %>%
  mutate(CTY = as.numeric(paste(CTY)))

# zone lookup
zone = c("Central Farmland",
         "Central Forest",
         "Northern Forest",
         "Southern Farmland",
         "Unknown")
zone2 = c("Farmland",
          "Forest",
          "Forest",
          "Farmland",
          "Unknown")
zonelookup = as.data.frame(cbind(zone, zone2))

dat2 = dat %>%
  rename("year"="YR",
         "zone"="DMZ") %>%
  filter(year > 2001) %>%
  select(year, CTY, zone, A:XT, CA:CT) %>%
  mutate(CTY = as.numeric(CTY)) %>%
  left_join(ctylookup, by="CTY") %>%
  left_join(zonelookup, by="zone") %>%
  mutate(DMU = ifelse(!county %in% c("Unknown",
                                     "Apostle Islands",
                                     "Bad River",
                                     "Lac Courte Oreilles",
                                     "Lac du Flambeau",
                                     "Fort McCoy",
                                     "Menominee",
                                     "Red Cliff") &
                        zone2 != "Unknown",
                      paste(county, zone2),
                      NA)) %>%
  filter(!is.na(DMU))

check <- dat2 %>% 
  filter(zone == "Northern Forest",
         year == 2022) %>% 
  group_by(year, zone) %>% 
  summarize(A = sum(A, na.rm = TRUE),
            BA = sum(BA, na.rm = TRUE),
            XA = sum(XA, na.rm = TRUE),
            CA = sum(CA, na.rm = TRUE))

nf <- dat2 %>% 
  filter(zone == "Northern Forest") %>% 
  group_by(year, zone) %>% 
  summarize(AntleredHarvest = sum(A, BA, XA, CA, na.rm = TRUE),
            AntlerlessHarvest = sum(AL, BAL, XAL, CAL, na.rm = TRUE))

nf2 <- nf %>% 
  pivot_longer(cols = AntleredHarvest:AntlerlessHarvest,
               names_to = "HarvestType",
               values_to = "Harvest")

## create plot
png(filename = "NorthernForestHarvest_2002-2022.jpg", width = 10, height = 6, units = "in", res = 600)
par(mar = c(5, 5, 5, 2))

# The color blind palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p3 <- ggplot(data = nf2, aes(x = year, y = Harvest, color = HarvestType)) + 
  geom_point(size = 3) + 
  geom_line(size = 1) + 
  # geom_line(aes(y = roll_mean), size = 1) + 
  # geom_blank(aes(y = y_max)) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2002, 2022, 1),
                     limits = c(2002, 2022)) + 
  scale_color_manual(values = cbbPalette,
                     name = "Harvest Type",
                     labels = c("Antlered", "Antlerless")) +
  xlab("Year") +
  ylab("Harvest") + 
  ggtitle("Northern Forest Zone Antlered and Antlerless Harvest 2002-2022") + 
  theme_bw()

p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"),
           axis.text.y = element_text(face = "bold"),
           axis.title = element_text(face="bold"),
           plot.title = element_text(face = "bold"))

legend()
dev.off()

########################
nf <- dat2 %>% 
  filter(zone == "Northern Forest") %>% 
  group_by(year, zone) %>% 
  summarize(Harvest_A = sum(A, BA, XA, CA, na.rm = TRUE),
            Harvest_AL = sum(AL, BAL, XAL, CAL, na.rm = TRUE))

## calculate 3 year average
nf3 <- nf %>%
  group_by(zone) %>% 
  mutate(rollmean_A = rollmean(Harvest_A, 3, na.pad = T), 
         ymax_A = Harvest_A*1.2,
         rollmean_AL = rollmean(Harvest_AL, 3, na.pad = T), 
         ymax_AL = Harvest_AL*1.2) %>% 
  select(year, zone, 
         Harvest_A, rollmean_A, ymax_A,
         Harvest_AL, rollmean_AL, ymax_AL)

nf4 <- nf3 %>% 
  pivot_longer(cols = -c(year, zone),
               names_to = c(".value", "item"),
               names_sep = "_")

## create plot
png(filename = "NorthernForestHarvest_2002-2022_V2.jpg", width = 10, height = 6, units = "in", res = 600)
par(mar = c(5, 5, 5, 2))

# The color blind palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p4 <- ggplot(data = nf4, aes(x = year, y = Harvest, color = item)) + 
  geom_point(size = 3) + 
  geom_line(aes(y = rollmean, color = item), size = 1) + 
  geom_blank(aes(y = ymax)) + # to increase the max of y-axis
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2002, 2022, 1),
                     limits = c(2002, 2022)) + 
  scale_color_manual(values = cbbPalette,
                     name = "Harvest Type",
                     labels = c("Antlered", "Antlerless")) +
  xlab("Year") +
  ylab("Harvest") + 
  ggtitle("Northern Forest Zone Antlered and Antlerless Harvests 2002-2022") + 
  theme_bw()

p4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"),
           axis.text.y = element_text(face = "bold"),
           axis.title = element_text(face="bold"),
           plot.title = element_text(face = "bold"))

legend()
dev.off()
