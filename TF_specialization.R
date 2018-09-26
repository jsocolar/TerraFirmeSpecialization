library(auk)
library(rgdal)
"%ni%" <- Negate("%in%")

##### Extract Loreto observations from eBird basic dataset #####
#setwd("/Users/TingleyLab/Desktop/useful_datasets/eBird/ebd_relFeb-2018")
#f_in <- "ebd_relFeb-2018.txt"
#f_out <- "ebd_Loreto.txt"
#
#Loreto_ebd <- f_in %>%
#  auk_ebd() %>%
#  auk_state(state = "PE-LOR") %>%
#  auk_complete() %>%
#  auk_distance(distance = c(0, 10)) %>%
#  auk_filter(file = f_out) %>%
#  read_ebd
#
#User = "TingleyLab"
#setwd(paste0("/Users/", User, "/Dropbox/Work/TerraFirmeSpecialization/"))

#save(Loreto_ebd, file="Loreto_ebd.Rdata")

##### Load Loreto observations from eBird and create presence-absence dataframe #####
User = "Jacob"
setwd(paste0("/Users/", User, "/Dropbox/Work/TerraFirmeSpecialization/"))
load("Loreto_ebd.Rdata")

Loreto_ebd <- Loreto_ebd[which(Loreto_ebd$approved == T), ]

events <- unique(Loreto_ebd$sampling_event_identifier)
species <- unique(Loreto_ebd$scientific_name)

Loreto_pa <- matrix(data=0, nrow=length(events), ncol=length(species))

# Below should finish in well under a minute
for(i in 1:nrow(Loreto_ebd)){
  Loreto_pa[which(events == Loreto_ebd$sampling_event_identifier[i]), 
            which(species == Loreto_ebd$scientific_name[i])] <- 1
}

Tind <- c("Crypturellus variegatus", "Pharomachrus pavoninus", "Monasa morphoeus", 
          "Capito auratus", "Synallaxis rutilans", "Sclerurus caudacutus", "Myrmoborus myotherinus",
          "Myrmelastes leucostigma", "Formicarius colma", "Myrmothera campanisona", 
          "Liosceles thoracicus", "Schiffornis turdina", "Cnipodectes subbrunneus", 
          "Tunchiornis ochraceiceps")
Vind <- c("Crypturellus undulatus", "Buteogallus schistaceus", "Amazona festiva", 
          "Opisthocomus hoazin", "Bucco tamatia", "Nasica longirostris", "Thamnophilus cryptoleucus",
          "Nyctiprogne leucopyga", "Capito aurovirens", "Cephalopterus ornatus", "Certhiaxis mustelinus",
          "Attila cinnamomeus", "Helicolestes hamatus", "Conirostrum bicolor")
Wind <- c("Crypturellus duidae", "Crypturellus strigulosus", "Notharchus ordii", 
          "Epinecrophylla leucophthalma", "Percnostola arenarum", "Sciaphylax castanea", "Pithys castaneus",
          "Lepidocolaptes duidae", "Zimmerius villarejoi", "Hemitriccus minimus", "Platyrinchus saturatus",
          "Neopipo cinnamomea", "Conopias parvus", "Attila citriniventris", "Xipholena punicea", 
          "Neopelma chrysocephalum", "Xenopipo atronitens", "Heterocercus aurantiivertex",
          "Polioptila clementsi", "Tachyphonus phoenicius")
Rind <- c("Eubucco richardsoni", "Ramphastos vitellinus", "Campephilus melanoleucos",
          "Xiphorhynchus guttatus", "Rhegmatorhina melanosticta", "Tolmomyias poliocephalus", 
          "Attila spadiceus", "Chiroxiphia pareola", "Tangara schrankii", "Psarocolius decumanus",
          "Cacicus oseryi", "Cacicus haemorrhous")

Tbirds <- Loreto_pa[ , which(species %in% Tind)]
Vbirds <- Loreto_pa[ , which(species %in% Vind)]
Wbirds <- Loreto_pa[ , which(species %in% Wind)]
Rbirds <- Loreto_pa[ , which(species %in% Rind)]

Tct <- rowSums(Tbirds)
Vct <- rowSums(Vbirds)
Wct <- rowSums(Wbirds)
Rct <- rowSums(Rbirds)

Tlists <- which(Tct > 2 & Vct == 0)
Vlists <- which(Vct > 2 & Tct == 0)
Wlists <- which(Wct > 2 & Vct == 0 & Rct == 0)
Rlists <- which(Rct > 2 & Tct > 2 & Vct == 0 & Wct == 0)

tf_special <- species[which(colSums(Loreto_pa[Tlists, ]) > 10*colSums(Loreto_pa[Vlists, ]) &
          colSums(Loreto_pa[Tlists, ]) > 9)]

v_special <- species[which(colSums(Loreto_pa[Vlists, ]) > 10*colSums(Loreto_pa[Tlists, ]) &
                colSums(Loreto_pa[Vlists, ]) > 9)]

r_special_lax <- species[which(colSums(Loreto_pa[Rlists, ]) > 5*colSums(Loreto_pa[Wlists, ]) &
                colSums(Loreto_pa[Rlists, ]) > 4)]

w_special <- species[which(colSums(Loreto_pa[Wlists, ]) > 10*colSums(Loreto_pa[Rlists, ]) &
                             colSums(Loreto_pa[Wlists, ]) > 9)]

r_special <- r_special_lax[which(r_special_lax %in% tf_special)]

t_common1 <- species[which(colSums(Loreto_pa[Tlists, ]) > length(Tlists)/8)]
v_common1 <- species[which(colSums(Loreto_pa[Vlists, ]) > length(Vlists)/8)]
t_common2 <- species[which(colSums(Loreto_pa[Tlists, ]) > length(Tlists)/4)]
v_common2 <- species[which(colSums(Loreto_pa[Vlists, ]) > length(Vlists)/4)]
r_common <- species[which(colSums(Loreto_pa[Rlists, ]) > length(Rlists)/4)]
w_common <- species[which(colSums(Loreto_pa[Wlists, ]) > length(Wlists)/4)]

t_common1[which(t_common1 %in% v_common1)]
t_common2[which(t_common2 %in% v_common2)]
r_common[which(r_common %in% w_common)]

clements <- read.csv("Clements-Checklist-v2018-August-2018.csv")
clsp <- clements[which(clements$category=="species"), ]

t_common_list <- t_common2[match(clsp$scientific.name, t_common2)][which(!is.na(t_common2[match(clsp$scientific.name, t_common2)]))]
v_common_list <- v_common2[match(clsp$scientific.name, v_common2)][which(!is.na(v_common2[match(clsp$scientific.name, v_common2)]))]
r_common_list <- r_common[match(clsp$scientific.name, r_common)][which(!is.na(r_common[match(clsp$scientific.name, r_common)]))]
w_common_list <- w_common[match(clsp$scientific.name, w_common)][which(!is.na(w_common[match(clsp$scientific.name, w_common)]))]

write.csv(t_common_list, file = "tf_common.csv")
write.csv(v_common_list, file = "v_common.csv")
write.csv(r_common_list, file = "r_common.csv")
write.csv(w_common_list, file = "w_common.csv")

shared.2 <- t_common1[which(t_common1 %in% v_common1)]
shared2 <- shared.2[match(clsp$scientific.name, shared.2)][which(!is.na(shared.2[match(clsp$scientific.name, shared.2)]))]
write.csv(shared2, file = "shared2.csv")


ee <- events[c(Tlists,Rlists,Wlists,Vlists)]
obs <- vector()
for(i in 1:length(ee)){
  obs[i] <- unique(Loreto_ebd$last_name[which(Loreto_ebd$sampling_event_identifier == ee[i])])
}

length(which(obs=="Socolar"))
length(which(obs=="Saboya del Castillo"))
length(which(obs=="Stotz"))
length(which(obs=="Carnes"))


##### Plotting Figure 1 (map of lists) #####
event_lats <- Loreto_ebd$latitude[match(events, Loreto_ebd$sampling_event_identifier)]
event_lons <- Loreto_ebd$longitude[match(events, Loreto_ebd$sampling_event_identifier)]
event_coord <- data.frame(lon = event_lons, lat = event_lats)
A <- 0.1

jC<- data.frame(lon = jitter(event_lons, amount = A), lat = jitter(event_lats, amount = A))


plot(loreto, border="gray45", main="all", cex.main=4.3, font.main=1)
plot(cRivers, col = "gray80", lwd=2, xlim=c(-77.9,-69.7), add=T)
#plot(peec, add=T, lwd=3, border="gray45")
plot(loreto, add=T, lwd=4, border = "gray45")
points(jC, pch=16, cex = 1, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.3))
text(x=-70.8, y=-8, labels = c(paste0("n = ", length(Tlists))), cex=4)


T_coords <- event_coord[Tlists, ]
V_coords <- event_coord[Vlists, ]
W_coords <- event_coord[Wlists, ]
R_coords <- event_coord[Rlists, ]

jT <- cbind(jitter(T_coords[,1], amount = A), jitter(T_coords[,2], amount = A))
jV <- cbind(jitter(V_coords[,1], amount = A), jitter(V_coords[,2], amount = A))
jR <- cbind(jitter(R_coords[,1], amount = A), jitter(R_coords[,2], amount = A))
jW <- cbind(jitter(W_coords[,1], amount = A), jitter(W_coords[,2], amount = A))

worldborders <- readOGR(dsn = "/Users/Jacob/Dropbox/Work/Map_data/TM_WORLD_BORDERS-0.3")
peru <- worldborders[which(worldborders$ISO3 =="PER"),]
peec <- worldborders[which(worldborders$ISO3 %in% c("PER","ECU", "COL")),]

peru_admin <- readOGR(dsn = "/Users/Jacob/Dropbox/Work/Map_data/PER_adm")
loreto <- peru_admin[which(peru_admin$NAME_1=="Loreto"),]

rivers <- readOGR(dsn = "/Users/Jacob/Dropbox/Work/Map_data/world_rivers_dSe")
cRivers <- raster::crop(rivers, raster::extent(-78, -69.7, -9, 0.2))

dev.off()
pdf("/Users/Jacob/Dropbox/Work/TerraFirmeSpecialization/Figure1_map.pdf", width=25, height=20)

split.screen(t(matrix(data = c(0,.35,.5,1,
                             .35,.7,.5,1,
                             0,.35,0,.5,
                             .35,.7,0,.5,
                             .7,1,.3,.7), nrow = 4)))

screen(5)
plot(peru_admin, lwd=.5, border="gray45")
plot(loreto, col="black", add=T)

screen(1)
plot(loreto, border="gray45", main="terra firme", cex.main=4.3, font.main=1)
plot(cRivers, col = "gray80", lwd=2, xlim=c(-77.9,-69.7), add=T)
#plot(peec, add=T, lwd=3, border="gray45")
plot(loreto, add=T, lwd=4, border = "gray45")
points(jT, pch=16, cex = 2, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.7))
text(x=-70.8, y=-8, labels = c(paste0("n = ", length(Tlists))), cex=4)


screen(2)
plot(loreto, border="gray45", main="varzea", cex.main=4.3, font.main=1)
plot(cRivers, col = "gray80", lwd=2, add=T)
#plot(peec, add=T, lwd=3, border="gray45")
plot(loreto, add=T, lwd=4, border = "gray45")
points(jV, pch=16, cex=2, col=rgb(red=1.0, green=0.2, blue=0.2, alpha=0.7))
text(x=-70.8, y=-8, labels = c(paste0("n = ", length(Vlists))), cex=4)


screen(4)
plot(loreto, border="gray45", main="white-sand", cex.main=4.3, font.main=1)
plot(cRivers, col = "gray80", lwd=2, add=T)
#plot(peec, add=T, lwd=3, border="gray45")
plot(loreto, add=T, lwd=4, border = "gray45")
points(jW, pch=16, cex=2, col=rgb(red=0, green=0.7, blue=0.7, alpha=0.7))
text(x=-70.8, y=-8, labels = c(paste0("n = ", length(Wlists))), cex=4)

screen(3)
plot(loreto, border="gray45", main="clay-soil", cex.main=4.3, font.main=1)
plot(cRivers, col = "gray80", lwd=1, add=T)
#plot(peec, add=T, lwd=2, border="gray45")
plot(loreto, add=T, lwd=3, border = "gray45")
points(jR, pch=16, cex = 2, col=rgb(red=1.0, green=0.5, blue=0.1, alpha=0.7))
text(x=-70.8, y=-8, labels = c(paste0("n = ", length(Rlists))), cex=4)


dev.off()

