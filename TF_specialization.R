##### Extract Loreto observations from eBird basic dataset #####
library(auk)
setwd("/Users/TingleyLab/Desktop/useful_datasets/eBird/ebd_relFeb-2018")
f_in <- "ebd_relFeb-2018.txt"
f_out <- "ebd_Loreto.txt"

Loreto_ebd <- f_in %>%
  auk_ebd() %>%
  auk_state(state = "PE-LOR") %>%
  auk_complete() %>%
  auk_distance(distance = c(0, 10)) %>%
  auk_filter(file = f_out) %>%
  read_ebd

User = "TingleyLab"
setwd(paste0("/Users/", User, "/Dropbox/Work/TerraFirmeSpecialization/"))

save(Loreto_ebd, file="Loreto_ebd.Rdata")

##### Load Loreto observations from eBird and create presence-absence dataframe #####
"%ni%" <- Negate("%in%")
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
Rind <- c("Capito auratus", "Eubucco richardsoni", "Ramphastos vitellinus", "Campephilus melanoleucos",
          "Xiphorhynchus guttatus", "Rhegmatorhina melanosticta", "Tolmomyias poliocephalus", 
          "Attila spadiceus", "Chiroxiphia pareola", "Tangara schrankii", "Psarocolius decumanus",
          "Cacicus oseryi", "Cacicus haemorrhous")
Rind <- c("Eubucco richardsoni", "Campephilus melanoleucos",
          "Rhegmatorhina melanosticta", "Tolmomyias poliocephalus", 
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
Rlists <- which(Rct > 2 & Vct == 0 & Wct == 0)

tf_special <- species[which(colSums(Loreto_pa[Tlists, ]) > 10*colSums(Loreto_pa[Vlists, ]) &
          colSums(Loreto_pa[Tlists, ]) > 8)]

v_special <- species[which(colSums(Loreto_pa[Vlists, ]) > 10*colSums(Loreto_pa[Tlists, ]) &
                colSums(Loreto_pa[Vlists, ]) > 8)]

r_special <- species[which(colSums(Loreto_pa[Rlists, ]) > 5*colSums(Loreto_pa[Wlists, ]) &
                colSums(Loreto_pa[Rlists, ]) > 4)]

r_special[which(r_special %in% tf_special)]
