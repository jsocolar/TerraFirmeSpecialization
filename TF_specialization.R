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
          "Schisticichla leucostigma", "Formicarius colma", "Myrmothera campanisona", 
          "Liosceles thoracicus", "Schiffornis turdina", "Cnipodectes subbrunneus", 
          "Tunchiornis ochraceiceps")
Vind <- c("Crypturellus_undulatus", "Opisthocomus_hoatzin", "Nyctiprogne_leucopygia", "Capito_aurovirens")