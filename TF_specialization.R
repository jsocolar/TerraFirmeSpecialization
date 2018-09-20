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

User = TingleyLab
setwd(paste0("/Users/", User, "/Dropbox/Work/TerraFirmeSpecialization/"))

save(Loreto_ebd, file="Loreto_ebd.Rdata")


User = Jacob
Tind <- c("Crypturellus_variegatus", "Pharomachrus_pavoninus", "Monasa_morphoeus", 
          "Capito_auratus", "Synallaxis_rutilans", "Sclerurus_caudacutus", "Myrmoborus_myotherinus",
          "Schisticichla_leucostigma", "Formicarius_colma", "Myrmothera_campanisona", 
          "Liosceles_thoracicus", "Schiffornis_turdina", "Cnipodectes_subbrunneus", 
          "Tunchiornis_ochraceiceps")
Vind <- c("Crypturellus_undulatus", "Opisthocomus_hoatzin", "Nyctiprogne_leucopygia", "Capito_aurovirens")