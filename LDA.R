prep = readRDS(file.path("Modellieren", "Objekte", "docsred.rds"))
vocab = readRDS(file.path("Modellieren", "Objekte", "vocab.rds"))

library(ldaPrototype)

if(FALSE){
  K = 5:75
  n = length(K)
  
  setwd("Modellieren")
  batch = LDABatch(prep, vocab, n = n, K = K, id = "BatchK5bis75", resources = list(walltime = 2*3600, memory = 4*1024))
  saveRDS(batch, file = "BatchK5bis75.rds")
}

if(FALSE){
  setwd("Modellieren")
  for(potK in c(20,30,40,50,60)){
    batch = LDABatch(prep, vocab, K = potK, id = paste0("BatchProto", potK),
                     chunk.size = 10, resources = list(walltime = 8*3600, memory = 4*1024))
    saveRDS(batch, file = paste0("BatchProto", potK, ".rds"))
  }
}

if(FALSE){
  setwd("Modellieren")
  for(potK in c(20,30,40,50,60)){
    batch = readRDS(paste0("BatchProto", potK, ".rds"))
    setFileDir(batch, paste0("BatchProto", potK))
    proto = getPrototype(batch, vocab, pm.backend = "socket", ncpus = 4)
    saveRDS(proto, file = paste0("proto", potK, ".rds"))
  }
}

if(FALSE){
  setwd("ModellierenGriffiths")
  for(potK in c(20,30,40,50,60)){
    batch = LDABatch(prep, vocab, K = potK, alpha = 50/potK, eta = 0.1,
                     id = paste0("BatchProtoGriffiths", potK),
                     chunk.size = 25, resources = list(walltime = 48*3600, memory = 4*1024))
    saveRDS(batch, file = paste0("BatchProtoGriffiths", potK, ".rds"))
  }
}

if(FALSE){
  setwd("ModellierenGriffiths")
  for(potK in c(20,30,40,50,60)){
    batch = readRDS(paste0("BatchProtoGriffiths", potK, ".rds"))
    setFileDir(batch, paste0("BatchProtoGriffiths", potK))
    proto = getPrototype(batch, vocab, pm.backend = "socket", ncpus = 4)
    saveRDS(proto, file = paste0("protogriffiths", potK, ".rds"))
  }
}
