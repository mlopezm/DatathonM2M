source("librarycode.R")
load("dat.ini.Rdat")

list_samplesgood=list()  #lista de samples con todo acieros
list_samplesbad=list()   #lista de samples todo fallos

lfiles= paste0("./sample_gb/",list.files("./sample_gb"))
for (i in seq(length(lfiles))){
  load(file=lfiles[i])
  list_samplesgood=c(list_samplesgood,list(sample_good))
  list_samplesbad=c(list_samplesbad,list(sample_bad))
}


samples_important=Reduce(intersect,  list_samplesgood)  # iccid de resultado +1
samples_noimportant=Reduce(intersect,  list_samplesbad)   #  iccid de resultado -1
rest=c(samples_important,samples_noimportant)
samples_dudosos=outersect(valsims,rest)   #  iccid de resultado 0




#eda
sim.p.import = sim.p
sim.p.import$import="N"
sim.p.import[sim.p.import$iccid %in%samples_important,"import"]="I"
sim.p.import[sim.p.import$iccid %in%samples_dudosos,"import"]="D"

table(sim.p.import$import)

sim.p.import_a=sim.p.import %>% arrange(assigned_rate_plan_id)

#crear tablas de porcentaje de dudosos, importantes, etc por acc y rate plan
tabla1= sim.p.import %>%
  group_by(account_id, import) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

tabla2= sim.p.import %>%
  group_by(assigned_rate_plan_id, import) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
