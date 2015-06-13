
#Sacar la fecha menor y mayor en horas y el numero total de horas de la muestra
ta=sim.u %>%  arrange(record_open_time) 
h1=trunc(strptime(ta$record_open_time[1], "%Y-%m-%d %H:%M:%S"), units="hours") #fecha de inicio de muestras con hora truncada
hn=strptime(ta$record_open_time[nrow(ta)], "%Y-%m-%d %H:%M:%S") #fecha fin de muestras
diff.hour= ceiling(as.numeric(hn-h1,units="hours"))  #diferencia en horas totales de la muestra
n.sims=max(sim.u$iccid)
rm(ta)

valsims=unique(sim.u$iccid)