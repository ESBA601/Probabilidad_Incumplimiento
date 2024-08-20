# AJUSTAR LOS S DE LAS VARIBALES

FECHA3 <- data.frame("FECHA"=c(seq(FECHA0 , by="-1 month", length.out=26,))) %>% arrange(desc(FECHA))
FECHA3$FECHA <- as.character(FECHA3$FECHA,format="%b-%Y")

vm1 <- paste("MES",'',FECHA3[1,1])
vm2 <- paste("MES",'',FECHA3[2,1])
vm3 <- paste("MES",'',FECHA3[3,1])
vm4 <- paste("MES",'',FECHA3[4,1])
vm5 <- paste("MES",'',FECHA3[5,1])
vm6 <- paste("MES",'',FECHA3[6,1])
vm7 <- paste("MES",'',FECHA3[7,1])
vm8 <- paste("MES",'',FECHA3[8,1])
vm9 <- paste("MES",'',FECHA3[9,1]) 
vm10 <- paste("MES",'',FECHA3[10,1])
vm11 <- paste("MES",'',FECHA3[11,1])
vm12 <- paste("MES",'',FECHA3[12,1])
vm13 <- paste("MES",'',FECHA3[13,1]) 
vm14 <- paste("MES",'',FECHA3[14,1]) 
vm15 <- paste("MES",'',FECHA3[15,1]) 
vm16 <- paste("MES",'',FECHA3[16,1]) 
vm17 <- paste("MES",'',FECHA3[17,1]) 
vm18 <- paste("MES",'',FECHA3[18,1]) 
vm19 <- paste("MES",'',FECHA3[19,1]) 
vm20 <- paste("MES",'',FECHA3[20,1]) 
vm21 <- paste("MES",'',FECHA3[21,1]) 
vm22 <- paste("MES",'',FECHA3[22,1]) 
vm23 <- paste("MES",'',FECHA3[23,1]) 
vm24 <- paste("MES",'',FECHA3[24,1]) 
vm25 <- paste("MES",'',FECHA3[25,1]) 
vm26 <- paste("MES",'',FECHA3[26,1]) 

tr1 <- paste("TRANS",'',FECHA3[1,1])
tr2 <- paste("TRANS",'',FECHA3[2,1])
tr3 <- paste("TRANS",'',FECHA3[3,1])
tr4 <- paste("TRANS",'',FECHA3[4,1])
tr5 <- paste("TRANS",'',FECHA3[5,1])
tr6 <- paste("TRANS",'',FECHA3[6,1])
tr7 <- paste("TRANS",'',FECHA3[7,1])
tr8 <- paste("TRANS",'',FECHA3[8,1])
tr9 <- paste("TRANS",'',FECHA3[9,1]) 
tr10 <- paste("TRANS",'',FECHA3[10,1])
tr11 <- paste("TRANS",'',FECHA3[11,1])
tr12 <- paste("TRANS",'',FECHA3[12,1])
tr13 <- paste("TRANS",'',FECHA3[13,1]) 
tr14 <- paste("TRANS",'',FECHA3[14,1]) 
tr15 <- paste("TRANS",'',FECHA3[15,1]) 
tr16 <- paste("TRANS",'',FECHA3[16,1]) 
tr17 <- paste("TRANS",'',FECHA3[17,1]) 
tr18 <- paste("TRANS",'',FECHA3[18,1]) 
tr19 <- paste("TRANS",'',FECHA3[19,1]) 
tr20 <- paste("TRANS",'',FECHA3[20,1]) 
tr21 <- paste("TRANS",'',FECHA3[21,1]) 
tr22 <- paste("TRANS",'',FECHA3[22,1]) 
tr23 <- paste("TRANS",'',FECHA3[23,1]) 
tr24 <- paste("TRANS",'',FECHA3[24,1]) 
tr25 <- paste("TRANS",'',FECHA3[25,1]) 
tr26 <- paste("TRANS",'',FECHA3[26,1]) 

pd1 <- paste("PD",'',FECHA3[1,1])
pd2 <- paste("PD",'',FECHA3[2,1])
pd3 <- paste("PD",'',FECHA3[3,1])
pd4 <- paste("PD",'',FECHA3[4,1])
pd5 <- paste("PD",'',FECHA3[5,1])
pd6 <- paste("PD",'',FECHA3[6,1])
pd7 <- paste("PD",'',FECHA3[7,1])
pd8 <- paste("PD",'',FECHA3[8,1])
pd9 <- paste("PD",'',FECHA3[9,1]) 
pd10 <- paste("PD",'',FECHA3[10,1])
pd11 <- paste("PD",'',FECHA3[11,1])
pd12 <- paste("PD",'',FECHA3[12,1])

PD_DEF1 <- rename(PD_DEF, {{vm26}}:="26_MES", {{vm25}}:="25_MES", {{vm24}}:="24_MES", {{vm23}}:="23_MES", {{vm22}}:="22_MES", {{vm21}}:="21_MES", {{vm20}}:="20_MES", {{vm19}}:="19_MES", {{vm18}}:="18_MES", {{vm17}}:="17_MES", {{vm16}}:="16_MES", {{vm15}}:="15_MES", {{vm14}}:="14_MES", {{vm13}}:="13_MES", {{vm12}}:="12_MES", {{vm11}}:="11_MES", {{vm10}}:="10_MES", {{vm9}}:="9_MES", {{vm8}}:="8_MES", {{vm7}}:="7_MES", {{vm6}}:="6_MES", {{vm5}}:="5_MES", {{vm4}}:="4_MES", {{vm3}}:="3_MES", {{vm2}}:="2_MES", {{vm1}}:="1_MES",
                  {{tr25}}:="M_26_25" ,{{tr24}}:="M_25_24" ,{{tr23}}:="M_24_23" ,{{tr22}}:="M_23_22" ,{{tr21}}:="M_22_21" ,{{tr20}}:="M_21_20" ,{{tr19}}:="M_20_19" ,{{tr18}}:="M_19_18" ,{{tr17}}:="M_18_17" ,{{tr16}}:="M_17_16" ,{{tr15}}:="M_16_15" ,{{tr14}}:="M_15_14" ,{{tr13}}:="M_14_13" ,{{tr12}}:="M_13_12" ,{{tr11}}:="M_12_11" ,{{tr10}}:="M_11_10" ,{{tr9}}:="M_10_9" ,{{tr8}}:="M_9_8" ,{{tr7}}:="M_8_7" ,{{tr6}}:="M_7_6" ,{{tr5}}:="M_6_5" ,{{tr4}}:="M_5_4" ,{{tr3}}:="M_4_3" ,{{tr2}}:="M_3_2" ,{{tr1}}:="M_2_1", 
                  {{pd12}}:="PD_12", {{pd11}}:="PD_11", {{pd10}}:="PD_10", {{pd9}}:="PD_9", {{pd8}}:="PD_8", {{pd7}}:="PD_7", {{pd6}}:="PD_6", {{pd5}}:="PD_5", {{pd4}}:="PD_4", {{pd3}}:="PD_3", {{pd2}}:="PD_2", {{pd1}}:="PD_1" 
)

# CREACION DE LA BASE DE CREDITO PARA EL MES ACTUAL 

ESTADOS1 <- select(PD_DEF, ID, NOMBRE_CLIENTE) %>% left_join(risk_cli,by="ID") %>%
  left_join(risk_fin, by="ID") %>% left_join(risk_pro, by="ID") %>%
  mutate(ESTADOS=ifelse (PD_DEF$ID %in% CASTIGO_NEW$ID , "Castigo", 
                         ifelse (PD_DEF$ID %in% EST_2$ID, "Nuevo", 
                                 ifelse (PD_DEF$ID %in% bef_can$ID, "Cancelado",
                                         ifelse (MAX_FDP==2 & VIGENTE>0, "Vencido",
                                                 ifelse (PRORROGAS>0 & VIGENTE>0, "Prorroga",
                                                         ifelse(LITIGIO>0,"Litigio",
                                                                ifelse(VENCIDO>0,"Vencido",
                                                                       ifelse(REESTRUCTURADO>0,"Reestructurado",
                                                                              ifelse(VIGENTE>0,"Vigente",
                                                                                     "Sin Riesgo"))))))))))

ESTADOS2 <- select(ESTADOS1, ID, NOMBRE_CLIENTE, VIGENTE, REESTRUCTURADO, VENCIDO, LITIGIO, TOTAL, ESTADOS)

ESTADOS2$ESTADOS <- ifelse(is.na(ESTADOS2$ESTADOS),"Sin Riesgo",ESTADOS2$ESTADOS)

# AJUSTAR LOS TITULOS PARA COLOCAR LAS FECHAS A ESTADOS Y SALDOS

est <- paste("Estado en ",FECHA3[1,1])
tot <- paste("Saldo de Capital en",'',FECHA3[1,1])
vig <- paste("Vigente",'',FECHA3[1,1])
ven <- paste("Vencido",'',FECHA3[1,1])
res <- paste("Reestructurado",'',FECHA3[1,1])
lit <- paste("Litigio",'',FECHA3[1,1])

ESTADOS_FIN <- rename(ESTADOS2, {{est}}:="ESTADOS", {{tot}}:="TOTAL", {{vig}}:="VIGENTE", {{res}}:="REESTRUCTURADO", {{ven}}:="VENCIDO", {{lit}}:="LITIGIO")

# CAMBIAR LOS NOMBRES DE LA TABLA DE CALCULOS

CALCULOS <- rename(HIST_MATR, '0-0'="CC", '0-1'="CU", '1-1'="UU", '1-2'="UD", '2-1'="DU", '2-3'="DT", '3-3'="TT", '1-0'="UC", '2-2'="DD", 
                   "Cumplen mes n-1"="CU1", "Incumplen mes n-1"="IN1", "Probabilidad de Incumplimiento (PI)"="PI", "Probabilidad de Permacer vencido (PPV)"="PPV",
                   "Producto (PI-CU1)"="PVIG", "Producto (IN1-PPV)"="PVEN", "Castigo Rezagado"="AA") %>% select(-1:-9)

# CAMBIAR LOS NOMBRES DE LOS TABLA DE PESOS

PESOS <- rename(HIS_MV, {{pd12}}:="MES_12", {{pd11}}:="MES_11", {{pd10}}:="MES_10", {{pd9}}:="MES_9", {{pd8}}:="MES_8", {{pd7}}:="MES_7", {{pd6}}:="MES_6", {{pd5}}:="MES_5", {{pd4}}:="MES_4", {{pd3}}:="MES_3", {{pd2}}:="MES_2", {{pd1}}:="MES_1")

# AJUSTAR LA INFORMACION QUE SE UTILIZARA PARA LOS SIGUIENTES MESES

ESTADOS_FINAL <- select(EST_5, -CLIENTES, -`26_MES`) %>% rename("NOMBRE"="NOMBRE_CLIENTE")

# ARCHIVO PARA GUARADAR

write.xlsx(PD_DEF1, paste('Experiencia Pago ',name0,'.xlsx'))

write.xlsx(CALCULOS, paste('Calculos',name0,'.xlsx'))

write.xlsx(PESOS, paste('Pesos',name0,'.xlsx'))

write.xlsx(risk_end, paste('Vector Final',name0,'.xlsx'))

write.xlsx(risk_max, paste('Vector con Motivo',name0,'.xlsx'))

write.xlsx(ESTADOS_FIN, paste('Estado Cartera de Credito',name0,'.xlsx'))
