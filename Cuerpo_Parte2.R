
# SELECCIONAR LOS CLIENTES DE LOS ESTADOS Y VECTOR PRESENTE

EST_1 <- select(ESTADOS, ID, NOMBRE)

# CLIENTES AGREGAR LOS NUEVOS CLIENTES 

EST_2 <- anti_join(risk_nom,  EST_1,  by="ID")

EST_2$`26_MES` <-"0"
EST_2$`25_MES` <-"0"
EST_2$`24_MES` <-"0"
EST_2$`23_MES` <-"0"
EST_2$`22_MES` <-"0"
EST_2$`21_MES` <-"0"
EST_2$`20_MES` <-"0"
EST_2$`19_MES` <-"0"
EST_2$`18_MES` <-"0"
EST_2$`17_MES` <-"0"
EST_2$`16_MES` <-"0"
EST_2$`15_MES` <-"0"
EST_2$`14_MES` <-"0"
EST_2$`13_MES` <-"0"
EST_2$`12_MES` <-"0"
EST_2$`11_MES` <-"0"
EST_2$`10_MES` <-"0"
EST_2$`9_MES` <-"0"
EST_2$`8_MES` <-"0"
EST_2$`7_MES` <-"0"
EST_2$`6_MES` <-"0"
EST_2$`5_MES` <-"0"
EST_2$`4_MES` <-"0"
EST_2$`3_MES` <-"0"
EST_2$`2_MES` <-"0"

# RENOMBRAR LOS ESTADOS NUEVOS PARA ACTUALIZAR EL MES ACTUAL

EST_3 <- rename(ESTADOS, c("NOMBRE_CLIENTE"="NOMBRE","26_MES" = "25_MES","25_MES" = "24_MES",
                           "24_MES" = "23_MES","23_MES" = "22_MES",
                           "22_MES" = "21_MES","21_MES" = "20_MES",
                           "20_MES" = "19_MES","19_MES" = "18_MES",
                           "18_MES" = "17_MES","17_MES" = "16_MES",
                           "16_MES" = "15_MES","15_MES" = "14_MES",
                           "14_MES" = "13_MES","13_MES" = "12_MES",
                           "12_MES" = "11_MES","11_MES" = "10_MES",
                           "10_MES" = "9_MES","9_MES" = "8_MES",
                           "8_MES" = "7_MES","7_MES" = "6_MES",
                           "6_MES" = "5_MES","5_MES" = "4_MES",
                           "4_MES" = "3_MES","3_MES" = "2_MES",
                           "2_MES" = "1_MES",))

# UNIR LOS CLIENTES NUEVOS AL HISTORICO DE ESTADOS

EST_4 <- rbind(EST_3, EST_2) 

# IDENTIFICAR LOS CLIENTES CASTIGADOS

CAST1 <- select(EST_3, ID, `2_MES`)
CAST2 <- filter(CAST1, `2_MES`=="3") %>% rename("1_MES"="2_MES")

# PRIMERO UNIR LOS CASTIGOS CON EL VECTOR DEL MES PRESNTE

VEC1 <- select(risk_end, ID, VEC) %>% rename("1_MES"="VEC")
VEC2 <- rbind(VEC1, CAST2)
VEC3 <- group_by(VEC2, ID) %>% summarise("1_MES"= max(`1_MES`))

# UNIR EL NUEVO VECTOR PRESENTE

EST_5 <- left_join(EST_4, VEC3, by="ID") %>% mutate (CLIENTES = 1)
EST_5$`1_MES` <- ifelse(is.na(EST_5$`1_MES`),"0", EST_5$`1_MES`)


# REALIZAR EL CONTEO POR CADA MES Y RENOMBRA LOS TITULOS

cnt_26 <- group_by(EST_5, `26_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "26_MES", TOTAL_26 = TOTAL)

cnt_25 <- group_by(EST_5, `25_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "25_MES", TOTAL_25 = TOTAL)

cnt_24 <- group_by(EST_5, `24_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "24_MES", TOTAL_24 = TOTAL)

cnt_23 <- group_by(EST_5, `23_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "23_MES", TOTAL_23 = TOTAL)

cnt_22 <- group_by(EST_5, `22_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "22_MES", TOTAL_22 = TOTAL)

cnt_21 <- group_by(EST_5, `21_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "21_MES", TOTAL_21 = TOTAL)

cnt_20 <- group_by(EST_5, `20_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "20_MES", TOTAL_20 = TOTAL)

cnt_19 <- group_by(EST_5, `19_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "19_MES", TOTAL_19 = TOTAL)

cnt_18 <- group_by(EST_5, `18_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "18_MES", TOTAL_18 = TOTAL)

cnt_17 <- group_by(EST_5, `17_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "17_MES", TOTAL_17 = TOTAL)

cnt_16 <- group_by(EST_5, `16_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "16_MES", TOTAL_16 = TOTAL)

cnt_15 <- group_by(EST_5, `15_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "15_MES", TOTAL_15 = TOTAL)

cnt_14 <- group_by(EST_5, `14_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "14_MES", TOTAL_14 = TOTAL)

cnt_13 <- group_by(EST_5, `13_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "13_MES", TOTAL_13 = TOTAL)  

cnt_12 <- group_by(EST_5, `12_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "12_MES", TOTAL_12 = TOTAL)

cnt_11 <- group_by(EST_5, `11_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "11_MES", TOTAL_11 = TOTAL)

cnt_10 <- group_by(EST_5, `10_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "10_MES", TOTAL_10 = TOTAL)

cnt_9 <- group_by(EST_5, `9_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "9_MES", TOTAL_9 = TOTAL)

cnt_8 <- group_by(EST_5, `8_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "8_MES", TOTAL_8 = TOTAL)

cnt_7 <- group_by(EST_5, `7_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "7_MES", TOTAL_7 = TOTAL)

cnt_6 <- group_by(EST_5, `6_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "6_MES", TOTAL_6 = TOTAL)

cnt_5 <- group_by(EST_5, `5_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "5_MES", TOTAL_5 = TOTAL)

cnt_4 <- group_by(EST_5, `4_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "4_MES", TOTAL_4 = TOTAL)

cnt_3 <- group_by(EST_5, `3_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "3_MES", TOTAL_3 = TOTAL)

cnt_2 <- group_by(EST_5, `2_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "2_MES", TOTAL_2 = TOTAL)

cnt_1 <- group_by(EST_5, `1_MES`) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( VECTOR = "1_MES", TOTAL_1 = TOTAL)

# REALIZAR LA UNION PARA TODOS LOS MESES

HISTORICO <- left_join(cnt_26, cnt_25, by = "VECTOR" ) %>% 
  left_join(cnt_24, by = "VECTOR") %>%
  left_join(cnt_23, by = "VECTOR") %>% 
  left_join(cnt_22, by = "VECTOR") %>% 
  left_join(cnt_21, by = "VECTOR") %>% 
  left_join(cnt_20, by = "VECTOR") %>% 
  left_join(cnt_19, by = "VECTOR") %>% 
  left_join(cnt_18, by = "VECTOR") %>% 
  left_join(cnt_17, by = "VECTOR") %>% 
  left_join(cnt_16, by = "VECTOR") %>% 
  left_join(cnt_15, by = "VECTOR") %>% 
  left_join(cnt_14, by = "VECTOR") %>%
  left_join(cnt_13, by = "VECTOR") %>% 
  left_join(cnt_12, by = "VECTOR") %>%
  left_join(cnt_11, by = "VECTOR") %>% 
  left_join(cnt_10, by = "VECTOR") %>% 
  left_join(cnt_9, by = "VECTOR") %>% 
  left_join(cnt_8, by = "VECTOR") %>% 
  left_join(cnt_7, by = "VECTOR") %>% 
  left_join(cnt_6, by = "VECTOR") %>% 
  left_join(cnt_5, by = "VECTOR") %>% 
  left_join(cnt_4, by = "VECTOR") %>% 
  left_join(cnt_3, by = "VECTOR") %>% 
  left_join(cnt_2, by = "VECTOR") %>%
  left_join(cnt_1, by = "VECTOR")

# REALIZAR LAS MATRICES DE TRANSICION 

ESTMAT <- mutate(EST_5, M_26_25 = str_c( `26_MES`,`25_MES`, sep ="-"),
                 M_25_24 = str_c( `25_MES`,`24_MES`, sep ="-"), M_24_23 = str_c( `24_MES`,`23_MES`, sep ="-"),
                 M_23_22 = str_c( `23_MES`,`22_MES`, sep ="-"), M_22_21 = str_c( `22_MES`,`21_MES`, sep ="-"),
                 M_21_20 = str_c( `21_MES`,`20_MES`, sep ="-"), M_20_19 = str_c( `20_MES`,`19_MES`, sep ="-"),
                 M_19_18 = str_c( `19_MES`,`18_MES`, sep ="-"), M_18_17 = str_c( `18_MES`,`17_MES`, sep ="-"),
                 M_17_16 = str_c( `17_MES`,`16_MES`, sep ="-"), M_16_15 = str_c( `16_MES`,`15_MES`, sep ="-"),
                 M_15_14 = str_c( `15_MES`,`14_MES`, sep ="-"), M_14_13 = str_c( `14_MES`,`13_MES`, sep ="-"),
                 M_13_12 = str_c( `13_MES`,`12_MES`, sep ="-"), M_12_11 = str_c( `12_MES`,`11_MES`, sep ="-"),
                 M_11_10 = str_c( `11_MES`,`10_MES`, sep ="-"), M_10_9 = str_c( `10_MES`,`9_MES`, sep ="-"),
                 M_9_8 = str_c( `9_MES`,`8_MES`, sep ="-"), M_8_7 = str_c( `8_MES`,`7_MES`, sep ="-"),
                 M_7_6 = str_c( `7_MES`,`6_MES`, sep ="-"), M_6_5 = str_c( `6_MES`,`5_MES`, sep ="-"),
                 M_5_4 = str_c( `5_MES`,`4_MES`, sep ="-"), M_4_3 = str_c( `4_MES`,`3_MES`, sep ="-"),
                 M_3_2 = str_c( `3_MES`,`2_MES`, sep ="-"), M_2_1 = str_c( `2_MES`,`1_MES`, sep ="-"))

# REALIZAR EL CONTEO POR CADA MES Y RENOMBRA LOS TITULOS PERO CON LAS MATRICES DE TRANCION

cm_26_25 <- group_by(ESTMAT, M_26_25) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_26_25, TOTAL_26 = TOTAL)

cm_25_24 <- group_by(ESTMAT, M_25_24) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_25_24, TOTAL_25 = TOTAL)

cm_24_23 <- group_by(ESTMAT, M_24_23) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_24_23, TOTAL_24 = TOTAL)

cm_23_22 <- group_by(ESTMAT, M_23_22) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_23_22, TOTAL_23 = TOTAL)

cm_22_21 <- group_by(ESTMAT, M_22_21) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_22_21, TOTAL_22 = TOTAL)

cm_21_20 <- group_by(ESTMAT, M_21_20) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_21_20, TOTAL_21 = TOTAL)

cm_20_19 <- group_by(ESTMAT, M_20_19) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_20_19, TOTAL_20 = TOTAL)

cm_19_18 <- group_by(ESTMAT, M_19_18) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_19_18, TOTAL_19 = TOTAL)

cm_18_17 <- group_by(ESTMAT, M_18_17) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_18_17, TOTAL_18 = TOTAL)

cm_17_16 <- group_by(ESTMAT, M_17_16) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_17_16, TOTAL_17 = TOTAL)

cm_16_15 <- group_by(ESTMAT, M_16_15) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_16_15, TOTAL_16 = TOTAL)

cm_15_14 <- group_by(ESTMAT, M_15_14) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_15_14, TOTAL_15 = TOTAL)

cm_14_13 <- group_by(ESTMAT, M_14_13) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_14_13, TOTAL_14 = TOTAL)

cm_13_12 <- group_by(ESTMAT, M_13_12) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_13_12, TOTAL_13 = TOTAL)

cm_12_11 <- group_by(ESTMAT, M_12_11) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_12_11, TOTAL_12 = TOTAL)

cm_11_10 <- group_by(ESTMAT, M_11_10) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_11_10, TOTAL_11 = TOTAL)

cm_10_9 <- group_by(ESTMAT, M_10_9) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_10_9, TOTAL_10 = TOTAL)

cm_9_8 <- group_by(ESTMAT, M_9_8) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_9_8, TOTAL_9 = TOTAL)

cm_8_7 <- group_by(ESTMAT, M_8_7) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_8_7, TOTAL_8 = TOTAL)

cm_7_6 <- group_by(ESTMAT, M_7_6) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_7_6, TOTAL_7 = TOTAL)

cm_6_5 <- group_by(ESTMAT, M_6_5) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_6_5, TOTAL_6 = TOTAL)

cm_5_4 <- group_by(ESTMAT, M_5_4) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_5_4, TOTAL_5 = TOTAL)

cm_4_3 <- group_by(ESTMAT, M_4_3) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_4_3, TOTAL_4 = TOTAL)

cm_3_2 <- group_by(ESTMAT, M_3_2) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_3_2, TOTAL_3 = TOTAL)

cm_2_1 <- group_by(ESTMAT, M_2_1) %>% summarise( TOTAL = sum(CLIENTES)) %>% rename( MATRIZ = M_2_1, TOTAL_2 = TOTAL)

# CREAR LA LISTA DE VECTORES

MATRIZ <- data.frame ("MATRIZ"=c("0-0","0-1","1-0","1-1","1-2","2-1","2-2","2-3","3-3"))

# REALIZAR LA UNION PARA TODOS LOS MESES POR MATRIZ

HIS_MATRIZ <- left_join ( MATRIZ, cm_26_25, by = "MATRIZ" ) %>%
  left_join ( cm_25_24, by = "MATRIZ" ) %>%
  left_join ( cm_24_23, by = "MATRIZ" ) %>%
  left_join ( cm_23_22, by = "MATRIZ" ) %>%
  left_join ( cm_22_21, by = "MATRIZ" ) %>%
  left_join ( cm_21_20, by = "MATRIZ" ) %>%
  left_join ( cm_20_19, by = "MATRIZ" ) %>%
  left_join ( cm_19_18, by = "MATRIZ" ) %>%
  left_join ( cm_18_17, by = "MATRIZ" ) %>%
  left_join ( cm_17_16, by = "MATRIZ" ) %>%
  left_join ( cm_16_15, by = "MATRIZ" ) %>%
  left_join ( cm_15_14, by = "MATRIZ" ) %>%
  left_join ( cm_14_13, by = "MATRIZ" ) %>%
  left_join ( cm_13_12, by = "MATRIZ" ) %>%
  left_join ( cm_12_11, by = "MATRIZ" ) %>%
  left_join ( cm_11_10, by = "MATRIZ" ) %>%
  left_join ( cm_10_9, by = "MATRIZ" ) %>%
  left_join ( cm_9_8, by = "MATRIZ" ) %>%
  left_join ( cm_8_7, by = "MATRIZ" ) %>%
  left_join ( cm_7_6, by = "MATRIZ" ) %>%
  left_join ( cm_6_5, by = "MATRIZ" ) %>%
  left_join ( cm_5_4, by = "MATRIZ" ) %>%
  left_join ( cm_4_3, by = "MATRIZ" ) %>%
  left_join ( cm_3_2, by = "MATRIZ" ) %>%
  left_join ( cm_2_1, by = "MATRIZ" ) 

# TRASPONER LA MATRIZ 
# CREO UN DATA FRAME PARA LOS MESES 
# CREO NUEVAS COLUMNAS Y DESPARECER LOS VALORES 0

HIS_MAT_T <- as.data.frame(t(HIS_MATRIZ[-1]))

# Cambiar por esta funcion

FECHA <- data.frame("FECHA"=c(seq(FECHA0 , by="-1 month", length.out=25,))) %>% arrange(FECHA)

#FECHA <- data.frame("FECHA"=c("TOTAL_26","TOTAL_25","TOTAL_24","TOTAL_23","TOTAL_22","TOTAL_21","TOTAL_20",
#                              "TOTAL_19","TOTAL_18","TOTAL_17","TOTAL_16","TOTAL_15","TOTAL_14","TOTAL_13","TOTAL_12","TOTAL_11","TOTAL_10",
#                              "TOTAL_9","TOTAL_8","TOTAL_7","TOTAL_6","TOTAL_5","TOTAL_4","TOTAL_3","TOTAL_2"))

HIS_MAT_T<- cbind (HIS_MAT_T,FECHA)

HIS_MAT_T$CC<- ifelse(is.na(HIS_MAT_T$V1),0,HIS_MAT_T$V1)
HIS_MAT_T$CU<- ifelse(is.na(HIS_MAT_T$V2),0,HIS_MAT_T$V2)
HIS_MAT_T$UC<- ifelse(is.na(HIS_MAT_T$V3),0,HIS_MAT_T$V3)
HIS_MAT_T$UU<- ifelse(is.na(HIS_MAT_T$V4),0,HIS_MAT_T$V4)
HIS_MAT_T$UD<- ifelse(is.na(HIS_MAT_T$V5),0,HIS_MAT_T$V5)
HIS_MAT_T$DU<- ifelse(is.na(HIS_MAT_T$V6),0,HIS_MAT_T$V6)
HIS_MAT_T$DD<- ifelse(is.na(HIS_MAT_T$V7),0,HIS_MAT_T$V7)
HIS_MAT_T$DT<- ifelse(is.na(HIS_MAT_T$V8),0,HIS_MAT_T$V8)
HIS_MAT_T$TT<- ifelse(is.na(HIS_MAT_T$V9),0,HIS_MAT_T$V9)

# CREAR NUEVAS VARIABLES EL CASTIGO REZAGADO, LOS PROBABILIDADES 

HIST_MATR<- mutate(HIS_MAT_T ,AA=(HIS_MAT_T$TT-lag(HIS_MAT_T$TT,1)), 
                   CU1=UC+UU+UD , IN1=DU+DD+DT+AA, 
                   PI=UD/CU1 , PPV=(DD+DT)/IN1,
                   PVIG=CU1*PI , PVEN=PPV*IN1 )

# CREAR LA MAXIMO VEROSIMILITUD POR MES

SP_12 <- data.frame("MES_12"=colSums(HIST_MATR[2:14,25:26]))
SE_12 <- data.frame("MES_12"=colSums(HIST_MATR[2:14,21:22]))
MV_12 <- data.frame("MES_12"=SP_12$MES_12/SE_12$MES_12)

SP_11 <- data.frame("MES_11"=colSums(HIST_MATR[3:15,25:26]))
SE_11 <- data.frame("MES_11"=colSums(HIST_MATR[3:15,21:22]))
MV_11 <- data.frame("MES_11"=SP_11$MES_11/SE_11$MES_11)

SP_10 <- data.frame("MES_10"=colSums(HIST_MATR[4:16,25:26]))
SE_10 <- data.frame("MES_10"=colSums(HIST_MATR[4:16,21:22]))
MV_10 <- data.frame("MES_10"=SP_10$MES_10/SE_10$MES_10)

SP_9 <- data.frame("MES_9"=colSums(HIST_MATR[5:17,25:26]))
SE_9 <- data.frame("MES_9"=colSums(HIST_MATR[5:17,21:22]))
MV_9 <- data.frame("MES_9"=SP_9$MES_9/SE_9$MES_9)

SP_8 <- data.frame("MES_8"=colSums(HIST_MATR[6:18,25:26]))
SE_8 <- data.frame("MES_8"=colSums(HIST_MATR[6:18,21:22]))
MV_8 <- data.frame("MES_8"=SP_8$MES_8/SE_8$MES_8)

SP_7 <- data.frame("MES_7"=colSums(HIST_MATR[7:19,25:26]))
SE_7 <- data.frame("MES_7"=colSums(HIST_MATR[7:19,21:22]))
MV_7 <- data.frame("MES_7"=SP_7$MES_7/SE_7$MES_7)

SP_6 <- data.frame("MES_6"=colSums(HIST_MATR[8:20,25:26]))
SE_6 <- data.frame("MES_6"=colSums(HIST_MATR[8:20,21:22]))
MV_6 <- data.frame("MES_6"=SP_6$MES_6/SE_6$MES_6)

SP_5 <- data.frame("MES_5"=colSums(HIST_MATR[9:21,25:26]))
SE_5 <- data.frame("MES_5"=colSums(HIST_MATR[9:21,21:22]))
MV_5 <- data.frame("MES_5"=SP_5$MES_5/SE_5$MES_5)

SP_4 <- data.frame("MES_4"=colSums(HIST_MATR[10:22,25:26]))
SE_4 <- data.frame("MES_4"=colSums(HIST_MATR[10:22,21:22]))
MV_4 <- data.frame("MES_4"=SP_4$MES_4/SE_4$MES_4)

SP_3 <- data.frame("MES_3"=colSums(HIST_MATR[11:23,25:26]))
SE_3 <- data.frame("MES_3"=colSums(HIST_MATR[11:23,21:22]))
MV_3 <- data.frame("MES_3"=SP_3$MES_3/SE_3$MES_3)

SP_2 <- data.frame("MES_2"=colSums(HIST_MATR[12:24,25:26]))
SE_2 <- data.frame("MES_2"=colSums(HIST_MATR[12:24,21:22]))
MV_2 <- data.frame("MES_2"=SP_2$MES_2/SE_2$MES_2)

SP_1 <- data.frame("MES_1"=colSums(HIST_MATR[13:25,25:26]))
SE_1 <- data.frame("MES_1"=colSums(HIST_MATR[13:25,21:22]))
MV_1 <- data.frame("MES_1"=SP_1$MES_1/SE_1$MES_1)

# REALIZAR UNA UNION DE TODAS LAS TABLAS

HIS_MV <- cbind (MV_12,MV_11,MV_10,MV_9,MV_8,MV_7,MV_6,MV_5,MV_4,MV_3,MV_2,MV_1)

PIMV <- HIS_MV[c(1),]

PPVMV <- HIS_MV[c(2),]

# APLICAR EL CONDICIONAL DE LA EXPERIENCIA DE PAGO PARA CADA MES

EXP_MES <- mutate(ESTMAT, 
                  PD_1 = ifelse(ESTMAT$M_2_1=="1-1",PIMV$MES_1,
                                ifelse(ESTMAT$M_2_1=="0-1",PIMV$MES_1,
                                       ifelse(ESTMAT$M_2_1=="2-1",PIMV$MES_1,
                                              ifelse(ESTMAT$M_2_1=="2-2",PPVMV$MES_1,
                                                     ifelse(ESTMAT$M_2_1=="1-2",PPVMV$MES_1,
                                                            ifelse(ESTMAT$M_2_1=="2-3",1,
                                                                   ifelse(ESTMAT$M_2_1=="3-3",1,
                                                                          0))))))),
                  PD_2 = ifelse(ESTMAT$M_3_2=="1-1",PIMV$MES_2,
                                ifelse(ESTMAT$M_3_2=="0-1",PIMV$MES_2,
                                       ifelse(ESTMAT$M_3_2=="2-1",PIMV$MES_2,
                                              ifelse(ESTMAT$M_3_2=="2-2",PPVMV$MES_2,
                                                     ifelse(ESTMAT$M_3_2=="1-2",PPVMV$MES_2,
                                                            ifelse(ESTMAT$M_3_2=="2-3",1,
                                                                   ifelse(ESTMAT$M_3_2=="3-3",1,
                                                                          0))))))),
                  PD_3 = ifelse(ESTMAT$M_4_3=="1-1",PIMV$MES_3,
                                ifelse(ESTMAT$M_4_3=="0-1",PIMV$MES_3,
                                       ifelse(ESTMAT$M_4_3=="2-1",PIMV$MES_3,
                                              ifelse(ESTMAT$M_4_3=="2-2",PPVMV$MES_3,
                                                     ifelse(ESTMAT$M_4_3=="1-2",PPVMV$MES_3,
                                                            ifelse(ESTMAT$M_4_3=="2-3",1,
                                                                   ifelse(ESTMAT$M_4_3=="3-3",1,
                                                                          0))))))),
                  PD_4 = ifelse(ESTMAT$M_5_4=="1-1",PIMV$MES_4,
                                ifelse(ESTMAT$M_5_4=="0-1",PIMV$MES_4,
                                       ifelse(ESTMAT$M_5_4=="2-1",PIMV$MES_4,
                                              ifelse(ESTMAT$M_5_4=="2-2",PPVMV$MES_4,
                                                     ifelse(ESTMAT$M_5_4=="1-2",PPVMV$MES_4,
                                                            ifelse(ESTMAT$M_5_4=="2-3",1,
                                                                   ifelse(ESTMAT$M_5_4=="3-3",1,
                                                                          0))))))),
                  PD_5 = ifelse(ESTMAT$M_6_5=="1-1",PIMV$MES_5,
                                ifelse(ESTMAT$M_6_5=="0-1",PIMV$MES_5,
                                       ifelse(ESTMAT$M_6_5=="2-1",PIMV$MES_5,
                                              ifelse(ESTMAT$M_6_5=="2-2",PPVMV$MES_5,
                                                     ifelse(ESTMAT$M_6_5=="1-2",PPVMV$MES_5,
                                                            ifelse(ESTMAT$M_6_5=="2-3",1,
                                                                   ifelse(ESTMAT$M_6_5=="3-3",1,
                                                                          0))))))),
                  PD_6 = ifelse(ESTMAT$M_7_6=="1-1",PIMV$MES_6,
                                ifelse(ESTMAT$M_7_6=="0-1",PIMV$MES_6,
                                       ifelse(ESTMAT$M_7_6=="2-1",PIMV$MES_6,
                                              ifelse(ESTMAT$M_7_6=="2-2",PPVMV$MES_6,
                                                     ifelse(ESTMAT$M_7_6=="1-2",PPVMV$MES_6,
                                                            ifelse(ESTMAT$M_7_6=="2-3",1,
                                                                   ifelse(ESTMAT$M_7_6=="3-3",1,
                                                                          0))))))),
                  PD_7 = ifelse(ESTMAT$M_8_7=="1-1",PIMV$MES_7,
                                ifelse(ESTMAT$M_8_7=="0-1",PIMV$MES_7,
                                       ifelse(ESTMAT$M_8_7=="2-1",PIMV$MES_7,
                                              ifelse(ESTMAT$M_8_7=="2-2",PPVMV$MES_7,
                                                     ifelse(ESTMAT$M_8_7=="1-2",PPVMV$MES_7,
                                                            ifelse(ESTMAT$M_8_7=="2-3",1,
                                                                   ifelse(ESTMAT$M_8_7=="3-3",1,
                                                                          0))))))),
                  PD_8 = ifelse(ESTMAT$M_9_8=="1-1",PIMV$MES_8,
                                ifelse(ESTMAT$M_9_8=="0-1",PIMV$MES_8,
                                       ifelse(ESTMAT$M_9_8=="2-1",PIMV$MES_8,
                                              ifelse(ESTMAT$M_9_8=="2-2",PPVMV$MES_8,
                                                     ifelse(ESTMAT$M_9_8=="1-2",PPVMV$MES_8,
                                                            ifelse(ESTMAT$M_9_8=="2-3",1,
                                                                   ifelse(ESTMAT$M_9_8=="3-3",1,
                                                                          0))))))),
                  PD_9 = ifelse(ESTMAT$M_10_9=="1-1",PIMV$MES_9,
                                ifelse(ESTMAT$M_10_9=="0-1",PIMV$MES_9,
                                       ifelse(ESTMAT$M_10_9=="2-1",PIMV$MES_9,
                                              ifelse(ESTMAT$M_10_9=="2-2",PPVMV$MES_9,
                                                     ifelse(ESTMAT$M_10_9=="1-2",PPVMV$MES_9,
                                                            ifelse(ESTMAT$M_10_9=="2-3",1,
                                                                   ifelse(ESTMAT$M_10_9=="3-3",1,
                                                                          0))))))),
                  PD_10 = ifelse(ESTMAT$M_11_10=="1-1",PIMV$MES_10,
                                 ifelse(ESTMAT$M_11_10=="0-1",PIMV$MES_10,
                                        ifelse(ESTMAT$M_11_10=="2-1",PIMV$MES_10,
                                               ifelse(ESTMAT$M_11_10=="2-2",PPVMV$MES_10,
                                                      ifelse(ESTMAT$M_11_10=="1-2",PPVMV$MES_10,
                                                             ifelse(ESTMAT$M_11_10=="2-3",1,
                                                                    ifelse(ESTMAT$M_11_10=="3-3",1,
                                                                           0))))))),
                  PD_11 = ifelse(ESTMAT$M_12_11=="1-1",PIMV$MES_11,
                                 ifelse(ESTMAT$M_12_11=="0-1",PIMV$MES_11,
                                        ifelse(ESTMAT$M_12_11=="2-1",PIMV$MES_11,
                                               ifelse(ESTMAT$M_12_11=="2-2",PPVMV$MES_11,
                                                      ifelse(ESTMAT$M_12_11=="1-2",PPVMV$MES_11,
                                                             ifelse(ESTMAT$M_12_11=="2-3",1,
                                                                    ifelse(ESTMAT$M_12_11=="3-3",1,
                                                                           0))))))),
                  PD_12 = ifelse(ESTMAT$M_13_12=="1-1",PIMV$MES_12,
                                 ifelse(ESTMAT$M_13_12=="0-1",PIMV$MES_12,
                                        ifelse(ESTMAT$M_13_12=="2-1",PIMV$MES_12,
                                               ifelse(ESTMAT$M_13_12=="2-2",PPVMV$MES_12,
                                                      ifelse(ESTMAT$M_13_12=="1-2",PPVMV$MES_12,
                                                             ifelse(ESTMAT$M_13_12=="2-3",1,
                                                                    ifelse(ESTMAT$M_13_12=="3-3",1,
                                                                           0))))))),
)

# PD DEFINITIVA
# PARA OBTENER LA PD DEL MES CREAR UN CONTEO PARA LOS MESES ACTIVOS Y SUMA DE LAS PD

PD_MES <- mutate(EXP_MES, SUMA = PD_12+PD_11+PD_10+PD_9+PD_8+PD_7+PD_6+PD_5+PD_4+PD_3+PD_2+PD_1,
                 row_count(EXP_MES,`1_MES`:`12_MES`, count = 1, var="CUMP", append = FALSE),
                 row_count(EXP_MES,`1_MES`:`12_MES`, count = 2, var="INCU", append = FALSE),
                 row_count(EXP_MES,`1_MES`:`12_MES`, count = 3, var="CAST", append = FALSE),
                 CONT_TOT = CUMP+INCU+CAST,
                 PD_PRV = SUMA/CONT_TOT
)

# COLOCAR LOS CLIENTES SIN RIESGO LA MINIMA PD POR EL CONTEO TOTAL
# TAMBIEN IDENTIFICO LOS CASTIGOS 

PD_SR <- mutate(PD_MES, PD_SR = ifelse(PD_MES$CONT_TOT==0,0.0003,0),
)

# COLOCO LOS CASTIGOS AL 100
# PD DEFINITIVA

PD_DEF <- mutate(PD_SR, PD_CAS = ifelse(CAST>0,1,0),
                 PD_DF = pmax(PD_CAS,PD_SR,PD_PRV,na.rm = TRUE))