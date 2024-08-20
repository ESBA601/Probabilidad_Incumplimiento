# LA FECHA EN FORMATO LETRA y COMO DF

var_aux_1 <- data.frame(FECHA1) 

name0 <- as.character(FECHA1,format="%b-%Y")

CIERRE <- rename(var_aux_1, c("CIERRE"="FECHA1"))

# ELIMINA LA NOTACION CIENTIFICA

options(scipen=999) 

# ACTUALIZAR LAS CUENTAS CONTABLES

cuenta0 <- select(RIESGO, CUENTA_CONTABLE) 

cuenta1 <- unique(cuenta0)

cuenta2 <- select(CUENTA, CUENTA_CONTABLE)

cuenta3 <- anti_join(cuenta1,  cuenta2,  by="CUENTA_CONTABLE")

cuenta4 <- rbind(cuenta2, cuenta3)

cuenta5 <- mutate(cuenta4,  DIG1= str_sub(cuenta4$CUENTA_CONTABLE, 1, 1), 
                  DIG2 = str_sub(cuenta4$CUENTA_CONTABLE, 4, 5),
                  DIG10 = str_sub(cuenta4$CUENTA_CONTABLE, 10, 10),
                  MAR_CUEN=ifelse(DIG1==1 & DIG2=="02",1,
                                  ifelse(DIG1==1,0,
                                         ifelse(DIG1==8 & DIG10==1,2,
                                                ifelse(DIG1==6,1,
                                                       ifelse(DIG1==8 & DIG10!=1,0,""))))))

CUENTA_NEW <- select(cuenta5, CUENTA_CONTABLE, MAR_CUEN) 

# PARA IDENTIFICAR LOS PRESTAMOS ACTIVOS PARA APLICAR LA PD 

risk_1 <- left_join(RIESGO, CUENTA_NEW, by = "CUENTA_CONTABLE") 
  
bef_1 <- left_join(ANTERIOR, CUENTA_NEW, by = "CUENTA_CONTABLE") 

# SEPARA EL IDENTIFICADOR ESTO *ES PARA DESPUES REALIZAR LA UNION CON TDC

risk_2 <- mutate(risk_1,  CLIENTE_IDENT = str_sub (risk_1$RIF_CI , 1, 1), IDENT = str_sub (risk_1$RIF_CI, 2, 15))

# SUBCONJUNTO DE LOS CLIENTES ACTIVOS DEL BANCO
# tambien se puede hacer la separacion por cartera

risk_str <- filter(risk_2, MAR_CUEN == "0")

bef_str <- filter(bef_1, MAR_CUEN == "0")

# SEPARAR E IDENTIFICAR LOS CLIENTES CON PRORROGAS  
# cual es mejor realizar la suma o el maximo, tambien se tiene que probar con un risgo con prorrogas
# puede estar antes de risk_str
# creo que tambien se tiene que realizar un cuadro adicional con los clientes en castigo

risk_3 <- select(risk_str, ID, CANTIDAD_PRORROGAS)

risk_pro <- group_by(risk_3, ID) %>% summarise(PRORROGAS=sum(CANTIDAD_PRORROGAS))

# AGRUPAR POR ID Y LUGO POR SALDO POR CLIENTE

risk_4 <- select(risk_str, ID, SALDO_ACTUAL, MONTO_VIGENTE, MONTO_REESTRUCTURADO ,MONTO_VENCIDO, MONTO_LITIGIO)

risk_cli <- group_by(risk_4, ID) %>% summarise(TOTAL = sum(SALDO_ACTUAL), 
                                                VIGENTE = sum(MONTO_VIGENTE),
                                                REESTRUCTURADO = sum(MONTO_REESTRUCTURADO), 
                                                VENCIDO = sum(MONTO_VENCIDO), 
                                                LITIGIO = sum(MONTO_LITIGIO))

# Solo quiero los clientes activos al cierre del mes pasado

bef_4 <- select(bef_str, ID, SALDO_ACTUAL)

bef_cli <- group_by(bef_4, ID) %>% summarise(TOTAL = sum(SALDO_ACTUAL))

# QUITAR DUPLICADOS DE LOS CLIENTES
# se tiene que tener cuidado con los s diferentes del mismo cliente
# se tiene que traer tambien la cedula o al final

risk_5 <- select(risk_str, ID, NOMBRE_CLIENTE)

risk_nom <- unique(risk_5)

bef_5 <- select(bef_str, ID, NOMBRE_CLIENTE)

bef_nom <- unique(bef_5)

# REALIZAR EL FUERA DE DE PLAZO METODO 2 UTILIZANDO LOS VALORES DE LA PERIODICIDAD
# se tiene que cambiar la fecha de cierre en los cierres

risk_6 <- select(risk_str, ID, NOMBRE_CLIENTE, RIF_CI, PRESTAMO, FECHA_LIQUIDACION ,FECHA_VENCIMIENTO_ACTUAL , FECHA_VENCIMIENTO_ORIGINAL, PERIODICIDAD, PERIODICIDAD_CANT, CUENTA_CONTABLE)
risk_7 <- mutate(risk_6, CIERRE)

# CAMBIO DE FORMATO DE LAS FECHAS VENCIMIENTO, ORIGINAL Y FECHA DE CIERRE

risk_7$FECHA_VENCIMIENTO_ACTUAL <- as.numeric(risk_7$FECHA_VENCIMIENTO_ACTUAL)
risk_7$FECHA_VENCIMIENTO_ACTUAL <- as.Date(risk_7$FECHA_VENCIMIENTO_ACTUAL, origin = "1899-12-30")
risk_7$FECHA_VENCIMIENTO_ACTUAL <- as.Date(risk_7$FECHA_VENCIMIENTO_ACTUAL)

risk_7$FECHA_VENCIMIENTO_ORIGINAL <- as.numeric(risk_7$FECHA_VENCIMIENTO_ORIGINAL)
risk_7$FECHA_VENCIMIENTO_ORIGINAL <- as.Date(risk_7$FECHA_VENCIMIENTO_ORIGINAL, origin = "1899-12-30")
risk_7$FECHA_VENCIMIENTO_ORIGINAL <- as.Date(risk_7$FECHA_VENCIMIENTO_ORIGINAL)

risk_7$FECHA_LIQUIDACION <- as.Date(risk_7$FECHA_LIQUIDACION, origin = "1899-12-30")
risk_7$CIERRE <- as.Date(risk_7$CIERRE)

# LLEVAR LA PERIODICIDAD A LA MISMA UNIDAD DIAS 

risk_8 <- mutate(risk_7, DIAS = ifelse (risk_7$PERIODICIDAD=="Y", 360, ifelse (risk_7$PERIODICIDAD=="M", 30, ifelse (risk_7$PERIODICIDAD=="D",1,0))),DFP=30, SUB_CNT=substring(risk_7$CUENTA_CONTABLE, 4, 5)) %>% 
  filter(SUB_CNT != 38 & SUB_CNT != 36 & SUB_CNT != 48 & SUB_CNT != 58)

# CALCULO TOTAL DE DIAS ESTIMADOS

risk_9 <- mutate(risk_8, TOT_DIAS = PERIODICIDAD_CANT * DIAS) 

# CALCULO DE LA FECHA ESTIMADA

risk_fue <- mutate(risk_9, FECHA_EST = FECHA_LIQUIDACION+TOT_DIAS+DFP, FECHA_FP = FECHA_LIQUIDACION+TOT_DIAS)

# CALCULO DEL FUERA DE PLAZO

risk_pla <- mutate(risk_fue, FUERA_PLAZO = ifelse (risk_fue$FECHA_EST>risk_fue$CIERRE, "0","2"))

# IDENTIFICAR EL FUERA DE PLAZO POR CLIENTE

risk_fin<- group_by(risk_pla, ID) %>% summarise(MAX_FDP = max(FUERA_PLAZO))

# CREAR LOS CASTIGOS NUEVOS E IDENTIFICAR LOS CLIENTES NUEVOS CASTIGO

risk_10 <- filter(risk_1, MAR_CUEN == 2) %>% select(ID, NOMBRE_CLIENTE)

risk_cas <- anti_join(risk_10, CASTIGO) %>% mutate(VEC ="3", CLIENTES = 1)

risk_cas <- unique(risk_cas)

# ACTUALIZAR LOS NUEVOS CASTIGOS

CASTIGO_NEW1 <- rbind(CASTIGO, risk_10) 

CASTIGO_NEW <- unique(CASTIGO_NEW1)

# TODAVIA FALTA Ao?=ADIR A LOS NUEVOS CLIENTES A LA BASE QUE SE LLEVERA

# UNIR EL  DEL CLIENTE CON SU SALDO Y LA TABLA DE PRORROGAS

risk_tot <- left_join(risk_nom, risk_cli, by = "ID") %>%
  left_join(risk_pro, by = "ID") %>%
  left_join(risk_fin, by = "ID") 

# COLOCAR EL VECTOR DE A LOS CLIENTES
# falta colocar los clientes adicionales 
# faltaria probar con un cliente que tenga prorroga

risk_vec <-mutate(risk_tot, VIG = ifelse (risk_tot$VIGENTE>0, "1","0"),
                  RES = ifelse (risk_tot$REESTRUCTURADO>0, "2","0"),
                  VEN = ifelse (risk_tot$VENCIDO>0, "2","0"),
                  LIT = ifelse (risk_tot$LITIGIO>0, "2","0"), 
                  PRO = ifelse (risk_tot$PRORROGAS>0, "2","0"),
                  FDP = ifelse (risk_tot$MAX_FDP>0, "2","0"), 
                  CLIENTES = 1)

# MAXIMO VECTOR POR CLIENTE
# de esta tabla se sacara el vector final para llevar al historico
# aqui se puede tambien traer la cedula o el rif

risk_max <-mutate(risk_vec, VEC = pmax(VIG,VEN,LIT,RES,PRO,FDP)) 

# IDENTIFICAR LOS CLIENTES QUE ESTAN CANCELANDO
# tener en cuenta los clientes castigados

act <- select(risk_cli, ID, TOTAL)

bef_id <- anti_join(bef_cli, act, by = "ID" ) %>% left_join(bef_nom, by = "ID" )

bef_can <- select(bef_id, ID, NOMBRE_CLIENTE) %>% mutate(VEC = 1, CLIENTES = 1)

# AQUI SE TIENE QUE UNIR PRIMERO LOS CASTIGADOS Y LOS CANCELADOS

risk_pre <- rbind(data.frame(bef_can),data.frame(risk_cas)) 

risk_ccs<- group_by(risk_pre, ID, NOMBRE_CLIENTE, CLIENTES) %>% summarise(VEC = max(VEC)) %>% select(ID, NOMBRE_CLIENTE, VEC, CLIENTES) 

# AGREGAR LOS CANCELADOS AL VECTOR DE LOS CLIENTES ACTIVOS

risk_def <- select(risk_max, ID, NOMBRE_CLIENTE, VEC, CLIENTES)

risk_end <- rbind(data.frame(risk_def), data.frame(risk_ccs)) 
