# Primero escribimos NA en las variables con 9, 99.9 y 99.8
################################################################################

encuesta$p10h[encuesta$p10h == 99] <- NA
encuesta$p10m[encuesta$p10m == 99] <- NA



### Crea la variable tiempo de traslado
encuesta <- encuesta %>% mutate(p10 = p10h*60 + p10m)


##Selección de Variables
################################################################################

encuesta <- encuesta %>% select(desertor,
                                edo,
                                muni,  
                                edad, 
                                sexo, 
                                p5, 
                                p6,   
                                p7,    
                                p9_1:p9_12,    
                                p10,
                                p12_1:p12_10,
                                p14,    
                                p15,  
                                p16,  
                                p17,   
                                p18,   
                                p23_1:p23_2,  
                                p25_1:p25_23,    
                                p27,     
                                p39,    
                                p40_1:p40_15,  
                                p41a:p41i
                                )
  

####Recodificacion de variables
################################################################################


# Cambia las variables  como integer o numeric
encuesta <- encuesta %>% mutate_at(vars(edad), as.integer)
encuesta <- encuesta %>% mutate_at(vars(p5, p10, p15), as.numeric)

# cambia la variable desertor: 1 =  desertor, 2 = no desertor

encuesta$desertor <-  recode_factor(encuesta$desertor,
                                    `1` = "desertor",
                                    `0` = "no_desertor")

# Cambia las variables a factor
encuesta <- encuesta %>% mutate_at(vars(sexo,
                                        p6,
                                        p7,
                                        p9_1:p9_12,
                                        p12_1:p12_10,
                                        p14,
                                        p16,
                                        p17,
                                        p18,
                                        p23_1,p23_2,
                                        p25_1:p25_23,
                                        p27,
                                        p39,
                                        p40_1:p40_15,
                                        p41a:p41i), as_factor)


#Arreglamos los NA

encuesta[encuesta == 99.9] <-  NA
encuesta[encuesta == 99.8] <-  NA
#encuesta[encuesta == -1] <-  NA
# Imputar promedio en variables numericas

  encuesta <- encuesta %>%
    mutate_at(vars(p5,p10,p15),
      funs(ifelse(is.na(.), round(mean(., na.rm = TRUE),1), .)))
  









#### clave preguntas ---  se irá completando el apartado
################################################################################

#f7 = "¿Cuántas personas de 14 a 25 años viven en su hogar, sin contar visitas temporales ni trabajadores domésticos?"
#f9  = "¿Cuántas personas de 14 a 25 años viven en su hogar, sin contar visitas temporales ni trabajadores domésticos?"
#f10 = "A continuación le voy a mostrar una tarjeta con niveles de estudio. Por favor dígame cuál es el último nivel de estudios en el que estuvo inscrito ____________ [INSERTAR NOMBRE] aunque no haya terminado o aunque no haya asistido."
#f11 = "[MOSTRAR TARJETA DE SELECCIÓN Y PREGUNTAR SÓLO A INDIVIDUOS CON CÓDIGOS 4, 5 o 6 en F.10] Ahora por favor dígame si ____________[INSERTAR EL NOMBRE DE INDIVIDUOS CON CÓDIGOS 4, 5 o 6 en F10) sigue asistiendo a la escuela, terminó los estudios o dejó de asistir a la escuela."
#p1  = "[MOSTRAR TARJETA] A continuación te voy a mostrar una tarjeta con algunas opciones de educación media superior. Por favor dime cuál de ellas has cursado o en cuál de ellas has estado inscrito aunque no la hayas terminado."
#p2  = "¿En qué mes y año entraste a la/el _______ [INSERTAR RESPUESTA DE P1]?"
#p3  = "¿Cuántos años terminaste en ese nivel?"
#p4  = "Para entrar a la preparatoria, bachillerato o carrera técnica, es muy probable que hayas realizado el EXANI I o COMIPEMS ¿Podrías decirme cuál fue tu puntaje en el EXANI I o en el COMIPEMS? Si no lo recuerdas exactamente, por favor dime una cifra aproximada."
#p5  = "Ahora dime, en una escala del 0 al 10, ¿cuál fue tu promedio de la secundaria? Si no lo recuerdas exactamente, por favor dime una cifra aproximada."
#p6  = " Y la secundaria a la que asististe, ¿era pública o privada?"
#p7  = "¿Cuántas veces te cambiaste de escuela durante tus estudios de preparatoria, bachillerato o carrera técnica?"
#p8  = " ¿Y alguno de esos cambios de escuela se debió a que te cambiaste de casa o se debió a otra razón?"
#p9  = "[MOSTRAR TARJETA] Ahora por favor piensa en tus estudios de preparatoria, bachillerato o carrera técnica y dime en cuál de las instituciones que aparecen en la tarjeta estabas inscrito."
#p10 = "¿Cuánto tiempo te tomaba llegar a la institución a la que asistías?"
#p11 = "[MOSTRAR TARJETA] A continuación te voy a mostrar una lista de algunas razones por las que la gente puede elegir una escuela sobre otra. ¿Cuál de ellas fue la más importante para ti cuando elegiste la escuela en la que estudiabas la preparatoria, bachillerato o carrera técnica [ESPERAR RESPUESTA] ¿Y la segunda más importante? [ESPERAR RESPUESTA] ¿Y la tercera?"
#p12 = "A continuación te voy a leer una lista de personas con las que posiblemente vivías al momento de cursar la preparatoria, bachillerato o carrera técnica. Por favor dime si vivías o no"
#p13 = "[MOSTRAR TARJETA] Cuando tenías problemas en la escuela, ¿qué tanta confianza tenías para recurrir __________ [INSERTAR OPCIONES] ¿Mucha, algo, poca o nada de confianza?"
#p14 = "Durante tu último año de estudios de la preparatoria, bachillerato o carrera técnica, dirías que______________[LEER OPCIONES]"
#p15 = "En una escala del 0 al 10, ¿podrías decirme cuál fue tu promedio en el último semestre/año que terminaste en la preparatoria, bachillerato o carrera técnica? Si no lo recuerdas exactamente, por favor dime una cifra aproximada. [ENCUESTADOR: ACEPTAR SOLO UN DECIMAL]"
#p16 = "[MOSTRAR TARJETA] Por favor piensa en el tiempo en que estudiaste la preparatoria, bachillerato o carrera técnica y dime cuál de las frases en la tarjeta describe mejor tu desempeño en ese periodo"
#p17 = "[MOSTRAR TARJETA] Por favor piensa en tu grupo del último año que cursaste la preparatoria, bachillerato o carrera técnica y dime cuál de las frases en la tarjeta describe mejor tu desempeño en ese periodo"
#p18 = "¿Durante el último año que cursaste la preparatoria, bachillerato o carrera técnica tenías o no tenías alguna beca?"
#p19 = "¿Qué instituciónte dio la beca?[LEER OPCIONES]"
#p20 = "Ahora dime, ¿recuerdas en qué mes y año dejaste de estudiar? Si no lo recuerdas exactamente por favor dime una fecha aproximada [ANOTAR MES Y AÑO]."
#p21 = "Por favor piensa en el momento en el que dejaste de estudiar ¿tú querías seguir estudiando o te querías salir de la escuela?"
#p22 = "A continuación te voy a leer una lista de personas que tal vez influyeron en la decisión de que dejaras de estudiar. Por favor dime si ____________ (INSERTAR OPCIONES) influyó o no influyó en la decisión de que dejaras de estudiar. [LEER OPCIONES]"
#p23 = "¿Tienes ____[INSERTAR OPCIÓNES] que hayan dejado de estudiar la preparatoria, bachillerato o carrera técnica, es decir que la hayan iniciado pero que no la concluyeron?"
#p24 = "Mientras estudiabas la preparatoria, bachillerato o carrera técnica ¿te ocurrió o no te ocurrió que _____ [INSERTAR OPCIONES]"
#p25 = "[MOSTRAR TARJETA] Ahora por favor dime, de las opciones de esta tarjeta ¿cuál fue el principal motivo por el que abandonaste la escuela? [ESPERAR RESPUESTA] Si hubo algún otro motivo, por favor dime cuál fue [ENCUESTADOR: REGISTRAR LA SEGUNDA Y TERCERA MENCIONES]"
#p26 = "Al dejar de asistir a la estcuela, ¿te buscó o no te buscó algún directivo o profesor de la escuela donde estudiabas para conocer tus razones para salirte y/o convencerte de seguir estudiando?"
#p27 = "Durante tus estudios de preparatoria, bachillerato o carrera técnica, ¿trabajaste en algún momento o no trabajaste en este periodo?"
#p28 = "Y cuando abandonaste los estudios, ¿estabas trabajando o no estabas trabajando?"
#p29 = "[MOSTRAR TARJETA] Por favor piensa cuántos años estudiaste la preparatoria, bachillerato o carrera técnica, ahora dime, ¿cuál de las siguientes frases describe mejor el tiempo que trabajaste y estudiaste al mismo tiempo?"
#p30 = "Al trabajar y estudiar la preparatoria, bachillerato o carrera técnica al mismo tiempo, aproximadamente ¿cuánto ganabas al mes?"
#p31 = "Y aproximadamente, ¿cuántas horas a la semana trabajabas en ese momento?"
#p32 = "Independientemente de quién tomó la decisión de que dejaras de estudiar, ¿crees que fue una buena o una mala decisión? [ESPERAR RESPUESTA] ¿muy o algo?"
#p33 = "[MOSTRAR TARJETA] ¿Qué tanto crees que haber dejado de estudiar ha afectado negativamente_______[INSERTAR OPCIONES] ¿mucho, algo, poco o nada?"
#p34 = "P34. ¿Te interesa continuar con tus estudios o NO te interesa continuar con tus estudios?"
#p35 = "[MOSTRAR TARJETA] De acuerdo con las opciones de la siguiente tarjeta, ¿hasta qué nivel educativo te gustaría estudiar?"
#p36 = "[MOSTRAR TARJETA] A continuación te voy a mostrar posibles causas por las que algunas personas no regresan a estudiar. Por favor piensa en tu situación y dime qué tanto ha influido cada una de ellas en que no retomes tus estudios. ¿Mucho, algo poco o nada?"
#p37 = "[MOSTRAR TARJETA] A continuación te voy a mostrar una tarjeta con razones por las que alguien podría decidir continuar con sus estudios, por favor dime ¿cuál de ellas es la que más te motivaría para continuar tus estudios? [ESPERAR RESPUESTA]¿y la segunda? [ACEPTAR HASTA DOS MENCIONES]"
#p38 = "[MOSTRAR TARJETA] De acuerdo con las opciones que aparecen en la tarjeta dime, si tuvieras la oportunidad de terminar tus estudios, ¿en qué tipo de institución lo harías? [ACEPTAR SOLO UNA RESPUESTA]"
#p39 = "¿Cuántos cuartos para dormir tenía la casa donde vivías cuándo estudiabas la preparatoria, bachillerato o carrera técnica?"
#p40 = "¿Con cuáles de las siguientes cosaas contabas en casa cuando estudiabas la preparatoria, el bachillerato o la carrera técnica?"
#p41 = "A continuación te voy a entregar una hoja que llenarás tú mismo, de esta forma se garantiza que tus respuestas serán absolutamente anónimas y confidenciales. Puede que algunas de estas preguntas no apliquen para ti. Cuando cursabas la preparatoria, bachillerato o carrera técnica ¿Con qué frecuencia consumías los productos que aparecen en la primer columna de la hoja?"

  
  

#"folio"  NO
#"edo"    SI 
#"muni"   SI   
#"loc"   NO   
#"ageb"   NO    
#"f7"     NO  
#"edad"   NO (USADA COMO FILTRO)   
#"sexo"   SI
#"ins"    NO  
#"ultimo" NO
#"actual" NO 
#"f20h"   NO   
#"f20m"   NO
#"f21"    NO 
#"p1"     NO   
#"p2m"    NO
#"p2a"    NO 
#"p4"     NO
#"p5"     SI
#"p6"     SI
#"p7"     SI 
#"p8"     NO
#"p9_1"   SI   
#"p9_2"   SI
#"p9_3"   SI
#"p9_4"   SI
#"p9_5"   SI
#"p9_6"   SI
#"p9_7"   SI
#"p9_8"   SI
#"p9_9"   SI
#"p9_10"  SI
#"p9_11"  SI
#"p9_12"  SI
#"p9ot"   NO (es la respueta de la p9_12)
#"p10h"   NO (Se calcula la variable tiempo_traslasdo)
#"p10m"   NO
#"p11_1"  NO
#"p11_2"  NO  
#"p11_3"  NO 
#"p12_1"  SI  
#"p12_2"  SI 
#"p12_3"  SI 
#"p12_4"  SI
#"p12_5"  SI 
#"p12_6"  SI
#"p12_7"  SI
#"p12_8"  SI 
#"p12_9"  SI
#"p12_10" SI 
#"p12_5cts" NO
#"p12_6cts" NO
#"p12_7cts" NO
#"p12_8cts" NO
#"p12_10ts" NO
#"p13_1"  NO
#"p13_2"  NO 
#"p13_3"  NO
#"p13_4"  NO
#"p13_5"  NO 
#"p13_6"  NO 
#"p13_7"  NO
#"p14"    SI  
#"p15"    SI
#"p16"    SI
#"p17"    SI 
#"p18"    SI 
#"p19"    NO  
#"p23_1"  SI  
#"p23_2"  SI
#"p23_1cts" NO
#"p23_2cts" NO
#"p25_1"  SI   
#"p25_2"  SI
#"p25_3"  SI 
#"p25_4"  SI
#"p25_5"  SI 
#"p25_6"  SI 
#"p25_7"  SI 
#"p25_8"  SI 
#"p25_9"  SI 
#"p25_10" SI
#"p25_11" SI 
#"p25_12" SI 
#"p25_13" SI 
#"p25_14" SI
#"p25_15" SI 
#"p25_16" SI
#"p25_17" SI  
#"p25_18" SI 
#"p25_19" SI
#"p25_20" SI 
#"p25_21" SI 
#"p25_22" SI
#"p25_23" SI 
#"p27"    SI    
#"p29"    NO 
#"p30"    NO   
#"p39"    SI  
#"p40_1"  SI
#"p40_2"  SI  
#"p40_3"  SI  
#"p40_4"  SI 
#"p40_5"  SI  
#"p40_6"  SI  
#"p40_7"  SI 
#"p40_8"  SI   
#"p40_9"  SI  
#"p40_10" SI 
#"p40_11" SI  
#"p40_12" SI  
#"p40_13" SI 
#"p40_14" SI  
#"p40_15" SI  
#"p41a"   SI 
#"p41b"   SI  
#"p41c"   SI  
#"p41d"   SI 
#"p41e"   SI  
#"p41f"   SI  
#"p41g"   SI 
#"p41h"   SI 
#"p41i"   SI  
#"id"      NO
#"cedo"    NO 
#"cmuni"   NO
#"cedo_muni" NO



  