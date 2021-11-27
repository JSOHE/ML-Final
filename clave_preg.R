####Recodificacion de variables
################################################################################

# Cambia las variables a factor 
encuesta <- encuesta %>% mutate_at(vars(8:11,14:123,125), as_factor)
encuesta <- encuesta %>% mutate_at(vars(edad), as.integer)
encuesta <- encuesta %>% mutate_at(vars(p5,p10h,p10m), as.numeric)

encuesta <- encuesta %>% mutate(tiempo = p10h*60 + p10m) %>% glimpse

# cambia la variable desertor: 1 =  desertor, 2 = no desertor

encuesta$desertor <-  recode_factor(encuesta$desertor,
              `1` = "desertor",
              `0` = "no_desertor")

######## Se eliminan variables que no aportan nada al entendimiento de los desertores
################################################################################

#f20h se refiere a la hora en que se hizo la encuesta y 
#f20m se refiere al minuto en que se hizo la encuesta.
# cedo era una clave que se utilizó para generar los municipio y poner nombres.
# cmuni era una clave usada para poner nombre a los municipios
#p2m es el mes en que entró a estudiar la educación media superior
#p2a es el año en que entró a estudiar la educación media superior
#p4  = "Para entrar a la preparatoria, bachillerato o carrera técnica, es muy probable que hayas realizado el EXANI I o COMIPEMS ¿Podrías decirme cuál fue tu puntaje en el EXANI I o en el COMIPEMS? Si no lo recuerdas exactamente, por favor dime una cifra aproximada."
# p10h y p10m se toman para crear la variable "tiempo", se deja todo en minutos 
encuesta <- encuesta %>% select(-folio, -muni, -f20h,-f20m, 
                                -p2m, -p2a, 
                                -cedo, -cmuni, -cedo_muni, 
                                -p4, -p10h,-p10m)

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
# [PASE A SOCIODEMOGRÁFICOS]
#p42 = "[MOSTRAR TARJETA] A continuación te voy a mostrar una tarjeta con algunos niveles de estudio. Por favor dime ¿cuál es el último nivel de estudios que cursaste aunque no hayas terminado?"
#p1  = "[MOSTRAR TARJETA] A continuación te voy a mostrar una tarjeta con algunas opciones de educación media superior. Por favor dime cuál de ellas has cursado o en cuál de ellas has estado inscrito aunque no la hayas terminado."

#p43 = "Ahora dime, ¿en qué año ingresaste a la preparatoria, bachillerato o carrera técnica?"
#p44 = "Cuando ingresaste a la preparatoria, bachillerato o carrera técnica, es muy probable que hayas realizado el EXANI I o COMIPEMS ¿Podrías decirme cuál fue tu puntaje en el EXANI I o en el COMIPEMS? Si no lo recuerdas exactamente, por favor dime una cifra aproximada."
#p45 = "En una escala del 0 al 10, ¿cuál fue tu promedio de la secundaria? Si no lo recuerdas exactamente, por favor dime una cifra aproximada. [ENCUESTADOR: ACEPTAR SOLO UN DECIMAL]"
#p46 = "Y la secundaria a la que asististe, ¿era pública o privada?"
#p47 = "¿Cuántas veces te cambiaste de escuela durante tus estudios de preparatoria, bachillerato o carrera técnica?"
#p48 = "¿Y alguno de esos cambios de escuela se debió a que te cambiaste de casa o se debió a otra razón?"
#p49 = "[MOSTRAR TARJETA] Ahora por favor piensa en tus estudios de preparatoria, bachillerato o carrera técnica y dime en cuál de las instituciones que aparecen en la tarjeta estás o estabas inscrito."
#p50 = "P50. Al cursar la preparatoria, bachillerato o carrera técnica ¿Cuánto tiempo te tomaba llegar a la institución a la que asistes o asistías?"
#p51 = "[MOSTRAR TARJETA] A continuación te voy a mostrar una lista de algunas razones por las que la gente puede elegir una escuela sobre otra. Ahora por favor dime cuál de ellas fue la más importante para ti cuando elegiste la escuela en la que estudias o estudiaste la preparatoria, bachillerato o carrera técnica. [ESPERAR RESPUESTA] ¿Y la segunda más importante? [ESPERAR RESPUESTA] ¿Y la tercera?"
#p52 =
#  p53 =
#  p54 =
#  p55 =
#  p56 =
#  p57 =
#  p58 =
#  p59 =
#  p60 =
#  p61 =
#  p62 =
#  p63 =
#  p64 =
  
  
  # similares
  # p1 = p42
  # p2 = p43
  # p3 = p44
  # p3 = p44
  