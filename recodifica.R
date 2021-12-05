# cambia la clase de las variables
encuesta <- encuesta %>% mutate_at(vars(sexo, 
                                        secpublica,
                                        p24_1:p24_23, tipoescuela),
                                   factor)

encuesta <- encuesta %>% mutate_at(vars(edad),
                                   as.integer)


# Codificación de la entodad federativa
encuesta$edo <- recode_factor(encuesta$edo,
                              `1`	= 'Ags',
                              `2`	= 'BC',
                              `3`	= 'BCS',
                              `4`	= 'Camp',
                              `5`	= 'Coah',
                              `6`	= 'Col',
                              `7`	= 'Chis',
                              `8`	= 'Chih',
                              `9`	= 'CDM',
                              `10` =	'Dgo',
                              `11` =	'Gto',
                              `12` =	'Gro',
                              `13` =	'Hgo',
                              `14` =	'Jal',
                              `15` =	'Mex',
                              `16` =	'Mich',
                              `17` =	'Mor',
                              `18` =	'Nay',
                              `19` =	'NL',
                              `20` =	'Oax',
                              `21` =	'Pue',
                              `22` =	'Qro',
                              `23` =	'Q. Roo',
                              `24` =	'SLP',
                              `25` =	'Sin',
                              `26` =	'Son',
                              `27` =	'Tab',
                              `28` =	'Tamps',
                              `29` =	'Tlax',
                              `30` =	'Ver',
                              `31` =	'Yuc',
                              `32` =	'Zac')
# colocamos las columnas de manera correcta

encuesta$desertor <-  recode_factor(encuesta$desertor,
                                    `1` = "desertor",
                                    `0` = "no_desertor")

encuesta$sexo <-  recode_factor(encuesta$sexo,
                                `1` = "hombre",
                                `2` = "mujer")

encuesta$secpublica <-  recode_factor(encuesta$secpublica,
                                      `1` = "si",
                                      `0` = "no")

encuesta$freqasis <- recode_factor(encuesta$freqasis,
              `1` = "siempre asistia",
              `2` = "asistia regularmente",
              `3` = "faltaba con frecuencia o mucho")

encuesta$reprueba <- recode_factor(encuesta$reprueba, 
                                   `0` = "no", 
                                   `1` = "si")

encuesta$becado <-  recode_factor(encuesta$becado, 
                                  `0` = "no",
                                  `1` = "si")

encuesta$p23_1 <- recode_factor(encuesta$p23_1,
                                `1` = "si",
                                `0` = "no")

encuesta$p23_2 <- recode_factor(encuesta$p23_2,
                                `1` = "si",
                                `0` = "no")


encuesta$p24_1  <- recode_factor(encuesta$p24_1, 
                                 `1` = "si",
                                 `2` = "no")

encuesta$p24_2  <- recode_factor(encuesta$p24_2, 
                                 `1` = "si",
                                 `2` = "no")

encuesta <- encuesta %>% 
  mutate_at(.vars = vars(p24_1:p24_23),
            .funs = recode_factor,
            `1` = "si menciono",
            `2` = "no menciono")

encuesta$trab_est <- recode_factor(encuesta$trab_est,
                                   `0` = "no",
                                   `1` = "si")

encuesta <- encuesta %>% 
  mutate_at(.vars = vars(p40_1:p40_15),
            .funs = recode_factor,
            `1` = "si",
            `2` = "no")


encuesta$tipoescuela <- recode_factor(encuesta$tipoescuela,
                                      `0` = "N/C",
                                      `1` = "CONALEP",
                                      `2` = "CEBETIS o CBTIS",
                                      `3` = "Colegio Bachilleres o DGB",
                                      `4` = "Prepa Publica",
                                      `5` = "EMSAD",
                                      `6` = "Vocacional o CECyT o CECYTE",
                                      `7` = "CBTA o CBTF",
                                      `8` = "CETMAR o CETAC",
                                      `9` = "Bachillerato estatal",
                                      `10` = "Prepa abierta",
                                      `11` = "Prepa privada",
                                      `12` = "Otro bachillerato publico")

encuesta$pervivia <- recode_factor(encuesta$pervivia, 
                                   `1` = "solo",
                                   `2` = "familia",
                                   `3` = "amigos u otros",
                                   `4` = "pareja o hijos")

encuesta <- encuesta %>% 
  mutate_at(.vars = vars(p13_esc:p13_amg),
            .funs = recode_factor,
            `0` = "no",
            `1` = "si")


encuesta <- encuesta %>% 
  mutate_at(.vars = vars(tabaco:otrasdrog),
            .funs = recode_factor,
            `0` = "no",
            `1` = "si")

encuesta$nivelprom <- recode_factor(encuesta$nivelprom, 
                                    `1` = "promedio muy alto",
                                    `2` = "promedio alto",
                                    `3` = "promedio regular",
                                    `4` = "promedio bajo",
                                    `5` = "promedio muy bajo")
