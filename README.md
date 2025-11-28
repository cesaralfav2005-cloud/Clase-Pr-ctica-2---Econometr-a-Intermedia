# Clase-Pr-ctica-2---Econometr-a-Intermedia
Integrantes del equipo:  
1. César de Jesús Alfaro Villalobos 
2. Nayhesli Galgania Hernández Gómez
3. Allan Moisés Juárez Sánchez 

NOMBRE DEL DOCENTE

# MSc. NORVIN ANTONIO DUARTE

GRUPO

3M1-EyN-S

################################################################################
Fecha: 28/11/2025 & 29/11/2025

Paso 1: Instalar paquetes

# Installing packages

install.packages("haven")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("estimatr")
install.packages("fixest")
install.packages("AER")
install.packages("car")
install.packages("broom")
install.packages("quantmod")
install.packages("modelsummary")
install.packages("lmtest")
install.packages("MatchIt")

Paso 2: Cargar Paquetes

# Loading Packages

library(haven)
library(dplyr)
library(xtable)
library(stargazer)
library(MatchIt)
library(margins)
library(ggplot2)
library(tidyr)
library(estimatr)
library(fixest)
library(AER)
library(texreg)
library(broom)
library(sandwich)
library(Synth)
library(plm)
library(car)
library(quantmod)
library(modelsummary)
library(lmtest)

################################################################################

# Cargar la base de datos de las variables principales
1. Variable Remesas
library(readxl)
REMESAS_PARA_R <- read_excel("~/CLASES UNIVERSITARIAS - IEN/ECONOMETRÍA RSTUDIO/Econometria tarea 2/REMESAS PARA R.xlsx")
View(REMESAS_PARA_R)                                                                                               

2. Variable PIB Trimestral
library(readr)
PIB_TRIMESTRAL_REAL_DE_NICARAGUA <- read_csv("~/CLASES UNIVERSITARIAS - IEN/ECONOMETRÍA RSTUDIO/Econometria tarea 2/PIB TRIMESTRAL REAL DE NICARAGUA.csv")
Rows: 73 Columns: 2                                                                                           
── Column specification ──────────────────────────────────────────────────────────────────────────────────────
Delimiter: ","
chr (1): observation_date
dbl (1): rate

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
View(PIB_TRIMESTRAL_REAL_DE_NICARAGUA)

3. Variable Tasa de Desempleo Trimestral
library(readr)
TASA_DE_DESEMPLEO_TRIMESTRAL <- read_csv("~/CLASES UNIVERSITARIAS - IEN/ECONOMETRÍA RSTUDIO/Econometria tarea 2/TASA DE DESEMPLEO TRIMESTRAL.csv")
Rows: 73 Columns: 2                                                                                           
── Column specification ──────────────────────────────────────────────────────────────────────────────────────
Delimiter: ","
dbl  (1): LNU04032231
date (1): observation_date

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
View(TASA_DE_DESEMPLEO_TRIMESTRAL)

4. Covariable: Se menciona posteriormente

# Eliminando los datos omitidos o espacios en blanco tanto en las variables principales como en las covariables a usarse

# Cargar Covariables

## Covariable a usarse: Tipo de Cambio Nicaragüense##
library(readxl)
TIPO_DE_CAMBIO_DATOS <- read_excel("~/CLASES UNIVERSITARIAS - IEN/ECONOMETRÍA RSTUDIO/Econometria tarea 2/TIPO DE CAMBIO DATOS.xlsx")
View(TIPO_DE_CAMBIO_DATOS)
colnames(TIPO_DE_CAMBIO_DATOS)
NEW_TIPO_DE_CAMBIO <- TIPO_DE_CAMBIO_DATOS[c("OBSERVACION","TIPO DE CAMBIO NIC")]
View(NEW_TIPO_DE_CAMBIO)
colnames(NEW_TIPO_DE_CAMBIO)
rm(TIPO_DE_CAMBIO_DATOS)

## Nota Aclaratoria: Se tomaron los datos desde el 2007 hasta el I Trimestre - 2025##

Paso 3: Combinar las bases de datos junto con las covariables a trabajarse, una vez cargado todas las variables principales

base_total <- cbind(REMESAS_PARA_R, TASA_DE_DESEMPLEO_TRIMESTRAL, PIB_TRIMESTRAL_REAL_DE_NICARAGUA, NEW_TIPO_DE_CAMBIO)
View(base_total)

## Eliminar las columnas que no son de utilidad en la nueva base de datos

base_total_anuelaa <- base_total[c("Fechas","Nicaragua","LNU04032231","rate","TIPO DE CAMBIO NIC")]
View(base_total_anuelaa)
base_total_karolg <- base_total_anuelaa[c("Nicaragua","LNU04032231","rate","TIPO DE CAMBIO NIC")]

## Otra forma de visualizar los datos de las variables a emplearse
attach(base_total_karolg)
base_total_karolg

 Nicaragua LNU04032231  rate TIPO DE CAMBIO NIC
1      55.03         5.9  3.70           18.11280
2      60.37         5.8  4.85           18.33320
3      63.23         9.4  5.24           18.55960
4      67.90        12.0  6.40           18.78850
5      65.23         8.2  5.84           19.01930
6      69.37         9.9  7.53           19.25150
7      69.07        15.3  3.85           19.48860
8      69.03        21.1 -2.78           19.72820
9      65.30        17.4 -5.29           19.96930
10     63.60        17.1 -6.18           20.21230
11     61.40        22.7 -2.93           20.46200
12     65.83        24.9  1.17           20.72810
13     67.27        20.1  3.77           20.96770
14     65.83        17.2  3.51           21.22300
15     66.97        20.7  4.77           21.48510
16     74.20        20.0  5.48           21.75000
17     71.40        15.6  5.66           22.01610
18     73.73        13.3  7.76           22.28410
19     75.53        16.0  7.25           22.55930
20     83.20        17.2  4.72           22.83780
21     83.37        12.8  9.14           23.11810
22     81.40        11.9  4.66           23.40030
23     82.07        13.5  3.77           23.68850
24     91.23        14.7  8.46           23.97970
25     85.77         9.8  3.46           24.27333
26     88.10         8.5  7.00           24.57000
27     88.57        11.4  6.16           24.87000
28     96.80        11.3  3.27           25.18000
29     92.40         8.2  5.52           25.48646
30     92.67         7.0  4.07           25.79666
31     93.17         8.3  3.59           26.11527
32    100.37         9.5  5.92           26.43724
33     96.40         6.3  4.73           26.76078
34     97.47         5.5  2.95           27.08649
35     97.63         7.5  6.32           27.42104
36    106.30         8.7  5.10           27.75911
37    100.77         4.6  3.88           28.10023
38    102.13         5.2  6.93           28.44319
39    104.30         7.4  3.53           28.79353
40    114.17         8.4  4.05           29.14754
41    107.77         4.5  7.58           29.50376
42    113.57         4.7  3.83           29.86285
43    118.53         5.9  3.25           30.23170
44    123.73         7.4  3.99           30.60441
45    117.80         4.7  2.52           30.97895
46    123.73         4.1 -5.10           31.35600
47    124.27         5.1 -4.34           31.74328
48    134.60         5.2 -6.38           32.13464
49    127.93         4.0 -9.11           32.52790
50    137.60         3.2 -2.20           32.92380
51    143.60         5.0 -1.58           33.33045
52    151.67         6.9  1.58           33.70483
53    141.50        10.1  2.19           33.96408
54    145.37         7.1 -7.27           34.21462
55    160.70         9.6 -1.41           34.46932
56    169.57         8.6 -2.56           34.72051
57    166.80         7.5  3.59           34.91057
58    176.60         4.5 18.53           35.08239
59    175.70         5.0 10.53           35.25763
60    196.53         6.0 10.12           35.43341
61    210.87         3.7  4.62           35.60878
62    254.53         3.4  4.44           35.78405
63    287.40         4.4  3.33           35.96278
64    322.17         5.6  1.97           36.14208
65    340.10         3.6  3.10           36.30182
66    398.23         3.8  3.32           36.39640
67    406.17         4.4  5.98           36.48763
68    408.87         5.4  5.23           36.57892
69    380.30         3.3  6.07           36.62430
70    445.63         3.7  4.20           36.62430
71    450.17         5.2  0.85           36.62430
72    471.60         5.4  3.38           36.62430
73    480.43         3.4  2.98           36.62430

Nota: Esta es la base de datos sin tomar en cuenta las fechas trimestrales con su respectivo control

Base de datos con la fecha trimestral
attach(base_total_anuelaa)
base_total_anuelaa
     Fechas Nicaragua LNU04032231  rate
1    2007-I     55.03         5.9  3.70
2   2007-II     60.37         5.8  4.85
3  2007-III     63.23         9.4  5.24
4   2007-IV     67.90        12.0  6.40
5    2008-I     65.23         8.2  5.84
6   2008-II     69.37         9.9  7.53
7  2008-III     69.07        15.3  3.85
8   2008-IV     69.03        21.1 -2.78
9    2009-I     65.30        17.4 -5.29
10  2009-II     63.60        17.1 -6.18
11 2009-III     61.40        22.7 -2.93
12  2009-IV     65.83        24.9  1.17
13   2010-I     67.27        20.1  3.77
14  2010-II     65.83        17.2  3.51
15 2010-III     66.97        20.7  4.77
16  2010-IV     74.20        20.0  5.48
17   2011-I     71.40        15.6  5.66
18  2011-II     73.73        13.3  7.76
19 2011-III     75.53        16.0  7.25
20  2011-IV     83.20        17.2  4.72
21   2012-I     83.37        12.8  9.14
22  2012-II     81.40        11.9  4.66
23 2012-III     82.07        13.5  3.77
24  2012-IV     91.23        14.7  8.46
25   2013-I     85.77         9.8  3.46
26  2013-II     88.10         8.5  7.00
27 2013-III     88.57        11.4  6.16
28  2013-IV     96.80        11.3  3.27
29   2014-I     92.40         8.2  5.52
30  2014-II     92.67         7.0  4.07
31 2014-III     93.17         8.3  3.59
32  2014-IV    100.37         9.5  5.92
33   2015-I     96.40         6.3  4.73
34  2015-II     97.47         5.5  2.95
35 2015-III     97.63         7.5  6.32
36  2015-IV    106.30         8.7  5.10
37   2016-I    100.77         4.6  3.88
38  2016-II    102.13         5.2  6.93
39 2016-III    104.30         7.4  3.53
40  2016-IV    114.17         8.4  4.05
41   2017-I    107.77         4.5  7.58
42  2017-II    113.57         4.7  3.83
43 2017-III    118.53         5.9  3.25
44  2017-IV    123.73         7.4  3.99
45   2018-I    117.80         4.7  2.52
46  2018-II    123.73         4.1 -5.10
47 2018-III    124.27         5.1 -4.34
48  2018-IV    134.60         5.2 -6.38
49   2019-I    127.93         4.0 -9.11
50  2019-II    137.60         3.2 -2.20
51 2019-III    143.60         5.0 -1.58
52  2019-IV    151.67         6.9  1.58
53   2020-I    141.50        10.1  2.19
54  2020-II    145.37         7.1 -7.27
55 2020-III    160.70         9.6 -1.41
56  2020-IV    169.57         8.6 -2.56
57   2021-I    166.80         7.5  3.59
58  2021-II    176.60         4.5 18.53
59 2021-III    175.70         5.0 10.53
60  2021-IV    196.53         6.0 10.12
61   2022-I    210.87         3.7  4.62
62  2022-II    254.53         3.4  4.44
63 2022-III    287.40         4.4  3.33
64  2022-IV    322.17         5.6  1.97
65   2023-I    340.10         3.6  3.10
66  2023-II    398.23         3.8  3.32
67 2023-III    406.17         4.4  5.98
68  2023-IV    408.87         5.4  5.23
69   2024-I    380.30         3.3  6.07
70  2024-II    445.63         3.7  4.20
71 2024-III    450.17         5.2  0.85
72  2024-IV    471.60         5.4  3.38
73   2025-I    480.43         3.4  2.98
   TIPO DE CAMBIO NIC
1            18.11280
2            18.33320
3            18.55960
4            18.78850
5            19.01930
6            19.25150
7            19.48860
8            19.72820
9            19.96930
10           20.21230
11           20.46200
12           20.72810
13           20.96770
14           21.22300
15           21.48510
16           21.75000
17           22.01610
18           22.28410
19           22.55930
20           22.83780
21           23.11810
22           23.40030
23           23.68850
24           23.97970
25           24.27333
26           24.57000
27           24.87000
28           25.18000
29           25.48646
30           25.79666
31           26.11527
32           26.43724
33           26.76078
34           27.08649
35           27.42104
36           27.75911
37           28.10023
38           28.44319
39           28.79353
40           29.14754
41           29.50376
42           29.86285
43           30.23170
44           30.60441
45           30.97895
46           31.35600
47           31.74328
48           32.13464
49           32.52790
50           32.92380
51           33.33045
52           33.70483
53           33.96408
54           34.21462
55           34.46932
56           34.72051
57           34.91057
58           35.08239
59           35.25763
60           35.43341
61           35.60878
62           35.78405
63           35.96278
64           36.14208
65           36.30182
66           36.39640
67           36.48763
68           36.57892
69           36.62430
70           36.62430
71           36.62430
72           36.62430
73           36.62430

Paso 4: Emplear directorio de Trabajo

# Working Directory

getwd()

[1] "C:/Users/AlfaroVillalobos/Documents/CLASES UNIVERSITARIAS - IEN/ECONOMETRÍA RSTUDIO/Econometria tarea 2/CLASE PRACTICA 2 ECONOMETRIA"

################################################################################
# Paso 5: Estimar el modelo MCO (OLS)
################################################################################

## Nota Aclaratoria de las Variables ##

# 1. Nicaragua = Ingreso de las remesas en Nicaragua (R). 
# 2. LNU04032231 = Tasa de desempleo trimestral (Z). 
# 3. rate = PIB Real Trimestral de Nicaragua (Y). 
# 4. TIPO DE CAMBIO NIC = Tipo de cambio oficial de la moneda nacional con respecto al dólar. 

# Estimando MCO
RegrMult <- lm(`rate` ~ `Nicaragua` + `TIPO DE CAMBIO NIC` + `LNU04032231`, data = base_total_karolg)
RegrMult

Call:
lm(formula = rate ~ Nicaragua + `TIPO DE CAMBIO NIC` + LNU04032231, 
    data = base_total_karolg)

Coefficients:
         (Intercept)             Nicaragua  `TIPO DE CAMBIO NIC`           LNU04032231  
           12.753229              0.009054             -0.314456             -0.209376  

summary(RegrMult)

Call:
lm(formula = rate ~ Nicaragua + `TIPO DE CAMBIO NIC` + LNU04032231, 
    data = base_total_karolg)

Residuals:
     Min       1Q   Median       3Q      Max 
-11.9554  -1.0166   0.4231   1.9962  16.1519 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)  
(Intercept)          12.753229   4.945975   2.579   0.0121 *
Nicaragua             0.009054   0.007260   1.247   0.2166  
`TIPO DE CAMBIO NIC` -0.314456   0.164458  -1.912   0.0600 .
LNU04032231          -0.209376   0.139855  -1.497   0.1389  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.451 on 69 degrees of freedom
Multiple R-squared:  0.05386,	Adjusted R-squared:  0.01272 
F-statistic: 1.309 on 3 and 69 DF,  p-value: 0.2784

Interpretación: El modelo de regresión lineal múltiple por MCO indica que, en su conjunto, las variables explicativas (remesas, tipo de cambio y desempleo) tienen una capacidad predictiva muy limitada sobre el PIB real de Nicaragua, explicando solo el 5.39% de su variabilidad (R² = 0.05386), mientras que ninguna de las variables individuales muestra significancia estadística al nivel convencional del 5% -especialmente las remesas (p = 0.217)-, lo que sugiere problemas fundamentales de especificación, muy probablemente por endogeneidad no controlada donde factores omitidos o bidireccionalidad entre remesas y PIB distorsionan las estimaciones, haciendo insostenible cualquier inferencia causal sobre el efecto de las remesas mediante este enfoque MCO.

## Verificar si hay autocorrelación

#test de Breusch-Godfrey (Autocorrelación)
# H0: no hay autocorrelación (Lo que queremos)
# H1: hay autocorrelación 
bgtest(RegrMult)

Breusch-Godfrey test for serial correlation of order up to 1

data:  RegrMult
LM test = 30.808, df = 1, p-value = 2.849e-08

dwtest(RegrMult)

Durbin-Watson test

data:  RegrMult
DW = 0.7029, p-value = 8.581e-12
alternative hypothesis: true autocorrelation is greater than 0

## Opciones de Gráfico para su respectivo análisis ##

plot(RegrMult)

[![Rplot.png](https://i.postimg.cc/nzvcPrbc/Rplot.png)](https://postimg.cc/47xgKJxr)

Interpretación: Este gráfico de residuos versus valores ajustados muestra que los residuos del modelo OLS no se distribuyen de forma aleatoria alrededor de cero; en su lugar, presentan una clara estructura en forma de "U" o curva, con residuos predominantemente negativos para valores ajustados bajos y altos, y positivos en el centro. Este patrón sistemático, en lugar de una nube de puntos aleatoria, es una señal clásica de que el modelo está mal especificado y confirma el problema de autocorrelación serial que ya fue detectado por los tests de Durbin-Watson y Breusch-Godfrey, lo que viola un supuesto fundamental de MCO y hace que los estimadores sean ineficientes.

[![Rplot01.png](https://i.postimg.cc/L526KS2M/Rplot01.png)](https://postimg.cc/sQHR7tMn)

[![Rplot02.png](https://i.postimg.cc/fyDTnsDN/Rplot02.png)](https://postimg.cc/5YKMLZX7)

Interpretación: El gráfico Scale-Location, que examina la homocedasticidad de los residuos, revela que la dispersión de los residuos estandarizados no es constante a lo largo de los valores ajustados, ya que se observa que la raíz cuadrada del valor absoluto de los residuos tiende a ser mayor en los extremos de los valores ajustados. Este patrón sugiere la presencia de heterocedasticidad, lo que significa que la varianza del error no es constante y por lo tanto los errores estándar de los coeficientes en el modelo OLS podrían estar sesgados, afectando la validez de las pruebas de hipótesis t y F.

[![Rplot03.png](https://i.postimg.cc/mkB2f4BT/Rplot03.png)](https://postimg.cc/G8g1GZ4f)

Interpretación: El gráfico de Residuos vs Leverage indica que, si bien existen algunos puntos con un leverage (palanca) moderadamente alto, ninguno de ellos supera significativamente la distancia de Cook de 0.5 o 1.0, lo que significa que no hay observaciones individuales con un peso excesivo para influir de manera determinante en los resultados de la regresión OLS. Esto sugiere que las estimaciones de los coeficientes son relativamente robustas y no están siendo driven por un outlier extremo o punto de alta influencia.

hist(RegrMult$residuals)
[![Rplot04.png](https://i.postimg.cc/TPVz8Dw9/Rplot04.png)](https://postimg.cc/cKJ9w6fg)

Interpretación: El histograma de los residuos del modelo OLS muestra una distribución que, aunque se aproxima a la forma de una campana, presenta colas ligeramente pesadas y una asimetría, con una frecuencia ligeramente mayor de residuos negativos de gran magnitud. Si bien la distribución no se desvía extremadamente de la normalidad, estas desviaciones menores, combinadas con los fuertes indicios de autocorrelación, contribuyen a que los valores p reportados en el modelo OLS deban interpretarse con precaución.

################################################################################
Paso 6: Estimar el modelo por variables instrumentales (2SLS)
################################################################################

# Primera etapa: Remesas ~ Instrumento + Controles

first_stage <- lm(`Nicaragua` ~ `LNU04032231` + `TIPO DE CAMBIO NIC`, data = base_total_karolg)
base_total_karolg$Nicaragua_hat <- predict(first_stage)

# Segunda etapa: rate ~ Nicaragua_hat + Controles

modelo_iv <- lm(`rate` ~ Nicaragua_hat + `TIPO DE CAMBIO NIC`, data = base_total_karolg)
summary(modelo_iv)

Call:
lm(formula = rate ~ Nicaragua_hat + `TIPO DE CAMBIO NIC`, data = base_total_karolg)

Residuals:
     Min       1Q   Median       3Q      Max 
-12.6872  -0.8815   0.4939   2.5002  15.5020 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)
(Intercept)          -28.80356   24.14781  -1.193    0.237
Nicaragua_hat         -0.13311    0.09506  -1.400    0.166
`TIPO DE CAMBIO NIC`   1.84978    1.36147   1.359    0.179

Residual standard error: 4.468 on 70 degrees of freedom
Multiple R-squared:  0.03253,	Adjusted R-squared:  0.004889 
F-statistic: 1.177 on 2 and 70 DF,  p-value: 0.3143

################################################################################
Paso 7: Comparar ambos modelos econométricos
################################################################################

## Comparación entre MCO y IV ##

stargazer(RegrMult, modelo_iv, type = "text", title = "Comparación MCO vs IV")

Comparación MCO vs IV
==========================================================
                              Dependent variable:         
                     -------------------------------------
                                     rate                 
                            (1)                (2)        
----------------------------------------------------------
Nicaragua                  0.009                          
                          (0.007)                         
                                                          
Nicaragua_hat                                 -0.133      
                                             (0.095)      
                                                          
`TIPO DE CAMBIO NIC`      -0.314*             1.850       
                          (0.164)            (1.361)      
                                                          
LNU04032231                -0.209                         
                          (0.140)                         
                                                          
Constant                  12.753**           -28.804      
                          (4.946)            (24.148)     
                                                          
----------------------------------------------------------
Observations                 73                 73        
R2                         0.054              0.033       
Adjusted R2                0.013              0.005       
Residual Std. Error   4.451 (df = 69)    4.468 (df = 70)  
F Statistic          1.309 (df = 3; 69) 1.177 (df = 2; 70)
==========================================================
Note:                          *p<0.1; **p<0.05; ***p<0.01


###############################################################################
Paso 8: Pruebas de relevancia y validez del instrumento
################################################################################

# Relevancia (First-stage F-statistic)

summary(first_stage)$fstatistic[1]

value 
53.28239

# Test de Hausman (para endogeneidad)

library(lmtest)
resid_first <- residuals(first_stage)
modelo_hausman <- lm(`rate` ~ `Nicaragua` + `TIPO DE CAMBIO NIC` + `LNU04032231` + resid_first, data = base_total_karolg)
summary(modelo_hausman)

Call:
lm(formula = rate ~ Nicaragua + `TIPO DE CAMBIO NIC` + LNU04032231 + 
    resid_first, data = base_total_karolg)

Residuals:
     Min       1Q   Median       3Q      Max 
-11.9554  -1.0166   0.4231   1.9962  16.1519 

Coefficients: (1 not defined because of singularities)
                      Estimate Std. Error t value Pr(>|t|)  
(Intercept)          12.753229   4.945975   2.579   0.0121 *
Nicaragua             0.009054   0.007260   1.247   0.2166  
`TIPO DE CAMBIO NIC` -0.314456   0.164458  -1.912   0.0600 .
LNU04032231          -0.209376   0.139855  -1.497   0.1389  
resid_first                 NA         NA      NA       NA  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.451 on 69 degrees of freedom
Multiple R-squared:  0.05386,	Adjusted R-squared:  0.01272 
F-statistic: 1.309 on 3 and 69 DF,  p-value: 0.2784

###############################################################################
Paso 9: Gráfico Comparativo
###############################################################################

#comparacion DOG :'V
#extraer los datos de los modelos 
tidy_ols <- tidy(RegrMult, conf.int = TRUE) %>% mutate(Modelo = "OLS")
tidy_iv  <- tidy(modelo_iv, conf.int = TRUE)  %>% mutate(Modelo = "IV (2SLS)")

#unir y filtrar
resultados <- bind_rows(tidy_ols, tidy_iv) %>%
  filter(term != "(Intercept)")

#Gráfico
ggplot(resultados, aes(x = estimate, y = term, color = Modelo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                position = position_dodge(width = 0.5), 
                width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Comparación de Coeficientes: OLS vs IV",
       subtitle = "Nótese la inmensa incertidumbre (barra larga) en el modelo IV",
       x = "Estimación del Efecto",
       y = "Variables") +
  theme_minimal() +
  theme(legend.position = "bottom")
[![Rplot05.png](https://i.postimg.cc/Zq8GkdR7/Rplot05.png)](https://postimg.cc/VSNGn5Xn)

Interpretación: Este gráfico de comparación visual entre los coeficientes del modelo OLS y el modelo IV es muy revelador, ya que muestra que el coeficiente para la variable de interés (remesas) en el modelo IV (Nicaragua_hat) no solo es negativo en contraste con el positivo de MCO, sino que su intervalo de confianza es extremadamente amplio, cruzando ampliamente el cero. Esta "inmensa incertidumbre" ilustra perfectamente la principal desventaja del enfoque IV: aunque intenta corregir la endogeneidad, la estimación resultante es muy imprecisa y poco fiable, lo que nos impide concluir cualquier efecto significativo de las remesas sobre la tasa de crecimiento.

## Respectivas Conclusiones ##
Contestando las preguntas de la clase práctica

Pregunta 1: Estime el efecto de las remesas utilizando OLS y IV - TSLS. Compare. ¿Cuál es preferible?
En el modelo de Mínimos Cuadrados Ordinarios (OLS), el coeficiente para las remesas ("Nicaragua") es de 0.009, con un valor p de 0.2166, lo que indica que no es estadísticamente significativo a niveles convencionales (p < 0.05). El modelo OLS tiene un R-cuadrado bajo (0.054) y un estadístico F no significativo (p = 0.2784), sugiriendo que el modelo en conjunto no explica adecuadamente la variación en la tasa de crecimiento del PIB. Además, las pruebas de Durbin-Watson (DW = 0.7029, p = 8.581e-12) y Breusch-Godfrey (LM = 30.808, p = 2.849e-08) revelan autocorrelación positiva en los residuos, violando un supuesto clave de OLS y haciendo que las estimaciones sean ineficientes y los errores estándar potencialmente sesgados. Por otro lado, en el modelo de Variables Instrumentales (IV-TSLS), que utiliza la tasa de desempleo en el sector construcción de EE.UU. como instrumento, el coeficiente para las remesas instrumentadas ("Nicaragua_hat") es -0.133, pero tampoco es significativo (p = 0.166). El modelo IV tiene un R-cuadrado aún más bajo (0.033) y un estadístico F no significativo (p = 0.3143). Aunque el instrumento es fuerte (estadístico F de la primera etapa = 53.28), la falta de significancia en el coeficiente de las remesas y los problemas de autocorrelación en OLS sugieren que IV-TSLS podría ser teóricamente preferible si se asume endogeneidad en las remesas, pero en la práctica, ninguno de los modelos proporciona evidencia sólida de un efecto significativo de las remesas sobre el crecimiento del PIB. Dada la autocorrelación en OLS, IV-TSLS es preferible para evitar sesgos por endogeneidad, aunque los resultados no sean concluyentes.

Pregunta 2: ¿Qué supuestos son relevantes para preferir IV - TSLS? ¿Se cumplen? Argumente.
Los supuestos clave para preferir IV-TSLS sobre OLS son: (1) Relevancia del instrumento: el instrumento debe estar correlacionado con la variable endógena (remesas). En este caso, el estadístico F de la primera etapa es 53.28, muy por encima de 10, lo que indica que el instrumento (tasa de desempleo en el sector construcción de EE.UU.) es fuerte y cumple con el supuesto de relevancia. (2) Exogeneidad del instrumento: el instrumento no debe estar correlacionado con el término de error en la ecuación principal. Esto no puede testearse directamente, pero teóricamente, el desempleo sectorial en EE.UU. afecta las remesas a través de los ingresos de los trabajadores nicaragüenses en ese sector, sin influir directamente en el crecimiento del PIB de Nicaragua, especialmente al incluir controles como el tipo de cambio. Sin embargo, la posible omisión de variables relevantes (como el crecimiento del PIB de EE.UU.) podría violar este supuesto. (3) Identificación: debe haber al menos un instrumento por variable endógena, lo que se cumple aquí. Además, la endogeneidad de las remesas es un supuesto implícito; aunque no se testea formalmente (el test de Hausman no pudo realizarse por singularidades), la autocorrelación en los residuos de OLS sugiere posibles omisiones que justifican el uso de IV. En conclusión, el supuesto de relevancia se cumple claramente, pero la exogeneidad no puede verificarse plenamente, por lo que IV-TSLS es preferible con cautela.

Pregunta 3: ¿Por qué cree que se sugiere utilizar la tasa de desempleo sectorial de US y no la tasa de desempleo de todos los sectores (tasa de desempleo)?
Se sugiere utilizar la tasa de desempleo sectorial de la construcción en EE.UU. en lugar de la tasa de desempleo general porque los trabajadores nicaragüenses en EE.UU. están concentrados en sectores específicos, como la construcción, donde las fluctuaciones en el empleo afectan directamente sus ingresos y, por ende, las remesas que envían a Nicaragua. El instrumento sectorial es más relevante para capturar variaciones en las remesas, ya que la tasa de desempleo general incluye sectores no relacionados con la migración nicaragüense, diluyendo su poder explicativo. Además, el instrumento sectorial probablemente sea más exógeno: el desempleo en la construcción está influenciado por factores específicos (como la actividad inmobiliaria) que no afectan directamente el crecimiento de Nicaragua, reduciendo el riesgo de correlación con omisiones en el error. La tasa de desempleo general, en cambio, podría correlacionarse con shocks macroeconómicos globales que impactan a Nicaragua a través de canales distintos a las remesas (como comercio o inversión), violando el supuesto de exogeneidad. Así, el instrumento sectorial ofrece una mejor combinación de relevancia y exogeneidad para identificar el efecto causal de las remesas.

                                                                          MUCHÍSIMAS GRACIAS POR SU ATENCIÓN!
