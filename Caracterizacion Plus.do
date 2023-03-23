**CARACTERIZACION + POBLACION EN PROCESO DE INCLUSION**

clear all
set mem 300m
glo enaho "G:\BM-BACKUP\Bases de datos\ENAHO\2010\ENAHO 2010\Bases"
glo endes "G:\BM-BACKUP\Bases de datos\ENDES\ENDES 2010\Bddatos\Español"
glo temp "G:\MIDIS\Cálculo y caracterización de poblacion MIDIS\Temp"

*1) INDICADORES DEMOGRAFICOS
*****************************

use "$enaho\enaho01-2010-200.dta"
keep if p204==1
keep if p203!=9
keep if p203!=10
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable)

recode dominio (1/3=1) (4/6=2) (7=3) (8=4), gen(reg_nat)
*label define reg_nat 1"Costa" 2"Sierra" 3"Selva" 4"Lima_Met"
label values reg_nat reg_nat

gen miembro14=1 if p208a<14
replace miembro14=0 if p208a>=14

gen miembro05=1 if p208a<5
replace miembro05=0 if p208a>=5

gen miembro65=1 if p208a>=65
replace miembro65=0 if p208a<65

bys conglome vivienda hogar: egen miembros_tot=sum(p204)
bys conglome vivienda hogar: egen miembros_14=sum(miembro14)
bys conglome vivienda hogar: egen miembros_05=sum(miembro05)
bys conglome vivienda hogar: egen miembros_65=sum(miembro65)

gen miembros_rango=1 if miembros_tot>=1 & miembros_tot<=2
replace miembros_rango=2 if miembros_tot>=3 & miembros_tot<=5
replace miembros_rango=3 if miembros_tot>=5 & miembros_tot<=20
label define miembros 1"Dos_menos" 2"Tres_cinco" 3"Mas_cinco"
label values miembros_rango miembros

gen miembros_rango14=1 if miembros_14==0
replace miembros_rango14=2 if miembros_14>=1 & miembros_14<=2
replace miembros_rango14=3 if miembros_14>=3 & miembros_14<=10
label define miembros14 1"Ninguno" 2"Uno_dos" 3"Tres_mas"
label values miembros_rango14 miembros14

gen miembros_rango05=1 if miembros_05==0
replace miembros_rango05=2 if miembros_05==1
replace miembros_rango05=3 if miembros_05>=2 & miembros_05<=5
label define miembros05 1"Ninguno" 2"Uno" 3"Dos_mas"
label values miembros_rango05 miembros05

gen miembros_rango65=1 if miembros_65==0
replace miembros_rango65=2 if miembros_65==1
replace miembros_rango65=3 if miembros_65>=2 & miembros_65<=5
label define miembros65 1"Ninguno" 2"Uno" 3"Dos_mas"
label values miembros_rango65 miembros65

gen jefe_sexo=1 if p203==1 & p207==1
replace jefe_sexo=2 if p203==1 & p207==2
label define jefe_sexo 1"Jefe_hombre" 2"Jefa_mujer"
label values jefe_sexo jefe_sexo

gen edadjefe_rango=1 if p203==1 & p208a>=15 & p208a<30
replace edadjefe_rango=2 if p203==1 & p208a>=30 & p208a<65
replace edadjefe_rango=3 if p203==1 & p208a>=65 & p208a<=98
label define edadjefe_rango 1"Menos_30" 2"30_64" 3"65_mas"
label values edadjefe_rango edadjefe_rango

gen edad_rango=1 if p208a>=0 & p208a<5
replace edad_rango=2 if p208a>=5 & p208a<14
replace edad_rango=3 if p208a>=14 & p208a<18
replace edad_rango=4 if p208a>=18 & p208a<35
replace edad_rango=5 if p208a>=35 & p208a<65
replace edad_rango=6 if p208a>=65 & p208a<100
label define edad_rango 1"Menos_cinco" 2"5_13" 3"14_17" 4"18_34" 5"35_64" 6"65_mas"
label values edad_rango edad_rango

g conyuge= p203==2
sort conglome vivienda hogar codperso 
by conglome vivienda hogar: egen num_conyuge=sum(conyuge)

g parejajefe=1 if jefe_sexo==1 & num_conyuge==1
replace parejajefe=2 if jefe_sexo==2 & num_conyuge==1
replace parejajefe=3 if jefe_sexo==1 & num_conyuge==0
replace parejajefe=4 if jefe_sexo==2 & num_conyuge==0
label define parejajefe 1"Jefe_hombre_cony" 2"Jefa_mujer_cony" 3"Jefe_hombre_solo" 4"Jefa_mujer_sola"
label values parejajefe parejajefe

save "$temp\enaho01-2010-200_midis.dta", replace

log using "$temp\indicadores_demograficos.smcl", name(Indicadores_Demograficos) replace
set more off
svyset conglome [pw=facpob07],strata(estrato)

svy: tab p207 hogar_vulnerable, col
svy: tab edad_rango hogar_vulnerable, col
svy: tab edad_rango hogar_vulnerable if p207==1, col
svy: tab edad_rango hogar_vulnerable if p207==2, col

svy: tab miembros_rango hogar_vulnerable if p203==1, col
svy: tab miembros_rango05 hogar_vulnerable if p203==1, col
svy: tab miembros_rango14 hogar_vulnerable if p203==1, col
svy: tab miembros_rango65 hogar_vulnerable if p203==1, col

tabstat miembros_tot  miembros_05 miembros_14 miembros_65 [w=facpob07], stats(mean) by(hogar_vulnerable)

tabstat p208a [w=facpob07] if p203==1,stats(mean) by(hogar_vulnerable)
tabstat p208a [w=facpob07] if jefe_sexo==1,stats(mean) by(hogar_vulnerable)
tabstat p208a [w=facpob07] if jefe_sexo==2,stats(mean) by(hogar_vulnerable)

svy: tab jefe_sexo hogar_vulnerable if p203==1, col
svy: tab  edadjefe_rango hogar_vulnerable, col

svy: tab  p208b1 hogar_vulnerable, col
svy: tab  p208d1 hogar_vulnerable, col
svy: tab  p208d1 hogar_vulnerable if p208a>=65, col

svy: tab p209 hogar_vulnerable if p203==1, col
svy: tab p209 hogar_vulnerable if jefe_sexo==1, col
svy: tab p209 hogar_vulnerable if jefe_sexo==2, col

svy: tab parejajefe hogar_vulnerable, col
svy: tab p209 hogar_vulnerable if jefe_sexo==1, col
svy: tab p209 hogar_vulnerable if jefe_sexo==2, col

log close Indicadores_Demograficos

*2) INDICADORES VIVIENDA/HOGAR
******************************

use "$enaho\enaho01-2010-100.dta", clear
keep if result==1 | result==2
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable mieperho)

recode dominio (1/3=1) (4/6=2) (7=3) (8=4), gen(reg_nat)
*label define reg_nat 1"Costa" 2"Sierra" 3"Selva" 4"Lima_Met"
label values reg_nat reg_nat

gen conexion_agua=1 if p110==1 | p110==2 | p110==3
replace conexion_agua=0 if p110>3 & p110!=.
label define agua 1"Agua_segura" 0"Agua_NO_segura"
label values  conexion_agua agua

gen conexion_saneam=1 if p111==1 | p111==2 | p111==3
replace conexion_saneam=0 if p111>3 & p111!=.
label define saneam 1"Saneam_segura" 0"Saneam_NO_segura"
label values  conexion_saneam saneam

gen conexion_luz=1 if p1121==1 
replace conexion_luz=0 if p1121==0
label define luz 1"Tiene_electricidad" 0"NO_tiene_electricidad"
label values  conexion_luz luz 

gen conexion_telefono=1 if p1141==1 | p1142==1
replace conexion_telefono=0 if p1141==0 & p1142==0
label define telefono 1"Tiene_telefono" 0"NO_tiene_telefono"
label values  conexion_telefono telefono 

egen num_servicios=rsum(conexion*)

gen miembro_hab=mieperho/p104

gen hog_hacinado=1 if miembro_hab>2.5
replace hog_hacinado=0 if miembro_hab<=2.5

gen hog_hacinado2=1 if miembro_hab>3
replace hog_hacinado2=0 if miembro_hab<=3

sort conglome vivienda hogar
save "$temp\enaho01-2010-100_midis.dta", replace

svyset conglome [pw=factor07],strata(estrato)

svy: tab num_servicios hogar_vulnerable, col
foreach  x in agua saneam luz telefono {
svy: tab  conexion_`x' hogar_vulnerable, col
}

svy: tab p102 hogar_vulnerable, col
svy: tab p103 hogar_vulnerable, col
svy: tab p103a hogar_vulnerable, col

svy: tab hog_hacinado hogar_vulnerable, col
svy: tab hog_hacinado2 hogar_vulnerable, col

tabstat miembro_hab [w=factor07],stats(mean) by(hogar_vulnerable)
svy: tab p106a hogar_vulnerable, col
svy: tab p113a hogar_vulnerable, col
forvalues  x=1/4  {
svy: tab  p114`x' hogar_vulnerable, col
}

save "$temp\enaho01-2010-100_midis.dta", replace

**EQUIPAMIENTO DEL HOGAR

use "$enaho\enaho01-2010-612.dta", clear
keep conglome vivienda hogar p612n p612
reshape wide p612,i(conglome vivienda hogar) j(p612n)
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable mieperho factor07 dominio estrato)

forvalues  x=1/26  {
recode  p612`x' (2=0) (1=1)
}

egen num_eqpos=rsum(p612*)

gen eqpos_rango=1 if num_eqpos==0
replace eqpos_rango=2 if num_eqpos==1
replace eqpos_rango=3 if num_eqpos>=2 & num_eqpos<=5
replace eqpos_rango=4 if num_eqpos>=6 & num_eqpos<=10
replace eqpos_rango=5 if num_eqpos>=11 & num_eqpos<=26
label define eqpos_rango 1"Ninguno" 2"1" 3"Entre_2_5" 4"Entre_6_10" 5"Mas_de_10"
label values eqpos_rango eqpos_rango

gen solo_radio=1 if p6121==1 & num_eqpos==1
gen radio_tvcolor=1 if p6121==1 & p6122==1 & num_eqpos==2
gen radio_tvcolor_cocinagas=1 if p6121==1 & p6122==1 & p61210==1 & num_eqpos==3

svyset conglome [pw=factor07],strata(estrato)

forvalues  x=1/26  {
svy: tab  p612`x' hogar_vulnerable, col
}

svy: tab num_eqpos hogar_vulnerable, col
svy: tab eqpos_rango hogar_vulnerable, col
svy: tab solo_radio hogar_vulnerable, count format(%12.0g)
svy: tab radio_tvcolor hogar_vulnerable, count format(%12.0g)

save "$temp\enaho01-2010-612_midis.dta", replace

*3) INDICADORES EDUCACION
*************************

use "$enaho\enaho01a-2010-300.dta", clear
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable)
keep if p204==1

gen      anos_educ = 0            if p301a==1 | p301a==2
replace  anos_educ = p301b        if p301a==3 | p301a==4
replace  anos_educ = p301c        if p301b==0 & (p301a==3 | p301a==4)
replace  anos_educ = p301b + 6    if p301a==5 | p301a==6  
replace  anos_educ = p301b + 11   if p301a==7 | p301a==8 | p301a==9 | p301a==10 
replace  anos_educ = p301b + 16   if p301a==11

gen nivel_educ=1 if p301a==1 | p301a==2 | p301a==3
replace nivel_educ=2 if p301a==4 | p301a==5
replace nivel_educ=3 if p301a==6
replace nivel_educ=4 if p301a==7 | p301a==8 | p301a==9 | p301a==10 | p301a==11
label define nivel_educ 1"Prim_incomp_menos" 2"Prim_comp_sec_incomp" 3"Sec_comp" 4"Superior"
label values  nivel_educ  nivel_educ

gen jhcony_sexo=1 if p203==1 & p207==1
replace jhcony_sexo=1 if p203==2 & p207==1
replace jhcony_sexo=2 if p203==1 & p207==2
replace jhcony_sexo=2 if p203==2 & p207==2
label define jhcony_sexo 1"Jefe_conyuge_hombre" 2"Jefa_conyuge_mujer"
label values jhcony_sexo jhcony_sexo

gen alfabetismo=1 if p208a>=15 & nivel_educ==2 | nivel_educ==3 | nivel_educ==4
replace alfabetismo=1 if p208a>=15 & nivel_educ==1 & p302==1 & p302x==1
replace alfabetismo=0 if p208a>=15 & nivel_educ==1 & p302==1 & p302x==2
replace alfabetismo=0 if p208a>=15 & nivel_educ==1 & p302==2

svyset conglome [pw=factor07],strata(estrato)

tabstat anos_educ [w=factor07] if (p203==1 | p203==2),stats(mean) by(hogar_vulnerable)
tabstat anos_educ if jhcony_sexo==1 [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat anos_educ if jhcony_sexo==2 [w=factor07],stats(mean) by(hogar_vulnerable)

tabstat anos_educ [w=factor07] if p208a>=25 & p208a<=34,stats(mean) by(hogar_vulnerable)
tabstat anos_educ if p207==1 & p208a>=25 & p208a<=34 [w=factor07] ,stats(mean) by(hogar_vulnerable)
tabstat anos_educ if p207==2 & p208a>=25 & p208a<=34 [w=factor07] ,stats(mean) by(hogar_vulnerable)

tabstat anos_educ [w=factor07] if p208a>=16 & p208a<=24,stats(n mean) by(hogar_vulnerable)
tabstat anos_educ if p207==1 & p208a>=16 & p208a<=24 [w=factor07] ,stats(n mean) by(hogar_vulnerable)
tabstat anos_educ if p207==2 & p208a>=16 & p208a<=24 [w=factor07] ,stats(n mean) by(hogar_vulnerable)

svy: tab nivel_educ hogar_vulnerable if p203==1, col
svy: tab nivel_educ hogar_vulnerable if jhcony_sexo==1, col
svy: tab nivel_educ hogar_vulnerable if jhcony_sexo==2, col

svy: tab alfabetismo hogar_vulnerable, col
svy: tab alfabetismo hogar_vulnerable if p207==1, col
svy: tab alfabetismo hogar_vulnerable if p207==2, col

svy: tab p301a hogar_vulnerable if p208a>=12 & p208a<=13 , col
svy: tab p301a hogar_vulnerable if p208a>=12 & p208a<=13 & p207==1 , col
svy: tab p301a hogar_vulnerable if p208a>=12 & p208a<=13 & p207==2 , col

svy: tab nivel_educ hogar_vulnerable if p208a>=17 & p208a<=18, col
svy: tab nivel_educ hogar_vulnerable if p208a>=17 & p208a<=18 & p207==1, col
svy: tab nivel_educ hogar_vulnerable if p208a>=17 & p208a<=18 & p207==2, col

svy: tab p314a hogar_vulnerable, col
svy: tab p314a hogar_vulnerable if p207==1, col
svy: tab p314a hogar_vulnerable if p207==2, col

svy:	tab	p305	hogar_vulnerable	if	p304a==2	,	col
svy:	tab	p305	hogar_vulnerable	if	p304a==3	,	col

save "$temp\enaho01a-2010-300_midis.dta", replace


*4) EMPLEO
************

use "$enaho\enaho01a-2010-500.dta", clear
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable)
keep if p204==1

gen jhcony_sexo=1 if p203==1 & p207==1
replace jhcony_sexo=1 if p203==2 & p207==1
replace jhcony_sexo=2 if p203==1 & p207==2
replace jhcony_sexo=2 if p203==2 & p207==2
label define jhcony_sexo 1"Jefe_conyuge_hombre" 2"Jefa_conyuge_mujer"
label values jhcony_sexo jhcony_sexo

***Ocupaciones
***************

gen cod_ocup1=floor(p505/100) if p505>=100
replace cod_ocup1=0 if p505<100

gen cod_ocup2=floor(p505/10)

#d;
lab def cod_ocup1
0 "FUERZAS ARMADAS Y POLICIALES"
1 "MIEMBROS DEL PODER EJECUTIVO Y DE LOS CUERPOS LEGISLATIVOS"
2 "PROFESIONALES, CIENTIFICOS E INTELECTUALES"
3 "TECNICOS DE NIVEL MEDIO Y TRABAJADORES ASIMILADOS"
4 "JEFES Y EMPLEADOS DE OFICINA"
5 "TRABAJADORES CALIFICADOS DE LOS SERVICIOS PERSONALES, PROTECCION,SEGURIDAD Y VENDEDORES DEL COMERCIO Y MERCADO"
6 "AGRICULTORES (EXPLOTADORES); TRABAJADORES CALIFICADOS AGROPECUARIOS,PESQUEROS"
7 "OBREROS, OPERADORES DE LAS ACTIVIDADES DE MINAS, CANTERAS,PETROLEO,INDUSTRIAS MANUFACT(NO INCLUYE CONFECCION DE PROD.DE PAPEL Y CARTON,ELABORACION DE PRODUCTOS DE CAUCHO Y PLASTICO,ARTES GRAFIC.FABRIC.DE INSTRUMENTOS DE MUSICA) Y OTROS"
8 "OBREROS DE LA CONSTRUCCION, CONFECCIONADORES DE PRODUCTOS DE PAPEL Y CARTON, TRAB.DEL CAUCHO Y PLASTICO,DE LAS ARTES GRAFICAS,FABR. DE INSTRUMENT.DE MUSICA,PINTORES,CONDUCTORES DE MAQ. Y MEDIOS DE TRANSPORTE(EXCEPTO A PEDAL Y A MANO) Y OTROS AFINES"
9 "NO ESPECIFICADOS - TRABAJADORES NO CALIFICADOS DE LOS SERVICIOS; PEONES AGROPECUARIOS,FORESTALES DE LA PESCA,DE LAS MINAS Y CANTERAS,IND.MANUFACTURERAS,CONSTRUCCION,PEONES DE CARGA Y VENDEDORES AMBULANTES Y OTROS AFINES.";

lab def cod_ocup2
01 "FUERZA ARMADAS"
02 "FUERZAS POLICIALES"
11 "MIEMBROS DEL PODER EJECUTIVO Y DE LOS CUERPOS LEGISLATIVOS,"
12 "DIRECTORES DE EMPRESA (CON 3 O MAS DIRECTORES)"
13 "DIRECTORES DE EMPRESA (CON 3 O MAS DIRECTORES)"
14 "GERENTE DE PEQUE?AS EMPRESAS (TALLERES)"
21 "PROFESIONALES DE LAS CIENCIAS FISICAS, QUIMICAS, MATEMATICA"
22 "PROFESIONALES DE LAS CIENCIAS FISICAS, QUIMICAS, MATEMATICA"
23 "PROFESIONALES DE LAS CIENCIAS BIOLOGICAS, LA MEDICINA Y LA"
24 "PROFESORES (MAESTROS Y/O PEDAGOGOS)"
25 "PROFESIONALES DEL DERECHO, CIENCIAS ECONOMICAS, ADMINISTRAT"
26 "PROFESIONALES DEL DERECHO, CIENCIAS ECONOMICAS, ADMINISTRAT"
27 "PROFESIONALES DE LAS ARTES, ESCULTURA, MUSICA Y AFINES"
28 "OTROS PROFESIONALES Y TRABAJADORES VARIOS"
31 "TECNICOS DE NIVEL MEDIO EN CIENCIAS FISICAS, QUIMICAS, MATE"
32 "OPERADORES DE EQUIPOS ESPECIALIZADOS, FOTOGRAFOS Y AFINES"
33 "TECNICOS EN NAVEGACION MARITIMA Y AERONAUTICA E INSPECTORES"
34 "TECNICOS DE NIVEL MEDIO Y TRABAJADORES ASIMILADOS DE LAS CI"
35 "TECNICOS DE NIVEL MEDIO Y TRABAJADORES ASIMILADOS DE LAS CI"
36 "JEFES DE VENTAS Y TECNICOS EN ADMINISTRACION, CONTABILIDAD,"
37 "AGENTES DE SERVICIOS ADMINISTRATIVOS"
38 "ASISTENTES Y AUXILIARES EN LA ADMINISTRACION"
39 "ARTISTAS AFINES, TRABAJADORES DEL ESPECTACULO, ATLETAS Y AU"
41 "JEFES DE DEPENDENCIAS ADMINISTRATIVAS (EXC. DIR.GRALS., GER"
42 "PERSONAL ADMINISTRATIVO Y EMPLEADOS AFINES"
43 "JEFES DE SERVICIOS DE TRANSPORTES Y TRABAJADORES AFINES"
44 "JEFES DE SERVICIOS DE CORREOS; EMPLEADOS DE BIBLIOTECAS, DE"
45 "CAJEROS, RECEPCIONISTAS Y TRABAJADORES ASIMILADOS"
46 "EMPLEADOS DE OFICINA EN OPERACION DE CAMPO Y OTROS OFICINIS"
51 "PERSONAL AL SERVICIO DIRECTO DE LOS PASAJEROS"
52 "JEFES, ECONOMOS Y MAYORDOMOS"
53 "TRABAJADORES DE LOS CUIDADOS PERSONALES"
54 "PELUQUEROS, ESPECIALISTAS EN TRAT.DE BELLEZA Y TRABAJADORES"
55 "TRABAJADORES DE SERVICIOS VARIOS"
56 "PERSONAL DE LOS SERVICIOS DE PROTECCION Y SEGURIDAD"
57 "COMERCIANTES VENDEDORES AL POR MAYOR Y POR MENOR"
58 "COMERCIANTES Y VENDEDORES (NO AMBULATORIO) NO CLASIFICADOS"
61 "AGRICULTORES (EXPLOTADORES) Y TRABAJADORES CALIFICADOS DE C"
62 "CRIADORES Y TRABAJADORES PECUARIOS CALIFICADOS DE LA CRIA D"
63 "PESCADORES, CAZADORES Y TRAMPEROS"
64 "TRABAJADORES AGROPECUARIOS"
71 "MINEROS, CANTEROS, SONDISTAS Y TRABAJADORES ASIMILADOS"
72 "OBREROS DE TRATAMIENTO DE LA MADERA Y DE LA FABRICACION DE"
73 "EROS DE LOS TRATAMIENTOS QUIMICOS Y TRABAJADORES ASIMILADOS"
74 "HILANDEROS, TEJEDORES, TINTOREROS Y TRABAJADORES ASIM."
75 "OBREROS DE LA PREPARACION, CURTIDO Y TRATAMIENTO DE PIELES"
76 "OBREROS DE LA PREPARACION DE ALIMENTOS Y BEBIDAS"
77 "OBREROS DEL CALZADO, SASTRES, MODISTOS, PELETEROS, EBANISTA"
78 "OBREROS, MECANICOS Y AJUSTADORES DE METALES, EQUIPOS ELECTR"
79 "AJUSTADORES, MONTADORES E INSTALADORES DE MAQUINAS E INSTRU"
81 "OBREROS DE LA FABRICACION DE PRODUCTOS DE CAUCHO Y PLASTICO"
82 "CONFECCIONADORES DE PRODUCTOS DE PAPEL Y CARTON"
83 "OBREROS DE LAS ARTES GRAFICAS"
84 "OBREROS MANUFACTUR. Y TRAB.ASIM.NO CLASIFICADOS EN O.C."
85 "PINTORES"
86 "OBREROS DE LA CONSTRUCCION"
87 "OPERADORES DE MAQUINAS AGRICOLAS, FIJAS Y DE INSTALACIONES"
88 "CONDUCTORES DE MEDIOS DE TRANSPORTE Y PERSONAS EN OCUPACION"
91 "VENDEDORES AMBULANTES"
92 "VENDEDORES AMBULANTES"
93 "COBRADORES Y VENDEDORES DE LOS SERVICIOS DE TRANSPORTE Y AF"
94 "PERSONAL DOMESTICO, LIMPIADORES, LAVANDEROS, PLANCHADORES Y"
95 "MENSAJEROS, REPARTIDORES, PORTEROS Y AFINES"
96 "RECOLECTORES DE BASURA Y AFINES"
97 "PEONES AGROPECUARIOS FORESTALES, DE LA PESCA Y AFINES"
98 "PEONES DE LA MINERIA, SUMINISTRO ELECTRICIDAD, GAS Y AGUA,"
99 "OCUPACION NO ESPECIFICA (N.E.)";
#d cr

lab val cod_ocup1 cod_ocup1
lab val cod_ocup2 cod_ocup2

***Número de personas en la firma
**********************************

gen tam_emp=1 if p512b==1
replace tam_emp=2 if p512b>1 & p512b<=5
replace tam_emp=3 if p512b>5 & p512b<=10
replace tam_emp=4 if p512b>10 & p512b<=50
replace tam_emp=5 if p512b>50 & p512b<=10000

lab def tam_emp 1 "Autoempleo" 2 "De_2_a_5_empleados" 3 "De_6_a_10_empleados"  4 "De_11_a_50_empleados"  5 "Más_de_50_empleados"
lab val tam_emp tam_emp


***Tiempo en la ocupación
**************************

gen exp_ocup=1 if p513a1==0 & p513a2>=1 & p513a2<=11
replace exp_ocup=2 if p513a1>=1 & p513a1<5
replace exp_ocup=3 if p513a1>=5 & p513a1<10
replace exp_ocup=4 if p513a1>=10

lab def exp_ocup 1 "Menos de 1 año" 2 "De 1 a 5 años" 3 "De 5 a 10 años" 4 "Más de 10 años"
lab val exp_ocup exp_ocup

***Horas trabajadas en la semana
********************************
gen hor_trab=1 if i520==0
replace hor_trab=2 if i520>0 & i520<=20
replace hor_trab=3 if i520>20 & i520<=40
replace hor_trab=4 if i520>40 & i520<=60
replace hor_trab=5 if i520>60 & i520<=100

lab def hor_trab 1 "Cero" 2 "De_1_a_20_horas" 3 "De_21_a_40_horas"  4 "De_41_a_60_horas"  5 "Más_de_60_horas"
lab val hor_trab hor_trab

***Ingreso laboral
**************************

egen ing_act_prinm=rsum(i524a1 i530a)
egen ing_act_prine=rsum(d529t  d536)
egen ing_tot_act_prin=rsum(ing_act_prinm  ing_act_prine)

egen ing_act_secm=rsum(i538a1 i541a)
egen ing_act_sece=rsum(d540t  d543)
egen ing_tot_act_sec=rsum(ing_act_secm  ing_act_sece)

egen ing_act_mo=rsum(ing_act_prinm ing_act_secm)
egen ing_act_es=rsum(ing_act_prine ing_act_sece)
egen ing_act_to=rsum(ing_act_mo ing_act_es)

gen ing_act_mon=ing_act_mo/12
gen ing_act_esp=ing_act_es/12
gen ing_act_tot=ing_act_to/12

***Afiliación a pensión
**************************

forvalues  x=1/5  {
svy: tab  p558a`x' hogar_vulnerable, col
}

recode p558a2 (2=1)
recode p558a3 (3=1)
recode p558a4 (4=1)
recode p558a5 (5=1)

egen num_pension=rsum(p558a1 p558a2 p558a3 p558a4)

gen pension=1 if p558a5==0
replace pension=0 if p558a5==1
lab def pension 1 "Afiliado" 0 "No_afiliado"
lab val pension pension


svyset conglome [pw=fac500a0],strata(estrato)

svy: tab ocu500 hogar_vulnerable if (p203==1 | p203==2), col
svy: tab ocu500 hogar_vulnerable if jhcony_sexo==1, col
svy: tab ocu500 hogar_vulnerable if jhcony_sexo==2, col

svy: tab ocu500 hogar_vulnerable, col
svy: tab ocu500	hogar_vulnerable if	p208a>=25,	col
svy: tab p546 hogar_vulnerable	if	ocu500==4,	col
svy: tab p546 hogar_vulnerable	if	ocu500==4 & p208a>=25,	col

svy: tab  cod_ocup1 hogar_vulnerable if (p203==1 | p203==2) & ocu500==1, col
svy: tab  cod_ocup1 hogar_vulnerable if jhcony_sexo==1 & ocu500==1, col
svy: tab  cod_ocup1 hogar_vulnerable if jhcony_sexo==2 & ocu500==1, col

svy: tab  cod_ocup2 hogar_vulnerable if (p203==1 | p203==2) & ocu500==1, col
svy: tab  cod_ocup2 hogar_vulnerable if jhcony_sexo==1 & ocu500==1, col
svy: tab  cod_ocup2 hogar_vulnerable if jhcony_sexo==2 & ocu500==1, col

svy: tab  cod_ocup1 hogar_vulnerable if p203!=1 & p203!=2 & ocu500==1, col
svy: tab  cod_ocup1 hogar_vulnerable if p203!=1 & p203!=2 & p207==1 & ocu500==1, col
svy: tab  cod_ocup1 hogar_vulnerable if p203!=1 & p203!=2 & p207==2 & ocu500==1, col

svy: tab  cod_ocup2 hogar_vulnerable if p203!=1 & p203!=2 & ocu500==1, col
svy: tab  cod_ocup2 hogar_vulnerable if p203!=1 & p203!=2 & p207==1 & ocu500==1, col
svy: tab  cod_ocup2 hogar_vulnerable if p203!=1 & p203!=2 & p207==2 & ocu500==1, col

svy: tab p507 hogar_vulnerable if ocu500==1, col
svy: tab p507 hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab p507 hogar_vulnerable if p207==2 & ocu500==1, col

forvalues  x=1/12 {
svy: tab  p511`x' hogar_vulnerable if ocu500==1, col
}

forvalues  x=1/12 {
svy: tab  p511`x' hogar_vulnerable if ocu500==1 & p207==1, col
}

forvalues  x=1/12 {
svy: tab  p511`x' hogar_vulnerable if ocu500==1 & p207==2, col
}

svy: tab tam_emp hogar_vulnerable if ocu500==1, col
svy: tab tam_emp hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab tam_emp hogar_vulnerable if p207==2 & ocu500==1, col

svy: tab exp_ocup hogar_vulnerable if ocu500==1, col
svy: tab exp_ocup hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab exp_ocup hogar_vulnerable if p207==2 & ocu500==1, col

svy: tab  hor_trab hogar_vulnerable if ocu500==1, col
svy: tab  hor_trab hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab  hor_trab hogar_vulnerable if p207==2 & ocu500==1, col

tabstat i520 if ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
tabstat i520 if p207==1 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
tabstat i520 if p207==2 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)

*tabstat i520 if (p203==1 | p203==2) & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
*tabstat i520 if jhcony_sexo==1 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
*tabstat i520 if jhcony_sexo==2 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)

tabstat ing_act_mon ing_act_esp ing_act_tot if ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
tabstat ing_act_mon ing_act_esp ing_act_tot if p207==1 & ocu500==1 [w=fac500a0],stats(mean n) by(hogar_vulnerable)
tabstat ing_act_mon ing_act_esp ing_act_tot if p207==2 & ocu500==1 [w=fac500a0],stats(mean n) by(hogar_vulnerable)

*tabstat ing_act_mon ing_act_esp ing_act_tot  if (p203==1 | p203==2) & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
*tabstat ing_act_mon ing_act_esp ing_act_tot  if jhcony_sexo==1 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)
*tabstat ing_act_mon ing_act_esp ing_act_tot  if jhcony_sexo==2 & ocu500==1 [w=fac500a0],stats(mean) by(hogar_vulnerable)

svy: tab tam_emp hogar_vulnerable if ocu500==1, col
svy: tab tam_emp hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab tam_emp hogar_vulnerable if p207==2 & ocu500==1, col

svy: tab pension hogar_vulnerable if ocu500==1, col
svy: tab pension hogar_vulnerable if p207==1 & ocu500==1, col
svy: tab pension hogar_vulnerable if p207==2 & ocu500==1, col

save "$temp\enaho01a-2010-500_midis.dta", replace

*5) GASTOS E INGRESOS EN EL HOGAR
*********************************

use "$temp\sumaria2010_vulnerable.dta", clear

*GASTOS

egen g1_alim=rsum(gru11hd gru12hd1 gru12hd2 gru13hd1 gru13hd2 gru13hd3 sg23 sig24 g05hd ig06hd g07hd ig08hd)
egen g2_vest=rsum(gru21hd gru22hd1 gru22hd2 gru23hd1 gru23hd2 gru23hd3 gru24hd)
egen g3_vivi=rsum(gru31hd gru32hd1 gru32hd2 gru33hd1 gru33hd2 gru33hd3 gru34hd)
egen g4_mant=rsum(gru41hd gru42hd1 gru42hd2 gru43hd1 gru43hd2 gru43hd3 gru44hd)
egen g5_salu=rsum(gru51hd gru52hd1 gru52hd2 gru53hd1 gru53hd2 gru53hd3 gru54hd)
egen g6_tran=rsum(gru61hd gru62hd1 gru62hd2 gru63hd1 gru63hd2 gru63hd3 gru64hd)
egen g7_educ=rsum(gru71hd gru72hd1 gru72hd2 gru73hd1 gru73hd2 gru73hd3 gru74hd)
egen g8_otro=rsum(gru81hd gru82hd1 gru82hd2 gru83hd1 gru83hd2 gru83hd3 gru84hd)
egen g_total=rsum(g1_alim g2_vest g3_vivi g4_mant g5_salu g6_tran g7_educ g8_otro)

gen gasto_alim=g1_alim/12
gen gasto_vest=g2_vest/12
gen gasto_vivi=g3_vivi/12
gen gasto_mant=g4_mant/12
gen gasto_salu=g5_salu/12
gen gasto_tran=g6_tran/12
gen gasto_educ=g7_educ/12
gen gasto_otro=g8_otro/12
gen gasto_tot=g_total/12

gen  gtot_monet=gashog1d
egen gtot_autoc=rsum(gru12hd1 gru22hd1 gru32hd1 gru42hd1 gru52hd1 gru62hd1 gru72hd1 gru82hd1)
egen gtot_espec=rsum(gru12hd2 gru22hd2 gru32hd2 gru42hd2 gru52hd2 gru62hd2 gru72hd2 gru82hd2)
egen gtot_dopub=rsum(gru13hd1 gru23hd1 gru33hd1 gru43hd1 gru53hd1 gru63hd1 gru73hd1 gru83hd1)
egen gtot_dopri=rsum(gru13hd2 gru23hd2 gru33hd2 gru43hd2 gru53hd2 gru63hd2 gru73hd2 gru83hd2)
egen gtot_otros=rsum(gru13hd3 gru23hd3 gru33hd3 gru43hd3 gru53hd3 gru63hd3 gru73hd3 gru83hd3)

gen gasto_monet=gtot_monet/12
gen gasto_autoc=gtot_autoc/12
gen gasto_espec=gtot_espec/12
gen gasto_dopub=gtot_dopub/12
gen gasto_dopri=gtot_dopri/12
gen gasto_otros=gtot_otros/12
gen gasto_total=gashog2d/12

gen gasto_alimpc=gasto_alim/mieperho
gen gasto_otrospc=(gasto_total-gasto_alim)/mieperho
gen gasto_monetpc=gasto_monet/mieperho
gen gasto_totalpc=gasto_total/mieperho

*INGRESOS

egen inglab_monbru=rsum(ingbruhd ingindhd insedthd ingseihd ingexthd)
egen inglab_monnet=rsum(ingnethd ingindhd insedlhd ingseihd ingexthd)
egen inglab_especi=rsum(pagesphd paesechd)
egen inglab_autoco=rsum(ingauthd isecauhd)
egen inglab_totalbru=rsum(inglab_monbru inglab_especi inglab_autoco)
egen inglab_totalnet=rsum(inglab_monnet inglab_especi inglab_autoco)

gen ing_trnsfpriv=ingtprhd
gen ing_trnsfpubl=ingtpuhd
egen ing_otros=rsum(ingrenhd ingoexhd)

gen inglab_monbru_mes=inglab_monbru/12
gen inglab_monnet_mes=inglab_monnet/12
gen inglab_especi_mes=inglab_especi/12
gen inglab_autoco_mes=inglab_autoco/12
gen inglab_totalbru_mes=inglab_totalbru/12
gen inglab_totalnet_mes=inglab_totalnet/12

gen ingtotal_monbru_mes=ingmo1hd/12
gen ingtotal_monnet_mes=ingmo2hd/12
gen ingtotal_totbru_mes=inghog1d/12
gen ingtotal_totnet_mes=inghog2d/12
gen ing_trnsfpriv_mes=ing_trnsfpriv/12
gen ing_trnsfpubl_mes=ing_trnsfpubl/12
gen ing_otros_mes=ing_otros/12

gen inglab_totalbru_mespp=inglab_totalbru_mes/percepho
gen inglab_totalnet_mespp=inglab_totalnet_mes/percepho

gen ingtotal_monbru_mespc=ingtotal_monbru_mes/mieperho
gen ingtotal_monnet_mespc=ingtotal_monnet_mes/mieperho
gen ingtotal_totbru_mespc=ingtotal_totbru_mes/mieperho
gen ingtotal_totnet_mespc=ingtotal_totnet_mes/mieperho
gen inglab_totalnet_mespc=inglab_totalnet_mes/mieperho
gen ing_trnsfpriv_mespc=ing_trnsfpriv_mes/mieperho
gen ing_trnsfpubl_mespc=ing_trnsfpubl_mes/mieperho
gen ing_otros_mespc=ing_otros_mes/mieperho

gen	juntos=1 if	ingtpu01>0	&	ingtpu01!=.
replace juntos=0 if ingtpu01==0

sort conglome vivienda hogar
save "$temp\sumaria2010_vulnerable_midis.dta", replace

tabstat totmieho mieperho percepho [w=factor07],stats(mean) by(hogar_vulnerable)

tabstat  gru11hd gru12hd1 gru12hd2 gru13hd1 gru13hd2 gru13hd3 [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru21hd gru22hd1 gru22hd2 gru23hd1 gru23hd2 gru23hd3 gru24hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru31hd gru32hd1 gru32hd2 gru33hd1 gru33hd2 gru33hd3 gru34hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru41hd gru42hd1 gru42hd2 gru43hd1 gru43hd2 gru43hd3 gru44hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru51hd gru52hd1 gru52hd2 gru53hd1 gru53hd2 gru53hd3 gru54hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru61hd gru62hd1 gru62hd2 gru63hd1 gru63hd2 gru63hd3 gru64hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru71hd gru72hd1 gru72hd2 gru73hd1 gru73hd2 gru73hd3 gru74hd [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gru81hd gru82hd1 gru82hd2 gru83hd1 gru83hd2 gru83hd3 gru84hd [w=factor07],stats(mean) by(hogar_vulnerable)

tabstat  gasto_alim gasto_vest gasto_vivi gasto_mant gasto_salu gasto_tran gasto_educ gasto_otro gasto_total [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gasto_monet gasto_autoc gasto_espec gasto_dopub gasto_dopri gasto_otros gasto_total [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  gasto_alimpc gasto_otrospc gasto_monetpc gasto_totalpc [w=factor07],stats(mean) by(hogar_vulnerable)

tabstat  ingtotal_totnet_mes inglab_totalnet_mes ing_trnsfpriv_mes ing_trnsfpubl_mes  ing_otros_mes [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  inglab_monnet_mes inglab_especi_mes inglab_autoco_mes inglab_totalnet_mes [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  ingtotal_totnet_mespc inglab_totalnet_mespc ing_trnsfpriv_mespc ing_trnsfpubl_mespc ing_otros_mespc [w=factor07],stats(mean) by(hogar_vulnerable)
tabstat  inglab_totalnet_mespp [w=factor07],stats(mean) by(hogar_vulnerable)


svy: tab pobreza hogar_vulnerable,col


*6) PERCEPCION DE JH
*********************

use "$enaho\enaho01b-2010-1.dta", clear
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable_midis.dta", keep(hogar_vulnerable ing* gas* pobreza)
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$enaho\enaho01b-2010-2.dta"

*hay 761 missing en variable p38 (ingreso minimo para vivir)

gen ing_dif= ingtotal_totnet_mes-p38


svyset conglome [pw=facgob07],strata(estrato)

forvalues  x=1/7 {
svy: tab  p11_`x' hogar_vulnerable, col
}

svy: tab p12 hogar_vulnerable, col

forvalues  x=1/5 {
svy: tab  p5_`x' hogar_vulnerable, col
}

svy: tab p15 hogar_vulnerable, col

svy: tab p21a hogar_vulnerable, col
svy: tab p21b hogar_vulnerable, col
svy: tab p21c hogar_vulnerable, col

svyset conglome [pw=factor07],strata(estrato)

svy: tab p32 hogar_vulnerable, col
svy: tab p33_2 hogar_vulnerable, col
svy: tab p37 hogar_vulnerable, col

tabstat  p38 [w=factor07],stats(mean) by(hogar_vulnerable)

svy: tab p38a hogar_vulnerable, col
svy: tab p39a hogar_vulnerable, col

svy: tab p39a pobreza if hogar_vulnerable==0
svy: tab p39a pobreza if hogar_vulnerable==1

sort conglome vivienda hogar
save "$temp\enaho01b-2010-1_midis.dta", replace


*7) PROGRAMAS SOCIALES
***********************

use "$temp\sumaria2010_vulnerable_midis.dta", clear
keep hogar_vulnerable mieperho factor07 dominio estrato conglome vivienda hogar juntos
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-100_midis.dta", keep(p701)
drop _m
sort conglome vivienda hogar
save "$temp\sumaria2010_vulnerable_midis_ppss.dta", replace

svyset conglome [pw=factor07],strata(estrato)

svy: tab p701 hogar_vulnerable, col
svy: tab juntos hogar_vulnerable, col

use "$enaho\enaho01-2010-200.dta", clear
destring codperso, replace
sort conglome vivienda hogar codperso
save "$temp\enaho01-2010-200_v2.dta", replace


use "$enaho\enaho01-2010-700.dta", clear
gen uno=1
collapse (sum) uno, by(conglome vivienda hogar p702)
rename p702 codperso
rename uno num_ppss
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$temp\enaho01-2010-200_v2.dta", keep(p203 p204)
drop if _merge==2
sort conglome vivienda hogar codperso
save "$temp\enaho01-2010-700_v1.dta", replace

use "$enaho\enaho01-2010-700.dta", clear
rename p702 codperso
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$temp\enaho01-2010-700_v1.dta", keep(p203 p204)
keep if p204==1
keep if p203!=9
keep if p203!=10
sort conglome vivienda hogar codperso
save "$temp\enaho01-2010-700_v2.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==1
gen pers_vdl=1
collapse (sum) pers_vdl, by(conglome vivienda hogar)
gen hogar_vdl=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_vdl.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==2
gen pers_compop=1
collapse (sum) pers_compop, by(conglome vivienda hogar)
gen hogar_compop=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_compop.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==3
gen pers_desesc=1
collapse (sum) pers_desesc, by(conglome vivienda hogar)
gen hogar_desesc=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_desesc.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==4
gen pers_almesc=1
collapse (sum) pers_almesc, by(conglome vivienda hogar)
gen hogar_almesc=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_almesc.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==5
gen pers_papilla=1
collapse (sum) pers_papilla, by(conglome vivienda hogar)
gen hogar_papilla=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_papilla.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==6
gen pers_canasta=1
collapse (sum) pers_canasta, by(conglome vivienda hogar)
gen hogar_canasta=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_canasta.dta", replace

use "$temp\enaho01-2010-700_v2.dta", clear
keep if p703==7
gen pers_otro=1
collapse (sum) pers_otro, by(conglome vivienda hogar)
gen hogar_otro=1
sort conglome vivienda hogar
save "$temp\enaho01-2010-700_otro.dta", replace

use "$temp\enaho01-2010-700_vdl.dta", clear
merge conglome vivienda hogar using "$temp\enaho01-2010-700_compop.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-700_desesc.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-700_almesc.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-700_papilla.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-700_canasta.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\enaho01-2010-700_otro.dta"
tab _m
drop _m
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable_midis_ppss.dta", keep(p701 hogar_vulnerable factor07 estrato juntos)

recode hogar_vdl (.=0)
recode hogar_compop (.=0)
recode hogar_desesc (.=0)
recode hogar_almesc (.=0)
recode hogar_papilla (.=0)
recode hogar_canasta (.=0)
recode hogar_otro (.=0)

egen hog_num_ppss=rsum(hogar_vdl hogar_compop hogar_desesc hogar_almesc hogar_papilla hogar_canasta hogar_otro)
egen hog_num_ppssjun=rsum(hogar_vdl hogar_compop hogar_desesc hogar_almesc hogar_papilla hogar_canasta hogar_otro juntos)

save "$temp\enaho01-2010-700_midis.dta", replace

svyset conglome [pw=factor07],strata(estrato)

foreach  x in vdl compop desesc almesc papilla canasta otro {
svy: tab  hogar_`x' hogar_vulnerable, col
}

svy: tab hog_num_ppss hogar_vulnerable, count format(%12.0g)
svy: tab hog_num_ppssjun hogar_vulnerable, count format(%12.0g)


svy: tab hog_num_ppss hogar_vulnerable, col
svy: tab hog_num_ppssjun hogar_vulnerable, col

*8) SALUD
**********

use "$enaho\enaho01a-2010-400.dta", clear
keep if p204==1
keep if p203!=9
keep if p203!=10
sort conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(hogar_vulnerable)

*A) SEGURO DE SALUD
*******************

gen miembro14=1 if p208a<14
replace miembro14=0 if p208a>=14

gen miembro05=1 if p208a<5
replace miembro05=0 if p208a>=5

gen jhcony_sexo=1 if p203==1 & p207==1
replace jhcony_sexo=1 if p203==2 & p207==1
replace jhcony_sexo=2 if p203==1 & p207==2
replace jhcony_sexo=2 if p203==2 & p207==2
label define jhcony_sexo 1"Jefe_conyuge_hombre" 2"Jefa_conyuge_mujer"
label values jhcony_sexo jhcony_sexo

gen seguro_salud=1 if p4199==0
replace seguro_salud=0 if p4199==1
label define seguro 1"Tiene_seguro" 0"No_tiene_seguro"
label values seguro_salud seguro

svyset conglome [pw=factor07],strata(estrato)

svy: tab seguro hogar_vulnerable, col
svy: tab seguro hogar_vulnerable if p203==1, col
svy: tab seguro hogar_vulnerable if p203==1, col
svy: tab seguro hogar_vulnerable if jhcony_sexo==1, col
svy: tab seguro hogar_vulnerable if jhcony_sexo==2, col
svy: tab seguro hogar_vulnerable if p207==1, col
svy: tab seguro hogar_vulnerable if p207==2, col

svy: tab seguro hogar_vulnerable if miembro14==1, col
svy: tab seguro hogar_vulnerable if p207==1 & miembro14==1, col
svy: tab seguro hogar_vulnerable if p207==2 & miembro14==1, col

svy: tab seguro hogar_vulnerable if miembro05==1, col
svy: tab seguro hogar_vulnerable if p207==1 & miembro05==1, col
svy: tab seguro hogar_vulnerable if p207==2 & miembro05==1, col

forvalues  x=1/8 {
svy: tab  p419`x' hogar_vulnerable, col
}

save "$enaho\enaho01a-2010-400_midis.dta", replace


**INDICADORES ENDES

use "$endes\rech0.dta", clear
sort hhid
save "$temp\rech0.dta", replace

use "$endes\rech23.dta", clear
sort hhid 
save "$temp\rech23", replace

use "$endes\rech0.dta", clear
merge hhid using "$temp\rech23"
tab _m
keep hhid hv270 hv025
sort hhid
save "$temp\rech0_23.dta", replace

use "$endes\rec0111.dta", clear
gen educamm=1 if v133<=5
replace educamm=2 if v133>=6 & v133<=11
replace educamm=3 if v133>=12
replace educamm=. if v133==.
gen nativamm=1 if v131==2  |  v131==3 |  v131==4
replace nativamm=0 if v131==1  | v131==5
keep caseid educamm nativamm
rename caseid caseid_madre
sort caseid_madre
save "$temp\lengua_madre.dta", replace

use "$endes\rech1.dta", clear

*genero el caseid de la mama para pegarle ahi las variables de etnicidad de la madre
generate caseid_madre=hhid
generate id2=string(hv112)
replace caseid_madre = caseid_madre + "  " +id2 if hv112<10
replace caseid_madre = caseid_madre + " "  +id2 if  hv112>9
sort hhid
merge hhid using "$temp\rech0_23"
tab _m
rename _m _mrech1_0_23

gen quintil = hv270
gen area=hv025
recode area 2=0

sort caseid_madre
merge caseid_madre using "$temp\lengua_madre.dta"
tab _merge
drop if _merge==2

*Definición de exclusiones
gen exclusion1=1 if area==0
replace exclusion1=0 if area==1
label var exclusion1 "Ambito Rural"

gen exclusion2=1 if educamm==1
replace exclusion2=0 if educamm!=1
label var exclusion2 "Educación primaria incompleta o menos de la madre"

gen exclusion3=1 if quintil==1
replace exclusion3=0 if quintil!=1
label var exclusion3 "Quintil I de riqueza"

gen exclusion4=1 if nativamm==1
replace exclusion4=0 if nativamm==0
label var exclusion4 "Lengua materna originaria de la madre"

*Definición de hogares vulnerables (3 o + exclusiones)

egen suma_exclusiones=rsum(exclusion1  exclusion2  exclusion3  exclusion4)
gen hogar_vulnerable=1 if suma_exclusiones>=3
replace hogar_vulnerable=0 if suma_exclusiones<3
label var hogar_vulnerable "Hogar presenta 3 exclusiones o más"

sort caseid

collapse (mean) hogar_vulnerable, by(hhid hc0)
tab hogar_vulnerable


save "$temp\basendes_poblacion_midis.dta", replace



*B) CRED COMPLETO EN MENORES DE 36 MESES
*******************************

use "$endes\rec0111.dta", clear
sort caseid
save "$temp\rec0111.dta", replace

use "$endes\rec21.dta", clear
rename  bidx hidx
sort caseid hidx
save "$temp\rec21", replace

use "$endes\rec91.dta", clear
sort caseid 
save "$temp\rec91", replace

use "$endes\rec95.dta", clear
rename idx95 hidx
sort caseid hidx
save "$temp\rec95", replace

use "$endes\rec43.dta", clear
sort caseid hidx

merge caseid using "$temp\rec0111.dta"
tab _m
drop _m
sort caseid hidx
merge caseid hidx using "$temp\rec21"
tab _m
drop _m
sort caseid
merge caseid using "$temp\rec91"
tab _m
drop _m
sort caseid hidx
merge caseid hidx using "$temp\rec95"
tab _m
save "$temp\cred.dta", replace
drop _m

rename caseid caseid_orig

*genero identificador de hogar a nivel individual
generate caseid=hhid
generate id=string(hc0)
replace caseid = caseid + "  " +id if hc0<10
replace caseid = caseid + " " +id if  hc0>9


sort hhid hc0
merge hhid hc0 using "$temp\base_desnutricion_midis.dta"
tab _m
drop _m



gen peso=v005/1000000
recode sregion (4/5=4)
label define region 1"Lima_Metropolitana" 2"Resto_Costa" 3"Sierra" 4"Selva"
label values sregion region

recode v024 (7=15)

gen edad=v008-b3
gen cred=0
replace cred=1 if edad<2 & s466c>1
replace cred=1 if edad==2 & s466c>2
replace cred=1 if (edad==3 | edad==4) & s466c>3
replace cred=1 if (edad==5 | edad==6) & s466c>4
replace cred=1 if (edad==7) & s466c>5
replace cred=1 if (edad==8 | edad==9) & s466c>6
replace cred=1 if (edad>=10 & edad<=12) & s466c>7
replace cred=1 if (edad>=13 & edad<=15) & s466c>8
replace cred=1 if (edad>=16 & edad<=18) & s466c>9
replace cred=1 if (edad>=19 & edad<=21) & s466c>10
replace cred=1 if (edad>=22 & edad<=24) & s466c>11
replace cred=1 if (edad>=25 & edad<=30) & s466c>12
replace cred=1 if (edad>=31 & edad<=36) & s466c>13
replace cred=1 if (edad>=37 & edad<=42) & s466c>14
replace cred=1 if (edad>=43 & edad<=48) & s466c>15
replace cred=1 if (edad>=49 & edad<=54) & s466c>16
replace cred=1 if (edad>=55 & edad<=59) & s466c>17
replace cred=9 if s466c>20
replace cred=9 if s466c==98
replace cred=9 if s466c==.
replace cred=0 if s466==0
replace cred=0 if s466==8

svyset v001 [pw=peso],strata(v022)

