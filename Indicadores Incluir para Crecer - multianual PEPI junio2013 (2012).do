********************************
** AÑOS 2012 - 2012 y 2012	 ***
********************************
clear
clear matrix
set mem 500m


global orig "E:\MIDIS\Datos\ENAHO\2012\Originales"
global trab "E:\MIDIS\Datos\ENAHO\2012\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\2012\Resultados"

global endes_orig "E:\MIDIS\Datos\ENDES\2012\Originales"
global endes_trab "E:\MIDIS\Datos\ENDES\2012\Trabajadas"
global endes_result "E:\MIDIS\Datos\ENDES\2012\Resultados"

*************************
** PREPARANDO LAS BASES **
**************************


****************************
** USANDO ENAHO  HOGARES ***
****************************

** 1) POBLACION EN PROCESO DE INCLUSION (POBLACION "MIDIS")

use "$orig\enaho01a_2012_300.dta", clear

*EXCLUSION 1 - Educación
**JH mujer tiene primaria incompleta o menos
**Conyuge mujer de JH hombre tiene primaria incompleta o menos
**JH hombre sin conyuge tiene primaria incompleta o menos

gen educ=1 if (p301a==1 | p301a==2 | p301a==3) /*primaria incompleta o menos*/
replace educ=0 if p301a>3 & p301a<=11 /*primaria completa a mas*/

tab p203, gen(rel_par)
bys conglome vivienda hogar: egen suma_cony=sum(rel_par2)
gen educ_jhm=1 if educ==1 & p203==1 & p207==2
replace educ_jhm=1 if educ==1 & p203==2 & p207==2
replace educ_jhm=1 if educ==1 & p203==1 & p207==1 &  suma_cony==0
replace educ_jhm=0 if educ==0 & p203==1

*EXCLUSION 2 - Lengua Materna
**JH o conyuge tienen lengua materna originaria

gen lengua_matorigjh=0 if p300a>3 & p300a<8 & (p203==1 | p203==2)
replace lengua_matorigjh=1 if p300a<=3 & (p203==1 | p203==2)

collapse (sum) lengua_matorig* educ*, by(conglome vivienda hogar)
sort conglome vivienda hogar
save "$trab\educ300hogar.dta", replace

*EXCLUSION 3 - I Quintil 
**Primer quintil del gasto total percapita del hogar (deflactado espacialmente)

use "$orig\enaho01_2012_100.dta", clear
sort conglome vivienda hogar
merge conglome vivienda hogar using "$orig\sumaria_2012.dta"
keep if _merge==3
drop _merge


sort conglome vivienda hogar
merge 1:1 conglome vivienda hogar using "$trab\educ300hogar.dta"
keep if _merge==3
drop _merge


gen gas_mes=gashog2d/12
gen gas_mespc=gas_mes/mieperho

*Deflactar por linea de pobreza
gen deflactor=linea

gen gas_mespcdef=gas_mespc/deflactor

gen factor07pob=factor07*mieperho
sort gas_mespcdef
xtile gas_qui [pw=factor07pob]=gas_mespcdef, nq(5)


*EXCLUSION 4 - AMBITO RURAL
gen area=1 if estrato>=1 & estrato<=5
replace area=2 if estrato>=6 & estrato<=8
label define area 1"Urbano" 2"Rural"
label values area area


*Definición de exclusiones
gen exclusion1=1 if area==2
replace exclusion1=0 if area==1
label var exclusion1 "Ambito Rural"

gen exclusion2=1 if educ_jhm==1
replace exclusion2=0 if educ_jhm==0
label var exclusion2 "Educación primaria incompleta o menos de mujer (JH o conyuge)"

gen exclusion3=1 if gas_qui==1
replace exclusion3=0 if gas_qui!=1
label var exclusion3 "Quintil I - gasto mensual pc deflactado"

gen exclusion4=1 if lengua_matorigjh==1 | lengua_matorigjh==2
replace exclusion4=0 if lengua_matorigjh==0
label var exclusion4 "Lengua materna originaria del JH o conyuge"

*Definición de hogares vulnerables (3 o + exclusiones)

egen suma_exclusiones=rsum(exclusion1  exclusion2  exclusion3  exclusion4)
gen hogar_vulnerable=1 if suma_exclusiones>=3
replace hogar_vulnerable=0 if suma_exclusiones<3
label var hogar_vulnerable "Hogar presenta 3 exclusiones o más"

*Caracterización de hogares vulnerables

*DEPARTAMENTO
gen dpto_cod=substr(ubigeo,1,2)
destring dpto_cod, replace
label define dpto 1"Amazonas" 2"Ancash" 3"Apurimac" 4"Arequipa" 5"Ayacucho" 6"Cajamarca" 7"Callao" 8"Cusco" 9"Huancavelica" 10"Huánuco" 11"Ica" 12"Junín" 13"La_Libertad" 14"Lambayeque" 15"Lima" 16"Loreto" 17"Madre_de_Dios" 18"Moquegua" 19"Pasco" 20"Piura" 21"Puno" 22"San_Martin" 23"Tacna" 24"Tumbes" 25"Ucayali"
label values dpto_cod dpto

*REGION NATURAL
recode dominio (1/3=1) (4/6=2) (7=3) (8=4), gen(reg_nat)
label define reg_nat 1"Costa" 2"Sierra" 3"Selva" 4"Lima_Met"
label values reg_nat reg_nat

*Población-personas
svyset conglome [pw=factor07pob],strata(estrato)

svy: tab area hogar_vulnerable, count format(%12.0g)
svy: tab reg_nat hogar_vulnerable, count format(%12.0g)
svy: tab dpto_cod hogar_vulnerable, count format(%12.0g)


*Población-hogares
svyset conglome [pw=factor07],strata(estrato)

svy: tab area hogar_vulnerable, count format(%12.0g)
svy: tab reg_nat hogar_vulnerable, count format(%12.0g)
svy: tab dpto_cod hogar_vulnerable, count format(%12.0g)


* Variables de pobreza e infraestructura en el hogar:

	* Pobreza monetaria:
gen hogar_pobre=pobreza<=2

	* Pobreza monetaria extrema:
gen hogar_pobre_ext=pobreza==1


	* Agua segura:
gen aguamej=1 if p110==1 | p110==2 |p110==3
replace aguamej=0 if p110!=. & aguamej==.

	* Saneamiento:
gen desgmej=1 if p111==1 | p111==2 |p111==4
replace desgmej=0 if p111!=. & desgmej==.

	* Electricidad:
gen luz=1 if p1121==1
replace luz=0 if p1121!=. & luz==.

	* Telefonia:
gen telf=1 if p1141==1 | p1142==1
replace telf=0 if p1141!=. & p1142!=. & telf==.

	* Paquete de servicios
gen paquetess=1 if aguamej==1 & desgmej==1 & luz==1 & telf==1
replace paquetess=0 if paquetess==. & aguamej!=. & desgmej!=. & luz!=. & telf!=.
label var paquetess "el hogar accede al paquete completo (agua, desague, luz y telf)"

sort conglome vivienda hogar
save "$trab\enaho_pepi2012.dta", replace /* Esta base se encuentra a nivel de hogar */





* Identificando hogares con adultos mayores:
use "$orig\enaho01_2012_200.dta", clear

gen edad65=0
replace edad65=1 if p208a>=65 & p208a<=98
sort conglome vivienda hogar

collapse  (max) edad65, by (conglome vivienda hogar)
rename edad65 hogar_con_am
sort conglome vivienda hogar
save "$trab\hogar_con_am.dta", replace 


* Pegamos la variable de existencia de adulto mayor en el hogar:
use "$trab\enaho_pepi2012.dta", clear
sort conglome vivienda hogar
merge  conglome vivienda hogar using "$trab\hogar_con_am.dta"
drop _merge


log using "$result\Brechas de pobreza y usando ingresos autónomos a nivel hogar 2012.log", replace


** BRECHA DE POBREZA - nivel de hogar
*(distancia del gasto per capita del hogar respecto a la linea de pobreza)

povdeco gas_mespc [w=factor07], varpl(linea) 
povdeco gas_mespc [w=factor07] if hogar_con_am==1, varpl(linea) 

povdeco gas_mespc [w=factor07] if hogar_vulnerable==1, varpl(linea)
povdeco gas_mespc [w=factor07] if hogar_con_am==1 & hogar_vulnerable==1, varpl(linea)
*usar el FGT1-->a==1


*** TASA DE POBREZA EXTREMA USANDO SOLO GASTOS AUTONOMOS
*(distancia del gasto autonomo per capita del hogar respecto a la linea de pobreza)

* Gasto autónomo=gasto total del hogar neto de transferencias publicas monetarias o en especie. 
* Se quita: 1) transferencias JUNTOS (ingtpu01) y 2) donaciones en especie (alimentos, ... todos los rubros de gasto)

gen gashog2dautonomo=gashog2d-ingtpu01- gru13hd1- gru23hd1- gru33hd1- gru43hd1- gru53hd1- gru63hd1- gru73hd1- gru83hd1
replace gashog2dautonomo=0 if gashog2dautonomo<0
summ ingtpuhd   ingtpu01  ingtpu02

gen gastopcautonomo=gashog2dautonomo/(mieperho*12)
label var gastopcautonomo "gasto pc mensual sin incluir juntos o donaciones publ en especie"


povdeco gastopcautonomo [w=factor07pob], varpl(linpe) 
povdeco gastopcautonomo [w=factor07pob] if hogar_vulnerable==1, varpl(linpe)
*usar el FGT1-->a==0

log close


* Pegamos la base de pobreza subjetiva:
sort conglome vivienda hogar
merge conglome vivienda hogar using "$orig\enaho01b_2012_2.dta"
gen pobre_subjetivo=0 if p39b1!=.
replace pobre_subjetivo=1 if (p39b1==0 | p39b1==1 | p39b1==2) & pobre_subjetivo==0

gen am_pobre_subj=pobre_subjetivo if hogar_con_am==1
drop _merge
sort conglome vivienda hogar
save "$trab\hogar_pepi2012_lista.dta", replace


****************
****************
****************


*****************************
** BASE ENAHO INDIVIDUALES **
*****************************

use "$orig\enaho01_2012_200.dta", clear
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_2012_300.dta"
rename _merge merge200_300
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_2012_400.dta"
rename _merge merge200_300_400
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_2012_500.dta"
rename _merge merge200_300_400_500
sort conglome vivienda hogar codperso


* Creamos los pesos:
gen peso_individual=factor07
gen peso_empleo=fac500

* Unimos con las variables del hogar
sort conglome vivienda hogar 
merge conglome vivienda hogar using "$trab\enaho_pepi2012.dta", keep (hogar_vulnerable hogar_pobre_ext)
rename _merge merge_indiv_hogar

* Variables educativas:

	* Asistencia preescolar:
gen preescolar=0 if p208a>=3 & p208a<=5 & mes>="03" & mes<="12"
replace preescolar=1 if p306==1 & p307==1  & preescolar==0
label var preescolar "asistencia a inicial o primaria de ninhos de 3 a 5a"
tab preescolar

	* Jovenes de 18 a 25 años que culminaron educacion secundaria:
gen joven_culmino_secun=0 if p208a>=18 & p208a<=25
replace joven_culmino_secun=1 if p301a>=6 & p301a<=11 & joven_culmino_secun==0
tab joven_culmino_secun


	* Niño de 6 a 11 años que asiste a educacion primaria:
gen asiste_primaria=0 if p208a>=6 & p208a<=11 & mes>="03" & mes<="12" 
replace asiste_primaria=1 if p306==1 & p307==1 & p308a==2 & asiste_primaria==0


	* Niño de 12 a 17 años que asiste a educacion secundaria:
gen asiste_secundaria=0 if p208a>=12 & p208a<=17 & mes>="03" & mes<="12"
replace asiste_secundaria=1 if p306==1 & p307==1 & p308a==3 & asiste_secundaria==0

* Variables de empleo:

	* Proporcion de niños de 6 a 13 años que trabajan en epoca escolar:
gen nino_trabaja=0 if p208a>=6 & p208a<=13 & (mes>="03" & mes<="12")
replace nino_trabaja=1 if (p211a>=1 & p211a<=7) & nino_trabaja==0
tab nino_trabaja

	
	* Proporcion de adolescentes entre 14 y 17 años que trabajan en epoca escolar:
gen adolescente_trabaja=0 if p208a>=14 & p208a<=17 & (mes>="03" & mes<="12")
replace adolescente_trabaja=1 if (p501==1 | p502==1 | p503==1) & adolescente_trabaja==0


	* Indicador de Desempleo
gen desempleo=0 if ocu500>=1 & ocu500<=3
replace desempleo=1 if (ocu500==2 | ocu500==3) & desempleo==0
tab desempleo

	* Porcentaje de la PEA rural dedicada a actividades no agrícolas:
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

gen PEArural_noagro=0 if (estrato>=6 & estrato<=8) & (ocu500>=1 & ocu500<=3)
replace PEArural_noagro=1 if (cod_ocup2<=60 | cod_ocup2>=65) & cod_ocup2!=. & cod_ocup2!=97 & PEArural_noagro==0



* Indicadores para adultos mayores:

gen edad65=(p208a>=65 & p208a<=98) if p208a!=.

	* Adultos mayores pobres extremos
gen am_pobre_ext=0 if edad65==1
replace am_pobre_ext=1 if  hogar_pobre_ext==1 & am_pobre_ext==0


******
******
******


	* afiliado a pension:
gen afiliado_pension=p558a5==0 if p558a5!=. & edad65==1

	* recibe pension:
gen recibe_pension=p5564a==1 if p5564a!=. & edad65==1


	* padece enfermedad:
gen padece_enfermedad=0 if p401!=.
replace padece_enfermedad=1 if p401==1 & padece_enfermedad==0
replace padece_enfermedad=1 if p4025==0 & padece_enfermedad==0

gen busco_atencion=0 if padece_enfermedad==1
replace busco_atencion=1 if (p4031==1 | p4032==1  | p4033==1   | p4034==1   | p4035==1   | p4036==1  | p4037==1   | p4038==1   | p4039==1) & busco_atencion==0

	* padece enfermedad y no recibe tratamiento médico:
gen am_sin_atencion=0 if edad65==1
replace am_sin_atencion=1 if padece_enfermedad==1 & busco_atencion==0 & am_sin_atencion==0


	* tiene algun seguro de salud:
gen am_seguro_salud=(p4191==1 | p4192==1 | p4193==1 | p4194==1 | p4195==1 | p4196==1 | p4197==1 | p4198==1) if p4191!=. & edad65==1
tab am_seguro_salud


sort conglome vivienda hogar codperso
save "$trab\indiv_pepi2012_lista.dta", replace




*********************
** USANDO  ENDES  ***
*********************

***INDICADOR ENDES - DESNUTRICIÓN CRÓNICA (OMS)


* rech0 es base de hogares e incluyen sus caracteristicas:
use "$endes_orig\rech0.dta", clear
sort hhid
save , replace

* rech1 es base de los miembros del hogar, incluyendo a la mujer entrevistada: incluye identificador de persona
use "$endes_orig\rech1.dta", clear
sort hhid hc0
save , replace

* rech23 es base de hogares tambien:
use "$endes_orig\rech23.dta", clear
sort hhid 
save , replace

* rech4 es base de los miembros del hogar, incluyendo a la mujer entrevistada: incluye identificador de persona
use "$endes_orig\rech4.dta", clear
sort hhid hc0
save , replace

* rech6 es base de niños de 0 a 5 años en el hogar: incluye identificador de persona
use "$endes_orig\rech6.dta", clear
sort hhid hc0
save , replace
 

* Pega base de niños con base de todos los miembros del hogar:

* aqui se pegan los niños con las caract de los miembros de su hogar, para tener la info de su madre:
use "$endes_orig\rech6.dta", clear
merge hhid hc0 using "$endes_orig\rech1"
tab _m
rename _m merge_rech6_rech1

* aqui se pegan a los miembros (incluidos los niños) con las caracteristicas de su vivienda:
sort hhid
merge hhid using "$endes_orig\rech23"
tab _m
rename _m merge_rech23
sort hhid
merge hhid using "$endes_orig\rech0.dta"
tab _m
rename _m merge_rech0


* aqui se pegan a los miembros (incluidos los niños) con los otros miembros:
sort hhid hc0
merge hhid hc0 using "$endes_orig\rech4"
tab _m
rename _m merge_rech4

	* guardamos la primera base que incluye a todos los miembros del hogar para analizar variables de desnutricion
save "$endes_trab\base_desnutricion_2012.dta", replace

*primero el peso (a nivel de hogar):
gen peso = hv005/1000000
svyset hv001 [weight=peso], strata(hv022)

gen cero=0
tostring cero, replace


*genero identificador de mujer:
generate caseid=hhid
generate id=string(hc0)
replace caseid = caseid + " " +cero+id if hc0<10
replace caseid = caseid + " " +id if  hc0>9


*GENERANDO DESNUTRICION CRONICA - OMS
gen 		desnutricion_cronica_oms = (hc70<-200) if hc70 != .
replace 	desnutricion_cronica_oms = 2 if (hc70>=-200 & hc70<601)
replace 	desnutricion_cronica_oms = . if hv103 == 0 
replace 	desnutricion_cronica_oms = . if hc70== 9998 | hc70 ==9999
recode	desnutricion_cronica_oms  2 = 0
*confirmando
svy linearized: mean desnutricion_cronica_oms
svy linearized : mean desnutricion_cronica_oms, over (hv025)
*cuadra  con documento inei


*genero el caseid de la mama para pegarle luego las variables de etnicidad y educacion de la madre
generate caseid_madre=hhid
generate id2=string(hv112)
replace caseid_madre = caseid_madre + "  "  +id2 
sort caseid_madre
save "$endes_trab\base_desnutricion_2012.dta", replace


*PEGANDO VARIABLE DE LENGUA DE LA MADRE
gen niños_menores5 = (hc1 >= 0 & hc1 <= 59)
keep if niños_menores5 == 1
*me quedo solo con niños cuya madre vive en el hogar
drop if hv112 == 0

sort caseid_madre
save "$endes_trab\niños_0a5.dta", replace
*OJO ESTA BASE SOLO ES DE NIÑOS QUE TIENEN DATO DE MADRE EN EL HOGAR (PARA JUNTAR CON LA BASE DE LENGUA)


* creamos las caracteristicas de lengua y educacion de la madre para pegarlas luego a la base de los niños (con caract. del hogar)
use "$endes_orig\rec0111.dta", clear
sort caseid

gen educamm=1 if v133<=5
replace educamm=2 if v133>=6 & v133<=11
replace educamm=3 if v133>=12
replace educamm=. if v133==.
gen nativamm=1 if v131==2  |  v131==3 |  v131==4
replace nativamm=0 if v131==1  | v131==5
gen caseid_madre=caseid
sort caseid_madre
save "$endes_trab\lengua_madre.dta", replace




* Ahora usaremos la que reporta todos los hijos nacidos de las madres. Sirve para calcular mortalidad infantil (muerte ocurrida durante el primer año de vida)
	* es una base de madres e hijos (cada hijo empatado con su madre)

use "$endes_orig\rec21.dta", clear
sort caseid bidx
*gen hidx=bidx  
sort caseid
	* pegamos la info de cuando se realizo la encuesta para determinar la edad del niño)
merge caseid using "$endes_orig\rec0111.dta", keep(v006 v005 v001 v022 v133 v131 v190 v025)
drop _merge
gen caseid_madre=caseid


	* mortalidad infantil:
	* identificamos a los niños que nacieron en el último año (meses desde el nacimiento hasta el mes de la entrevista)
gen meses_hasta_entrevista=(v006-b1) if b2==2012
replace meses_hasta_entrevista=(12-b1) + v006 if b2==2011 & meses_hasta_entrevista==.

gen nacido_ult_anio=1 if meses_hasta_entrevista>=0 & meses_hasta_entrevista<=11

	* niño que falleció (mortalidad infantil en el primer año)
gen mortalidad_infantil=0 if nacido_ult_anio==1
replace mortalidad_infantil=1 if b5==0 & mortalidad_infantil==0
tab mortalidad_infantil
sort caseid_madre

* añadimos las variables para crear pepi
*primero el peso:
gen peso = v005/1000000
svyset v001 [weight=peso], strata(v022)

gen quintil = v190
gen area=v025
recode area 2=0
sort caseid

gen educamm=1 if v133<=5
replace educamm=2 if v133>=6 & v133<=11
replace educamm=3 if v133>=12
replace educamm=. if v133==.
gen nativamm=1 if v131==2  |  v131==3 |  v131==4
replace nativamm=0 if v131==1  | v131==5


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

count
sort caseid_madre
save "$endes_trab\base_mortalidad.dta", replace



* base de niños y vacunas:
use "$endes_orig\rec41.dta", clear
*gen hidx=midx  
*gen caseid_madre=caseid
sort caseid_madre hidx
save, replace

* base de niños y enfermedades:
use "$endes_orig\rec43.dta", clear
sort caseid hidx
*gen caseid_madre=caseid
sort caseid_madre hidx
save, replace

* base de niños: edad y talla
use "$endes_orig\rec44.dta", clear
*gen hidx=hwidx 
*gen caseid_madre=caseid
sort caseid_madre hidx
save, replace


* uniendo las base de niños vivos nacidos en los ultimos 5 años:
use "$endes_orig\rec44.dta", clear
merge caseid_madre hidx using "$endes_orig\rec41.dta"
tab _merge
drop _merge
sort caseid_madre hidx
merge caseid_madre hidx using "$endes_orig\rec43.dta"
tab _merge
drop _merge
sort caseid_madre hidx
save "$endes_trab\salud_materno_infantil.dta", replace


/*
* abrimos la base de niños de 0 a 5 años que proviene de la base_desnutricion_2012.dta, pero que solo incluye a los niños que viven con su madre
	* le vamos a pegar las caracteristicas de su madre:

use "$endes_trab\niños_0a5.dta", clear
sort caseid_madre
merge caseid_madre using "$endes_trab\lengua_madre.dta"
tab _merge

drop if _merge == 2
drop _merge
keep niños_menores5 caseid_madre nativamm educamm  v001 v005 v022
sort caseid_madre 
sort caseid_madre 
save "$endes_trab\niños_0a5.dta", replace
*/

****
****
****

* A la base de todos los miembros del hogar 8incluyendo los niños) le pegamos las caracteristicas de la madre de los niños para crear la PEPI:

use "$endes_trab\base_desnutricion_2012.dta", clear
count
sort caseid_madre
codebook caseid_madre /* hay 45230 madres */
merge caseid_madre using "$endes_trab\lengua_madre.dta"
rename _merge _merge_madres
tab _merge_madres
drop if _merge==2
codebook caseid_madre


gen sex =hc27
recode sex 2=0
gen quintil = hv270
gen area=hv025
recode area 2=0
sort caseid_madre


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


sort caseid_madre
save "$endes_trab\base_desnutricion_2012_conpepi", replace



****
****
****


** Ahora usaremos la base de salud materno infantil y le pegamos la base de "lengua_madre" para identificar a la PEPI***
	* Pegamos la base de salud materno infantil con las caracteristicas de su madre/hogar: merge=3 identifica a los niños
	
use  "$endes_trab\salud_materno_infantil.dta", clear
count
sort caseid_madre
merge caseid_madre using "$endes_trab\lengua_madre"
drop if _merge==2
drop _merge

* creamos la pepi:
gen quintil = v190
gen area=v025
recode area 2=0
sort caseid_madre


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


sort caseid_madre


	* Creamos el resto de variables de interés (salud del niño)


* Bajo peso al nacer:
replace m19=. if m19>=9000
replace m19=m19/1000
gen bajo_peso=0 if m19!=.
replace bajo_peso=1 if m19<2.5 & m19!=. & bajo_peso==0


* seis o más controles prenatales 
gen seis_prenatales=0 if m14!=.
replace seis_prenatales=1 if m14>=6 & m14<=20 & seis_prenatales==0
tab seis_prenatales


* IRA:
gen IRA=0 if hw1>=0 & hw1<=36
replace IRA=1 if h31==2 & h31b==1 & IRA==0
tab IRA


*EDA:
gen EDA=0 if hw1>=0 & hw1<=36
replace EDA=1 if h11==2   & EDA==0
tab EDA


* Anemia:
gen anemia= 0 if hw1>=6 & hw1<=59
replace anemia=1 if hw57>=1 & hw57<=3 & anemia==0 
tab anemia

* Parto institucional:
gen parto_institucional=0 if m15!=.
replace parto_institucional=1 if (m15>=21 & m15<=27) & parto_institucional==0
replace parto_institucional=1 if (m15==31 |  m15==32) & parto_institucional==0
tab parto

* Dosis de vitamina A:
* Recibio vitamina A en alun momento (niños de 6 a 36 meses)
gen vitamina_a=0 if hw1>=6 & hw1<=36
replace vitamina_a=1 if (h33>=1 & h33<=3) & vitamina_a==0
replace vitamina_a=2 if h34==1 & vitamina_a==1
tab vitamina_a

* Recibio al menos una dosis de vitamina A:
gen dosis_A= vitamina_a>=1 
replace dosis_A=. if vitamina_a==.
tab dosis_A

* Recibio dosis de hierro (ultimos 7 dias):
gen recibio_hierro=0 if hw1>=6 & hw1<=36
replace recibio_hierro=1 if h42==1 & recibio_hierro==0
tab recibio_hierro

replace v024=15 if v024==7

* Variable para identificar si la madre tiene hijo menor de 6 meses (esta info luego servira para crear la variable de lactancia)
bys caseid_madre: egen edad_menor=min(hw1) if hw1!=.
sort caseid_madre
save "$endes_trab\base_indicadoresDCI_2012.dta", replace


* creamos una pequeña base que identifica si la mujer tiene hijos menores de 6 meses
use "$endes_trab\base_indicadoresDCI_2012.dta", clear
collapse (min) edad_menor, by(caseid_madre)
count
gen hijo_0_6=0
replace hijo_0_6=1 if edad_menor>=0 & edad_menor<=5
sort caseid_madre
save "$endes_trab\mujer_hijos.dta", replace





************************************************
** Base para embarazo adolescente y lactancia **
************************************************

use "$endes_orig\rec0111.dta", clear
sort caseid
merge caseid using "$endes_orig\re212232.dta"
drop _merge
sort caseid
merge caseid using "$endes_orig\rec42.dta"
tab _merge
drop _merge
gen caseid_madre=caseid

* pegamos la pequeña base para identificar si tiene hijos menores de 6 meses (para lactancia exclusiva)
sort caseid_madre
merge caseid_madre using "$endes_trab\mujer_hijos.dta"
drop _merge

*primero el peso:
gen peso = v005/1000000
svyset v001 [weight=peso], strata(v022)

gen quintil = v190
gen area=v025
recode area 2=0
sort caseid

gen educamm=1 if v133<=5
replace educamm=2 if v133>=6 & v133<=11
replace educamm=3 if v133>=12
replace educamm=. if v133==.
gen nativamm=1 if v131==2  |  v131==3 |  v131==4
replace nativamm=0 if v131==1  | v131==5


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


* Lactancia exclusiva, debemos tener en cuenta si la mujer tiene hijos menores de 6 meses:

	* ahora lactancia:
gen lactancia_exclusiva=0 if hijo_0_6==1 & v409!=. 
replace lactancia_exclusiva=1 if (v409==0 & v410==0 & v410a==0 &  v411==0 &  v411a==0 & v412==0 &  v413==0 /*
*/ & v414a==0 & v414b==0 & v414c==0 & v414e==0 & v414f==0 & v414g==0 & v414h==0 & v414i==0 & v414j==0 & v414k==0 & v414l==0 & v414o==0 & v414p==0 & v414q==0 & v414r==0 & v414s==0 & v415==0) &  lactancia_exclusiva==0
tab lactancia


* Embarazo adolescente
gen teen_pregnant=0 if v012>=15 & v012<=19
replace teen_pregnant=1 if v212>=9 & v212<=19 & teen_pregnant==0
tab teen_pregnant


sort caseid_madre
save "$endes_trab\base_embarazo2012.dta", replace


****
****





**************************************************
** PREPARANDO LOS INDICADORES DE LA ESTRATEGIA ***
**************************************************

log using "$endes_result\Indicadores ENDIS a nivel hogar 2012.log", replace


*********************************
** INCLUIDOS EN LA BASE ENDES ***
*********************************

use "$endes_trab\base_desnutricion_2012_conpepi.dta", replace


svyset hv001 [weight=peso], strata(hv022)

*****************************
** 1. DESNUTRICIÓN CRÓNICA***
*****************************

* a) Tasa de desnutricion crónica (variable incluida en la base de hogares - con cada uno de los niños)

foreach x in   desnutricion_cronica_oms {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}

***
***

* b) Bajo peso al nacer
* c) Seis o más controles prenatales
* d) IRA
* e) EDA
* g) Anemia
* *) Lactancia (esta mas abajo, pues se usa otra base de solo mujeres, no de niños)
* h) Niños de 6 a 36 meses que recibieron vitamina A 
* i) Niños de 6 a 36 meses que recibieron  Hierro en los ultimos 7 dias


use "$endes_trab\base_indicadoresDCI_2012.dta", clear
count

gen peso = v005/1000000
svyset v001 [weight=peso], strata(v022)

svyset v001 [weight=v005], strata(v022)


foreach x in   bajo_peso seis_prenatales IRA EDA anemia dosis_A recibio_hierro {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


***************************************
** 2. DESARROLLO INFANTIL TEMPRANO ****
***************************************

* *) Porcentaje de madres que recibieron parto institucional

foreach x in parto {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


*	*) Mortalidad:

use "$endes_trab\base_mortalidad.dta", clear


foreach x in mortalidad {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}



**************************************************
** 3. DESARROLLO INTEGRAL NIÑEZ Y ADOLESCENCIA ***
**************************************************

* Tambien incluye la variable de lactancia (el ultimo niño por mujer)
use "$endes_trab\base_embarazo2012.dta", clear

* *) Lactancia exclusiva
* a) Incidencia de embarazo adolescente:

svyset v001 [weight=peso], strata(v022)

foreach x in teen_pregnant lactancia  {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


******************************
*****************************
*****************************


*********************************
** INCLUIDOS EN LA BASE ENAHO ***
*********************************

********************
** A NIVEL HOGAR ***
********************

*************************************
** 2. DESARROLLO INFANTIL TEMPRANO ****
***************************************

* a) Acceso a agua segura en el hogar
* b) Acceso a saneamiento en el hogar

use "$trab\hogar_pepi2012_lista.dta", clear
svyset conglome [weight=factor07],strata(estrato)


foreach x in    aguamej desgmej {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}

*****************************
** 4). INCLUSIÓN ECONÓMICA **
*****************************

* a) Pobreza monetaria usando ingreso autonomo (ese dato se calcula antes)
* b) Acceso a electricidad (hogar)
* c) Acceso a telefonia (hogar)


foreach x in     luz telf {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}

**************************************
** 5). PROTECCIÓN DEL ADULTO MAYOR  **
**************************************

* b) Brecha de pobreza en hogares con adultos mayores de 65 años a más (calculada antes)
* c) Tasa de pobreza subjetiva en hogares con adultos mayores de 65 años a más 


foreach x in     am_pobre_subj {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}

***
***
***

log close



log using "$endes_result\Indicadores ENDIS a nivel individual 2012.log", replace


*************************
** A NIVEL INDIVIDUAL ***
*************************

use "$trab\indiv_pepi2012_lista.dta", clear
count
sort conglome vivienda hogar  
merge conglome vivienda hogar using  "$trab\hogar_pepi2012_lista.dta"
drop _merge

svyset conglome [weight=facpob07],strata(estrato)


*************************************
** 2. DESARROLLO INFANTIL TEMPRANO ****
***************************************

* c) Asistencia de niños de entre 3 y 5 años a educación básica regular

foreach x in preescolar {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


*****************************************************
** 3. DESARROLLO INTEGRAL PARA LA NIÑEZ Y ADOLESCENCIA **
*********************************************************

* a) Jovenes de 18 a 25 años que culminaron educacion secundaria
* b) Porcentaje de niños y niñas a 6 a 11 años que asisten a educacion primaria
* c) Porcentaje de niños y niñas a 12 a 17 años que asisten a educacion secundaria
* d) Niños de 6 a 13 años que trabajan en epoca escolar 
* e) Adolescentes de 14 a 17 años que trabajan en epoca escolar


foreach x in  joven_culmino_secun asiste_primaria asiste_secundaria  nino_trabaja {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


svyset conglome [weight=fac500],strata(estrato)


foreach x in  adolescente_trabaja   {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}



*****************************
** 4). INCLUSIÓN ECONÓMICA **
*****************************

* Tasa de pobreza en poblacion de 18 a 64 años usando ingreso autonomo
* Tasa de pobreza extrema en poblacion de 18 a 64 años usando ingreso autonomo


*** TASA DE POBREZA EXTREMA USANDO SOLO GASTOS AUTONOMOS

povdeco gastopcautonomo [w=facpob07] if p208a>=18 & p208a<=64, varpl(linea) 
povdeco gastopcautonomo [w=facpob07] if hogar_vulnerable==1 & p208a>=18 & p208a<=64, varpl(linea)
*usar el FGT1-->a==0


*** TASA DE POBREZA EXTREMA USANDO SOLO GASTOS AUTONOMOS

povdeco gastopcautonomo [w=factor07] if p208a>=18 & p208a<=64, varpl(linpe) 
povdeco gastopcautonomo [w=factor07] if hogar_vulnerable==1 & p208a>=18 & p208a<=64, varpl(linpe)
*usar el FGT1-->a==0




/*
svyset conglome [weight=factor07],strata(estrato)

foreach x in   asiste_superior  {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}


svyset conglome [weight=fac500],strata(estrato)

foreach x in    PEArural_noagro {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}

*/

**************************************
** 5). PROTECCIÓN DEL ADULTO MAYOR  **
**************************************

* a) Tasa de pobreza extrema en los adultos mayores de 65 años a más (CREAR)


* d) Proporción de personas de 65 años a más que reciben una pensión
* e) Proporción de personas de 65 años a más que cuentan con seguro de salud
* f) Número de horas trabajadas a la semana (personas de 65 a más)
* g) Ingreso monetario promedio que recibe el adulto mayor de 65 años a más (CREAR)
* h) Proporción de personas de 65 años a más atendidos por un profesional de la salud ante enfermedad
* i) Porcentaje de personas de 65 años a más que declaran tener enfermedades, malestares, recaídas y/o accidentes sin tratamiento médico (CREAR)



svyset conglome [weight=factor07],strata(estrato)

foreach x in   am_pobre_ext recibe_pension am_seguro_salud am_sin_atencion {
svy linearized: mean `x'
svy linearized: mean `x', over (hogar_vulnerable)
}




log close

/*
*****************************
** 4). INCLUSIÓN ECONÓMICA **
*****************************

* concentracion de cultivos:

use "$trab\hogar_agricola.dta", clear
svyset conglome [weight= factora0],strata(estrato)

gen ingreso_mas_70=0 if concentracion!=.
replace ingreso_mas_70=1 if concentracion>0.7 & ingreso_mas_70==0
tab ingreso_mas_70

replace max_valor_hogar=. if max_valor_hogar>10000 & max_valor_hogar!=.
replace valor_total_hogar=. if valor_total_hogar>10000 & valor_total_hogar!=.


foreach x in    nro_productos_hogar max_valor_hogar valor_total_hogar  ingreso_mas_70 {
svy linearized: mean `x' 
svy linearized: mean `x', over (hogar_vulnerable)
}


* vinculacion con mercados

use "$trab\vinculacion_mercados.dta", clear
svyset conglome [weight= factora0],strata(estrato)

foreach x in     vinculo_2de5 {
svy linearized: mean `x' 
svy linearized: mean `x', over (hogar_vulnerable)
}


*/
