
**POBLACION EN PROCESO DE INCLUSION e INDICADORES EMBLEMATICOS MIDIS**

clear
clear matrix
set mem 300m

foreach x in 05 11 {

global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

global endes_orig "E:\MIDIS\Datos\ENDES\20`x'\Originales"
global endes_trab "E:\MIDIS\Datos\ENDES\20`x'\Trabajadas"
global endes_result "E:\MIDIS\Datos\ENDES\20`x'\Resultados"


**************************
** PREPARANDO LAS BASES **
**************************


********************
** USANDO ENAHO ***
*******************

** 1) POBLACION EN PROCESO DE INCLUSION (POBLACION "MIDIS")

use "$orig\enaho01a_20`x'_300.dta", clear

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

use "$orig\sumaria_20`x'.dta", clear

sort conglome vivienda hogar
merge 1:1 conglome vivienda hogar using "$trab\educ300hogar.dta"
drop _merge
merge conglome vivienda hogar using "$orig\enaho01_20`x'_100"
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

*Población-hogares
svyset conglome [pw=factor07],strata(estrato)
svy: tab area hogar_vulnerable, count format(%12.0g)



* Pobreza monetaria:
gen hogar_pobre=pobreza<=2

* Agua segura:
gen aguamej=1 if p110==1 | p110==2 |p110==3
replace aguamej=0 if p110!=. & aguamej==.

* Saneamiento:
gen desgmej=1 if p111==1 | p111==2 |p111==3
replace desgmej=0 if p111!=. & desgmej==.

* Electricidad:
gen luz=1 if p1121==1
replace luz=0 if p1121!=. & luz==.

* Telefonia:
gen telf=1 if p1141==1 | p1142==1
replace telf=0 if p1141!=. & p1142!=. & telf==.

gen paquetess=1 if aguamej==1 & desgmej==1 & luz==1 & telf==1
replace paquetess=0 if paquetess==. & aguamej!=. & desgmej!=. & luz!=. & telf!=.
label var paquetess "el hogar accede al paquete completo (agua, desague, luz y telf)"

sort conglome vivienda hogar
merge conglome vivienda hogar using "$orig\enaho01_20`x'_100.dta"
keep if _merge==3
drop  _merge
sort conglome vivienda hogar
save "$trab\enaho_vulnerable20`x'.dta", replace
}



****
****

* Usamos módulos personales (200,300,400,500)
clear
clear matrix
set mem 400m

foreach x in 05 11 {
global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

use "$orig\enaho01_20`x'_200.dta", replace
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_20`x'_300.dta"
drop _merge
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_20`x'_400.dta"
drop _merge
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\enaho01a_20`x'_500.dta"
drop _merge
sort conglome vivienda hogar 
count


** variables:


* horas trabajadas mensuales:
gen horas_trabajadas=p513t
replace horas_trabajadas=. if p513t>=70 & p513t<=98
gen horas_mensuales=horas_trabajadas*4


* ingreso laboral
egen ing_act_prinm=rsum(i524a1 i530a)
egen ing_act_prine=rsum(d529t  d536)
egen ing_tot_act_prin=rsum(ing_act_prinm  ing_act_prine)


gen ing_act_mon=ing_act_prinm/12
gen ing_act_esp=ing_act_prine/12
gen ing_act_tot=ing_tot_act_prin/12

/*
* ingreso total:
gen ingreso_principal=0 if no_remunerado!=.
replace ingreso_principal=p524a1 if p524a1!=.
replace ingreso_principal=ingreso_principal*30 if p523==1
replace ingreso_principal=ingreso_principal*4 if p523==2
replace ingreso_principal=ingreso_principal*2 if p523==3

gen ganancia=0 if no_remunerado!=.
replace ganancia=p530a if p530a!=.
replace ganancia=. if ganancia==999999 

gen ingreso_total=ingreso_principal +ganancia
*/



*1** ingreso por hora:
gen ingreso_hora=ing_act_tot/horas_mensuales

*2** ingreso total
sum ing_act_tot


*4 participacion asoc. base
**

*5 horas de trabajo semanales
sum horas_trabajadas

*6 dci
**

*7 años de escolaridad:
gen escolaridad=0 if p301a==1 | p301a==2
replace escolaridad=p308b if p301a==3 & (p308b>=1 & p308b<=5) & escolaridad==.
replace escolaridad=p308c if p301a==3 & (p308c>=1 & p308c<=5) & escolaridad==.
replace escolaridad=6 if p301a==4 &  escolaridad==.
replace escolaridad=6+p308b if p301a==5 & (p308b>=1 & p308b<=4) & escolaridad==.
replace escolaridad=11 if p301a==6 
replace escolaridad=11+p308b if (p301a==7 | p301a==9) & escolaridad==.
replace escolaridad=14 if p301a==8 & escolaridad==.
replace escolaridad=16 if p301a==10 & escolaridad==.
replace escolaridad=16+p308b if p301a==11 & escolaridad==.


*8 tenencia de tierra:
**

*9 recibio atencion medica:
gen consulta_inst=0 if p4031!=.
replace consulta_inst=1 if (p4031==1 | p4032==1 | p4033==1 | p4034==1 | p4035==1 | p4036==1 | p4037==1 | p4038==1 | p4039==1) & consulta_inst==0

*11 PEA NO agricola:

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

gen PEA_noagro=0 if (ocu500>=1 & ocu500<=3)
replace PEA_noagro=1 if (cod_ocup2<=60 | cod_ocup2>=65) & cod_ocup2!=. & cod_ocup2!=97 & PEA_noagro==0


*10 PEA agricola:
gen PEA_agro=0 if PEA_noagro!=.
replace PEA_agro=1 if PEA_agro==0 & PEA_agro==0


* 12 trabajo no remunerado:
gen no_remunerado=0 if p507!=.
replace no_remunerado=1 if (p507==5 | p507==6) & no_remunerado==0

*13 analfabeto:
gen analfabeto=0
replace analfabeto=1 if p302==2

*14 seguro salud:
gen seguro_salud=0 if p4199!=.
replace seguro_salud=1 if p4199==0 & seguro_salud==0

sort conglome vivienda hogar codperso
save "$trab\enaho_individual20`x'.dta", replace
}

***
* Juntando base individual con base de hogar**
**********************************************

clear
clear matrix
set mem 600m

foreach x in 05 11 {
global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

use "$trab\enaho_individual20`x'.dta"
sort conglome vivienda hogar 
merge conglome vivienda hogar using "$trab\enaho_vulnerable20`x'.dta"
keep if _merge==3
drop _merge


*3** ingreso autonomo (nivel de hogar)
gen gashog2dautonomo=gashog2d- gru13hd1- gru23hd1- gru33hd1- gru43hd1- gru53hd1- gru63hd1- gru73hd1- gru83hd1
replace gashog2dautonomo=0 if gashog2dautonomo<0

gen gastopcautonomo=gashog2dautonomo/(mieperho*12)
label var gastopcautonomo "gasto pc mensual sin incluir juntos o donaciones publ en especie"


* 15 titulo de propiedad (nivel hogar)
gen propia_titulo=0 if p105a!=.
replace propia_titulo=1 if p106a==1 & propia_titulo==0 

*16-19 servicios
sum aguamej desgmej luz telf

* identificador de mujer y mujer jefa:

gen mujer=p207==2

gen mujer_jefa=0 if p203==1
replace mujer_jefa=1 if mujer==1 & mujer_jefa==0

* identificador de grupos de edad:

* mayor de 15 años
gen mayor_15=0 if p208a!=.
replace mayor_15=1 if p208a>=16 & p208a<=98 & mayor_15==0

sort conglome vivienda hogar codperso
count
save "$trab\enaho_hogar_indiv_20`x'.dta", replace
}



********************
********************

*Haciendo los calculos

*********************



clear
clear matrix
set mem 600m

foreach x in 05 11 {
global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

use "$trab\enaho_hogar_indiv_20`x'.dta"

log using "$result\Resultados SOCIAS 20`x'.log", replace


* Individual:
svyset conglome [pw=factor07],strata(estrato)

foreach y in ingreso_hora ing_act_tot horas_trabajadas escolaridad consulta_inst PEA_agro PEA_noagro no_remunerado analfabeto seguro_salud {
table mujer [pw=factor07], c(mean `y') row
table mujer [pw=factor07] if hogar_vulnerable==1, c(mean `y') row
}
* Hogar:
svyset conglome [pw=factor07],strata(estrato)

foreach y in gastopcautonomo propia_titulo aguamej desgmej luz telf {
table mujer_jefa [pw=factor07], c(mean `y') row
table mujer_jefa [pw=factor07] if hogar_vulnerable==1, c(mean `y') row
}
log close
}

**
** Datos faltantes:

* Tenencia de tierras:

clear
clear matrix
set mem 600m

global orig "E:\MIDIS\Datos\ENAHO\2005\Originales"
global trab "E:\MIDIS\Datos\ENAHO\2005\Trabajadas"

*2005
use "$orig\enaho02_2005_2000a.dta"
sort conglome vivienda hogar
gen tierra_propia=0
replace tierra_propia=1 if p2005d==1 & tierra_propia==0
collapse (max) tierra_propia,  by(conglome vivienda hogar)
sort conglome vivienda hogar
save "$trab\tenencia_tierra2005.dta", replace


global orig "E:\MIDIS\Datos\ENAHO\2011\Originales"
global trab "E:\MIDIS\Datos\ENAHO\2011\Trabajadas"


*2011
use "$orig\enaho02_2011_2000a.dta"
sort conglome vivienda hogar
gen tierra_propia=0
replace tierra_propia=1 if p2005d==1 & tierra_propia==0
collapse (max) tierra_propia,  by(conglome vivienda hogar)
sort conglome vivienda hogar
save "$trab\tenencia_tierra2011.dta", replace



* Añadimos las nuevas variables:



foreach x in 05 11 {

clear
clear matrix
set mem 600m



global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

use "$trab\enaho_hogar_indiv_20`x'.dta"
sort conglome vivienda hogar
merge conglome vivienda hogar using "$trab\tenencia_tierra20`x'.dta"
drop _merge
sort conglome vivienda hogar codperso
merge conglome vivienda hogar codperso using "$orig\participa_prog20`x'.dta"
drop _merge
replace participa=0 if participa==.


log using "$result\Resultados SOCIAS 20`x'_extra.log", replace

* Individual:
table mujer [pw=factor07], c(mean participa) row
table mujer [pw=factor07] if hogar_vulnerable==1, c(mean participa) row

* Hogar:
table mujer_jefa [pw=factor07], c(mean  tierra_propia) row
table mujer_jefa [pw=factor07] if hogar_vulnerable==1, c(mean  tierra_propia) row

log close
}


********************
** Variables extra**
********************

* Analfabeto
* Escolaridad
* PEA rural predominante

foreach x in 05 11 {


clear
clear matrix
set mem 600m

global orig "E:\MIDIS\Datos\ENAHO\20`x'\Originales"
global trab "E:\MIDIS\Datos\ENAHO\20`x'\Trabajadas"
global result "E:\MIDIS\Datos\ENAHO\20`x'\Resultados"

use "$trab\enaho_hogar_indiv_20`x'.dta"

log using "$result\Resultados SOCIAS 20`x'_extra2.log", replace

* Trabajadores:
gen agricultor=0 if cod_ocup1!=.
replace agricultor=1 if cod_ocup1==6 & agricultor==0

gen no_calificado=0 if cod_ocup1!=.
replace no_calificado=1 if cod_ocup1==9 & no_calificado==0


* Individual:
table mujer [pw=factor07], c(mean agricultor) row
table mujer [pw=factor07] if  hogar_vulnerable==1, c(mean agricultor) row

table mujer [pw=factor07] , c(mean no_calificado) row
table mujer [pw=factor07] if hogar_vulnerable==1, c(mean no_calificado) row

log close

}


/*
table mujer [pw=factor07] if mayor_15==1, c(mean analfabeto) row
table mujer [pw=factor07] if mayor_15==1 & hogar_vulnerable==1, c(mean analfabeto) row

table mujer [pw=factor07] if p208a>=18 & p208a<=98, c(mean escolaridad) row
table mujer [pw=factor07] if (p208a>=18 & p208a<=98) & hogar_vulnerable==1, c(mean escolaridad) row
*/

log close

}






**********
**********

* DCI **

clear
clear matrix
set mem 300m

global endes_orig "E:\MIDIS\Datos\ENDES\2011\Originales"
global endes_trab "E:\MIDIS\Datos\ENDES\2011\Trabajadas"
global endes_result "E:\MIDIS\Datos\ENDES\2011\Resultados"

use "$endes_trab\base_desnutricion2011.dta", clear


count
drop _merge
rename caseid xx
rename caseid_madre caseid
sort caseid
merge caseid using "$endes_orig\rec41.dta"
drop if _merge==2
drop _merge
sort caseid
merge caseid using "$endes_orig\rec43.dta"
drop if _merge==2
drop _merge
sort caseid

gen mujer=hc27==2 if hc27!=.
tab mujer
tab hc27
**
table mujer [pw=peso], c(mean desnutricion_cronica) row
table mujer [pw=peso] if hogar_vulnerable==1, c(mean desnutricion_cronica) row
