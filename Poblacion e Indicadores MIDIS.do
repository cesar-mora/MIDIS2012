
**POBLACION EN PROCESO DE INCLUSION e INDICADORES EMBLEMATICOS MIDIS**
clear
set mem 300m
clear all
glo enaho "E:\MIDIS\Datos\ENAHO\2010\Originales"
glo endes "E:\MIDIS\Datos\ENDES\2010\Originales"
glo temp "E:\MIDIS\Datos\ENAHO\2010\Trabajadas"


** 1) POBLACION EN PROCESO DE INCLUSION (POBLACION "MIDIS")

use "$enaho\enaho01a-2010-300.dta", clear

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
save "$temp\educ300hogar.dta", replace

*EXCLUSION 3 - I Quintil 
**Primer quintil del gasto total percapita del hogar (deflactado espacialmente)

use "$enaho\sumaria-2010.dta", clear
sort conglome vivienda hogar
merge 1:1 conglome vivienda hogar using "$temp\educ300hogar.dta"
drop _merge

gen gas_mes=gashog2d/12
gen gas_mespc=gas_mes/mieperho

*Deflactar por linea de pobreza
gen deflactor=linea

gen gas_mespcdef=gas_mespc/deflactor

gen factor07pob=factor07*mieperho
sort gas_mespcdef
xtile gas_qui [pw=factor07pob]=gas_mespcdef, nq(5)

recode educ_jh (0=.) (1/3=1) (4/5=2) (6=3), gen(educ_jh2)


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

svy: tab area exclusion1, count format(%12.0g)
svy: tab reg_nat exclusion1, count format(%12.0g)
svy: tab dpto_cod exclusion1, count format(%12.0g)

svy: tab area exclusion2, count format(%12.0g)
svy: tab reg_nat exclusion2, count format(%12.0g)
svy: tab dpto_cod exclusion2, count format(%12.0g)

svy: tab area exclusion3, count format(%12.0g)
svy: tab reg_nat exclusion3, count format(%12.0g)
svy: tab dpto_cod exclusion3, count format(%12.0g)

svy: tab area exclusion4, count format(%12.0g)
svy: tab reg_nat exclusion4, count format(%12.0g)
svy: tab dpto_cod exclusion4, count format(%12.0g)

svy: tab area hogar_vulnerable, count format(%12.0g)
svy: tab reg_nat hogar_vulnerable, count format(%12.0g)
svy: tab dpto_cod hogar_vulnerable, count format(%12.0g)

foreach  x in  jh_mujer jh_mujer_monop miembros numero_niños edad_jh lengua_jh educ_jh2 {
svy: tab  `x' reg_nat if hogar_vulnerable==1, count format(%12.0g)
}
foreach  x in  jh_mujer jh_mujer_monop miembros numero_niños edad_jh lengua_jh educ_jh2 {
svy: tab  `x' if dominio==8, count format(%12.0g)
svy: tab  `x', count format(%12.0g)
}
foreach  x in agua saneam luz telefono {
svy: tab  conexion_`x' reg_nat if hogar_vulnerable==1, count format(%12.0g)
}
foreach  x in agua saneam luz telefono {
svy: tab  conexion_`x' if dominio==8, count format(%12.0g)
svy: tab  conexion_`x', count format(%12.0g)
}
svy: tab  seguro_salud reg_nat if hogar_vulnerable==1, count format(%12.0g)

sort conglome vivienda hogar

apoverty gas_mespc [pw=factorpob],varpl(linpe) all

apoverty gas_mespc [pw=factorpob],varpl(linpe) pgr
apoverty gas_mespc [pw=factorpob] if hogar_vulnerable==1,varpl(linpe) pgr

apoverty gas_mespc [pw=factorpob],varpl(linea10) pgr
apoverty gas_mespc [pw=factorpob] if hogar_vulnerable==1,varpl(linea10) pgr

sort conglome vivienda hogar

save "$temp\sumaria2010_vulnerable.dta", replace


** 2) INDICADORES EMBLEMATICOS

***INDICADOR 1: ASISTENCIA PRE-ESCOLAR
*(% ninos 3/5 anos que asiste a escuela)

use "$enaho\enaho01a-2010-300.dta", clear
keep if p204==1
destring mes, replace
gen 	edninicial=1 if p306==1 & p307==1 & mes>=4 & (p208a==3 | p208a==4 | p208a==5) 
replace edninicial=0 if (p306==2 | (p306==1 & p307==2)) & mes>=4 & (p208a==3 | p208a==4 | p208a==5) 
tab p306 edninicial if mes>=4 & (p208a==3 | p208a==4 | p208a==5)
label var edninicial "asistencia a inicial o primaria de ninhos de 3 a 5a"
keep if (p208a==3 | p208a==4 | p208a==5) & mes>=4 
keep conglome vivienda hogar codperso edninicial p207 factor07 estrato
sort  conglome vivienda hogar
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(exclusion* hogar_vulnerable pobreza dpto_cod)

svyset conglome [pw=factor07], strata(estrato)

svy: tab hogar_vulnerable edninicial, row
svy: tab edninicial

forvalues x= 1/4 {
svy: tab exclusion`x' edninicial, row
}

* Estimacion por regiones
svy: tab dpto_cod edninicial, row



save "$temp\ii_edninicial.dta", replace


***INDICADOR 2: ACCESO A PAQUETE DE SERVICIOS BASICOS
*(% poblacion que vive en hogares con acceso a 4 servicios basicos)

use "$enaho\enaho01-2010-100.dta", clear
keep if result==1 | result==2

gen aguamej=1 if p110==1 | p110==2 |p110==3
replace aguamej=0 if p110!=. & aguamej==.

gen desgmej=1 if p111==1 | p111==2 |p111==3
replace desgmej=0 if p111!=. & desgmej==.

gen luz=1 if p1121==1
replace luz=0 if p1121!=. & luz==.

gen telf=1 if p1141==1 | p1142==1
replace telf=0 if p1141!=. & p1142!=. & telf==.

gen paquetess=1 if aguamej==1 & desgmej==1 & luz==1 & telf==1
replace paquetess=0 if paquetess==. & aguamej!=. & desgmej!=. & luz!=. & telf!=.
label var paquetess "el hogar accede al paquete completo (agua, desague, luz y telf)"

keep conglome vivienda hogar paquetess aguamej* desgmej* luz telf factor07 estrato
sort   conglome vivienda hogar  
merge conglome vivienda hogar using "$temp\sumaria2010_vulnerable.dta", keep(exclusion* hogar_vulnerable pobreza mieperho dpto_cod)

gen factor07pob=factor07*mieperho
svyset conglome [pw=factor07pob], strata(estrato)

svy: tab hogar_vulnerable paquetess, row
svy: tab paquetess

forvalues x= 1/4 {
svy: tab exclusion`x' paquetess, row
}

* Estimacion por regiones
svy: tab dpto_cod paquetess, row



sort   conglome vivienda hogar  
save "$temp\hh_paquetess.dta", replace


***INDICADOR 3: TASA DE POBREZA EXTREMA
*(% poblacion que vive en hogares con gasto percapita por debajo de linea de pobreza extrema)

use "$temp\sumaria2010_vulnerable.dta", clear

svyset conglome [pw=factor07pob], strata(estrato)

svy: tab hogar_vulnerable pobreza, row
svy: tab pobreza

forvalues x= 1/4 {
svy: tab exclusion`x' pobreza, row
}

* Estimacion por regiones
svy: tab dpto_cod pobreza, row


***INDICADOR 4: BRECHA DE POBREZA
*(distancia del gasto per capita del hogar respecto a la linea de pobreza)

povdeco gas_mespc [w=factor07pob], varpl(linea10) 
povdeco gas_mespc [w=factor07pob] if hogar_vulnerable==1, varpl(linea10)
*usar el FGT1-->a==1

forvalues x= 1/4 {
povdeco gas_mespc [w=factor07pob]  if exclusion`x'==1, varpl(linea10)
povdeco gas_mespc [w=factor07pob]  if exclusion`x'==0, varpl(linea10)
}

* Estimacion por regiones
forvalues x= 1/25 {
povdeco gas_mespc [w=factor07pob] if dpto_cod==`x', varpl(linea10) 
}






***INDICADOR 5: TASA DE POBREZA EXTREMA USANDO SOLO GASTOS AUTONOMOS
*(distancia del gasto autonomo per capita del hogar respecto a la linea de pobreza)

* Gasto autónomo=gasto total del hogar neto de transferencias publicas monetarias o en especie. 
* Se quita: 1) transferencias JUNTOS (ingtpu01) y 2) donaciones en especie (alimentos, ... todos los rubros de gasto)

gen gashog2dautonomo=gashog2d-ingtpu01- gru13hd1- gru23hd1- gru33hd1- gru43hd1- gru53hd1- gru63hd1- gru73hd1- gru83hd1
replace gashog2dautonomo=0 if gashog2dautonomo<0
summ ingtpuhd   ingtpu01  ingtpu02

gen gastopcautonomo=gashog2dautonomo/(mieperho*12)
label var gastopcautonomo "gasto pc mensual sin incluir juntos o donaciones publ en especie"

povdeco gastopcautonomo [w=factor07pob], varpl(linpe10) 
povdeco gastopcautonomo [w=factor07pob] if hogar_vulnerable==1, varpl(linpe10)
*usar el FGT1-->a==0

forvalues x= 1/4 {
povdeco gastopcautonomo [w=factor07pob] if exclusion`x'==1, varpl(linea10)
povdeco gastopcautonomo [w=factor07pob] if exclusion`x'==0, varpl(linea10)
}


* Estimacion por regiones
forvalues x= 1/25 {
povdeco gastopcautonomo [w=factor07pob] if dpto_cod==`x', varpl(linpe10) 
}


save "$temp\sumaria2010_vulnerable_v2.dta", replace


use "$temp\sumaria2010_vulnerable_v2.dta", clear

* Indicadores emblematicos a nivel nacional


***INDICADOR 6: ENDES - DESNUTRICIÓN CRÓNICA (OMS)

use "$endes\rech0.dta", clear
sort hhid
save "$temp\rech0.dta", replace

use "$endes\rech1.dta", clear
sort hhid hc0
save "$temp\rech1", replace

use "$endes\rech23.dta", clear
sort hhid 
save "$temp\rech23", replace

use "$endes\rech4.dta", clear
sort hhid hc0
save "$temp\rech4", replace

use "$endes\rech6.dta", clear
sort hhid hc0
save "$temp\rech6", replace
 
use "$endes\rech6.dta", clear
merge hhid hc0 using "$temp\rech1"
tab _m
rename _m merge_rech6_rech1
sort hhid
merge hhid using "$temp\rech23"
tab _m
rename _m merge_rech23
sort hhid
merge hhid using "$temp\rech0.dta"
tab _m
rename _m merge_rech0
sort hhid hc0
merge hhid using "$temp\rech4"
tab _m
rename _m merge_rech4
save "$temp\base_desnutricion.dta", replace

*primero el peso:
gen peso = hv005/1000000
svyset hv001 [weight=peso], strata(hv022)

*genero identificador de hogar a nivel individual
generate caseid=hhid
generate id=string(hc0)
replace caseid = caseid + "  " +id if hc0<10
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

sort caseid
save "$temp\base_desnutricion.dta", replace

*PEGANDO VARIABLE DE LENGUA DE LA MADRE
gen niños_menores5 = (hc1 >= 0 & hc1 <= 59)
keep if niños_menores5 == 1
*me quedo solo con niños cuya madre vive en el hogar
drop if hv112 == 0
save "$temp\niños_0a5.dta", replace

*genero el caseid de la mama para pegarle ahi las variables de etnicidad de la madre
generate caseid_madre=hhid
generate id2=string(hv112)
replace caseid_madre = caseid_madre + "  " +id2 if hv112<10
replace caseid_madre = caseid_madre + " "  +id2 if  hv112>9
sort caseid_madre
save "$temp\niños_0a5.dta", replace
*OJO ESTA BASE SOLO ES DE NIÑOS QUE TIENEN DATO DE MADRE EN EL HOGAR (PARA JUNTAR CON LA BASE DE LENGUA)

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

use "$temp\niños_0a5.dta"
merge caseid_madre using "$temp\lengua_madre.dta"
drop if _m == 2
rename _m merge_v131
keep caseid niños_menores5 caseid_madre nativamm educamm merge_v131
sort caseid
save "$temp\niños_0a5.dta", replace

use "$temp\base_desnutricion.dta"
sort caseid
merge caseid using "$temp\niños_0a5.dta"
gen sex =hc27
recode sex 2=0
gen quintil = hv270
gen area=hv025
recode area 2=0
save "$temp\base_desnutricion", replace

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

keep if desnutricion_cronica_oms!=.

svy linearized: mean desnutricion_cronica_oms
svy linearized: mean desnutricion_cronica_oms, over (hogar_vulnerable)
svy linearized: mean desnutricion_cronica_oms, over (exclusion1)
svy linearized: mean desnutricion_cronica_oms, over (exclusion2)
svy linearized: mean desnutricion_cronica_oms, over (exclusion3)
svy linearized: mean desnutricion_cronica_oms, over (exclusion4)

* estimacion por regiones:
svy linearized: mean desnutricion_cronica_oms, over (hv024)

