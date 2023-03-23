clear
set mem 500m

cd "C:\Documents and Settings\CESAR MORA\Escritorio\De interes\Bases"

global a "C:\Documents and Settings\CESAR MORA\Escritorio\De interes\Trabajadas"
global b "C:\Documents and Settings\CESAR MORA\Escritorio\De interes\Outputs"


** Variables de nivel individual **

* Ayacucho - Victor Fajardo.


foreach x in 21 {
foreach y in 03 {
use if ccdd=="`x'" & ccp=="`y'" using fpob`x'00.dta


* poblacion rural:
gen pob_rural=area=="2"

* sexo:
gen mujer=p02==2

* no afiliado a algun seguro:
gen no_seguro=p084==4
tab no_seguro

* analfabeto (solo mayores de 14 años)
gen analfabeto=0 if p03a>=15
replace analfabeto=1 if p10==2 & analfabeto==0
tab analfabeto

* lengua nativa:
gen lengua_nativa=0 if p09!=.
replace lengua_nativa=1 if (p09>=1 & p09<=4)
tab lengua_nativa

* tasa de asistencia escolar (6 a 12 años)
gen asistencia=0 if p03a>=6 & p03a<=12
replace asistencia=1 if p12==1 & asistencia==0
tab asistencia


* tasa de embarazo adolescente (para mujeres mayores de 20 años)
gen teen_preg20=0 if  p02==2 & p03a>=20
replace teen_preg20=1 if (p25<=19 & p25!=.) & teen_preg20==0
tab teen_preg20

* tasa de embarazo adolescente (para mujeres de 15 a los 19 años)
gen teen_preg1519=0 if  p02==2 & p03a>=15 & p03a<=19
replace teen_preg1519=1 if (p25<=19 & p25!=.) & teen_preg1519==0
tab teen_preg1519


* no tiene DNI (mayores de 18 años)
gen no_dni=p26==2 if p26!=.

* no tiene partida de nacimiento (niños hasta los 17 años)
gen no_partida=0 if p03a>=0 & p03a<=17
replace no_partida=1 if p04==2 & no_partida==0
tab no_partida


* no tiene partida de nacimiento (niños hasta los 3 años)
gen no_partida3=0 if p03a>=0 & p03a<=3
replace no_partida3=1 if p04==2 & no_partida3==0
tab no_partida3

log using "$b\ubi`x'`y'-individuos.log", replace
sum pob_rural- no_partida
log close


* hogar con niños de 6 a 12 años que no asisten a la escuela
gen child_noeduc=0
replace child_noeduc=1 if (p03a>=6 & p03a<=12) & asistencia==0
tab child_noeduc


* jefe de hogar con primaria incompleta:
gen jefe_educ=0 if p01==1
replace jefe_educ=1 if p11a==1 | p11a==2 & jefe_educ==0
replace jefe_educ=1 if p11a==3 & (p11b>=1 & p11b<=5) & jefe_educ==0
replace jefe_educ=1 if p11a==3 & (p11c>=1 & p11c<=5) & jefe_educ==0
tab jefe_educ

* persona ocupada:
gen ocupado=0
replace ocupado=1 if p13==1
replace ocupado=1 if (p14>=1 & p14<=4) & ocupado==0
tab ocupado if p03a>=14

save "$a\base_`x'`y'.dta", replace


* Guardando la info de que si en el hogar hay un jefe con primaria incompleta o menos
* y el numero de ocupados:

use "$a\base_`x'`y'.dta"
sort ccdd ccpp ccdi area codccpp zona seccion aeu aerini aerfin viv
collapse (max) jefe_educ child_noeduc (sum) ocupado, by(ccdd ccpp ccdi area codccpp zona seccion aeu aerini aerfin viv)
sort ccdd ccpp ccdi area codccpp zona seccion aeu aerini aerfin viv
save "$a\nbi_`x'`y'.dta", replace



* Colapsamos por distrito:
use "$a\base_`x'`y'.dta"
sort  ccdd ccpp ccdi
collapse (mean) pob_rural- no_partida, by(ccdd ccpp ccdi)
sort  ccdd ccpp ccdi
save "$a\ubi_`x'`y'-individuos.dta", replace


*****
*****

* Nivel de hogar:
use if ccdd=="`x'" & ccp=="`y'" using fviv`x'00, clear
keep if v02==1

* AGua potable:
gen agua_potable=0 if v02==1
replace agua_potable=1 if (v04==1 | v04==2| v04==3) 
tab agua_potable

* No Agua potable (todos los dias, todo el dia)
gen agua_potable_tododias=0 if agua_potable!=.
replace agua_potable_tododias=1 if agua_potable==1 & (v05==1 & v05a==24)
tab agua_potable_tododias


* No Desague:
gen no_desague=(v06>=3 & v06<=6) if v06!=.
tab no_desague

* No sshh:
gen no_sshh=(v06==6 | v06==5) if v06!=.
tab no_sshh

* No electricidad:
gen no_electricidad= v07==2 if v07!=.
tab no_electricidad


* NBIs:

* NBI1:
gen nbi1=0 if v03a!=.
replace nbi1=1 if v03a==5 & nbi1==0
replace nbi1=1 if v03b==1 & (v03a==4 | v03a==6 | v03a==6 | v03a==3 | v03a==8) & nbi1==0 
replace nbi1=1 if (v01==6 | v01==7) & nbi1==0
tab nbi1

* NBI2:
gen nbi2=0 if v08!=.
replace nbi2=1 if (ret/v08)>3 & nbi2==0
tab nbi2

*NBI3:
gen nbi3=no_sshh
tab nbi3


* Pegando la info
sort ccdd ccpp ccdi area codccpp zona seccion aeu aerini aerfin viv
count
merge ccdd ccpp ccdi area codccpp zona seccion aeu aerini aerfin viv using "$a\nbi_`x'`y'.dta"
keep if _merge==3
drop _merge


* NBI4:
gen nbi4=child_noeduc
tab nbi4

* NBI5:
gen nbi5=0
replace nbi5=1 if jefe_educ==1 & ocupado==0
tab nbi5

drop  jefe_educ child_noeduc ocupado


* NBI en total
egen nbi_total=rowtotal(nbi1 nbi2 nbi3 nbi4 nbi5)

* al menos una NBI
gen una_nbi=nbi_total>=1
tab una_nbi

* al menos dos NBI
gen dos_nbi=nbi_total>=2
tab dos_nbi

drop  nbi_total


log using "$b\ubi_`x'`y'-hogares.log", replace
sum agua_potable- dos_nbi
log close

* Colapsamos por distrito:
sort  ccdd ccpp ccdi
collapse  (mean) agua_potable- dos_nbi, by(ccdd ccpp ccdi)
sort  ccdd ccpp ccdi
save "$a\ubi_`x'`y'-hogares.dta", replace

erase "$a\base_`x'`y'.dta"
erase "$a\nbi_`x'`y'.dta"

}
}

***
***

* Merge y append

* ordenando:
foreach x in 0510 0803 0905 0901 1003 1310 1607 2003 2103 {
foreach y in individuos hogares {
use "$a\ubi_`x'-`y'.dta", clear
sort  ccdd ccpp ccdi
save, replace
}
}

* merge:
foreach x in 0510 0803 0905 0901 1003 1310 1607 2003 2103 {
use "$a\ubi_`x'-individuos.dta", clear
sort  ccdd ccpp ccdi
merge  ccdd ccpp ccdi using "$a\ubi_`x'-hogares.dta"
sort  ccdd ccpp ccdi
drop _merge
save "$a\ubi_`x'_global.dta", replace
}


* append
use "$a\ubi_0510_global.dta", clear
append using "$a\ubi_0803_global.dta"
append using "$a\ubi_0905_global.dta"
append using "$a\ubi_0901_global.dta"
append using "$a\ubi_1003_global.dta"
append using "$a\ubi_1310_global.dta"
append using "$a\ubi_1607_global.dta"
append using "$a\ubi_2003_global.dta"
append using "$a\ubi_2103_global.dta"

sort  ccdd ccpp ccdi
save "$a\Base_final.dta", replace



***
***

* Promedios provinciales

foreach x in 21 {
foreach y in 03 {
use if ccdd=="`x'" & ccp=="`y'" using fpob`x'00.dta




