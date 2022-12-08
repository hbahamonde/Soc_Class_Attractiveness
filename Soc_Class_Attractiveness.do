* This do file opens up original file and prepares it for R

clear all

* https://seafile.utu.fi/lib/c4ea71cb-195d-43e7-ab48-cae13be9af38/file/candidate_voter_data.dta?dl=1

use "/Users/hectorbahamonde/Research/Soc_Class_Attractiveness/ISODATA_clean2.dta", clear

decode v_attribuutti, generate(attractiveness_dimension) /* converts attributes to string */
decode k_SP, generate(candidate_gender) /* converts attributes to string */
rename v_arvio candidate_attractiveness
* keep this order below
rename id row_id
rename kuva id
* keep this order above
rename v_syntymävuosi voter_year_birth

keep if attractiveness_dimension == "Attraktiivisuus"

* net install iscogen /* make sure package is installed */
iscogen voter_isei = isei(v_isco), replace
iscogen candidate_isei = isei(k_isco1), replace

keep row_id id candidate_attractiveness candidate_gender voter_isei candidate_isei  voter_year_birth

label variable candidate_gender ""
label variable voter_isei ""
label variable candidate_isei ""

save "/Users/hectorbahamonde/Seafile/Soc_Class_Attractiveness/candidate_voter_data.dta", replace
