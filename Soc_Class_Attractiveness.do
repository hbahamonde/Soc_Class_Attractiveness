* This do file opens up original file and prepares it for R

clear all
use "/Users/hectorbahamonde/Research/Soc_Class_Attractiveness/ISODATA_clean2.dta", clear

decode v_attribuutti, generate(attractiveness_dimension) /* converts attributes to string */
decode k_SP, generate(candidate_gender) /* converts attributes to string */
rename v_arvio candidate_attractiveness
rename kuva candidate_id
rename id row_id
rename v_syntymävuosi voter_year_birth

keep if attractiveness_dimension == "Attraktiivisuus"

* net install iscogen /* make sure package is installed */
iscogen voter_isei = isei(v_isco), replace
iscogen candidate_isei = isei(k_isco1), replace

keep row_id candidate_id candidate_attractiveness candidate_gender voter_isei candidate_isei  voter_year_birth

label variable candidate_gender ""
label variable voter_isei ""
label variable candidate_isei ""

save "/Users/hectorbahamonde/Seafile/Soc_Class_Attractiveness/candidate_voter_data.dta", replace
