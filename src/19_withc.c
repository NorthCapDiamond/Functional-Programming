#include <stdio.h>
#include <stdint.h>
#include <libguile.h>

int main(){
	printf("Enter: year_start month_start day_start yer_limit month_limit day_limit\n");
	uint64_t year_start = 1901;
	uint64_t month_start = 1;
	uint64_t day_start = 1;

	uint64_t year_limit = 2000;
	uint64_t month_limit = 12;
	uint64_t day_limit = 31;

	uint64_t res;

	//scanf("%lu", &year_start);
	//scanf("%lu", &month_start);
	//scanf("%lu", &day_start);

	//scanf("%lu", &year_limit);
	//scanf("%lu", &month_limit);
	//scanf("%lu", &day_limit);

	uint64_t sunny = 0;

	SCM solution_f;	
	scm_init_guile();
	scm_c_primitive_load("19_withc.scm");
	solution_f = scm_variable_ref(scm_c_lookup("solution"));

	res = scm_to_int(scm_call_7(solution_f, scm_from_int(year_start), scm_from_int(month_start), scm_from_int(day_start), scm_from_int(year_limit), scm_from_int(month_limit), scm_from_int(day_limit), scm_from_int(sunny)));

	printf("Answer is %ld\n", res);
	return 0;

}
