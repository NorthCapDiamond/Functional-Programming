#include <stdio.h>
#include <stdint.h>
#include <libguile.h>

int main(){
	printf("Enter: year_start month_start day_start yer_limit month_limit day_limit\n");
	uint64_t year_start;
	uint64_t month_start;
	uint64_t day_start;

	uint64_t year_limit;
	uint64_t month_limit;
	uint64_t day_limit;

	uint64_t res;

	scanf("%llu", &year_start);
	scanf("%llu", &month_start);
	scanf("%llu", &day_start);

	scanf("%llu", &year_limit);
	scanf("%llu", &month_limit);
	scanf("%llu", &day_limit);

	uint64_t sunny = 0;

	SCM solution_f;	
	scm_init_guile();
	scm_c_primitive_load("19_withc.scm");
	solution_f = scm_variable_ref(scm_c_lookup("solution"));

	res = scm_to_int(scm_call_7(solution_f, scm_from_int(year_start), scm_from_int(month_start), scm_from_int(day_start), scm_from_int(year_limit), scm_from_int(month_limit), scm_from_int(day_limit), scm_from_int(sunny)));

	printf("Answer is %d\n", res);
	return 0;

}
