/* Solution of Task 12 Starting Guile from C */

#include <libguile.h>
#include <stdio.h>
#include <stdint.h>

int main(int argc, char** argv){

  SCM tailrec;
  scm_init_guile();

  scm_c_primitive_load("12_withc.scm");
  tailrec = scm_variable_ref(scm_c_lookup("inner"));


  printf("Enter your n\n");
  uint64_t n;
  scanf("%llu", &n);
  uint64_t res = 0;
  uint64_t answer = 0;
  uint64_t counter = 0;
  uint64_t current = 0;
  
  for(;;){
	  counter+=1;
	  current+=counter;
	  answer = 0;

	  res = scm_to_int(scm_call_4(tailrec, scm_from_int(current), scm_from_int(1), scm_from_int(answer), scm_from_int(n)));

	  if(res >= n){
		  printf("Answer is %d\n", current);
		  break;
	  }

  }


  return 0;
}

