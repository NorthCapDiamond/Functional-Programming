all : clean run

clean:
	-rm *.log 2>/dev/null

format:
	guile format.scm rb-bag.scm > rb-bag-format.scm
run: 
	guile rb-bag-format.scm
