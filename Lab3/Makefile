.PHONY: all format clean

all : format clean units

clean:
	-rm *.log 2>/dev/null

units:
	guile no-format/unity.scm


format:
	guile format.scm no-format/io.scm > format/io.scm
	guile format.scm no-format/lagrange.scm > format/lagrange.scm
	guile format.scm no-format/testing.scm > format/testing.scm
	guile format.scm no-format/linear.scm > format/linear.scm
	guile format.scm no-format/runner.scm > format/runner.scm
	guile format.scm no-format/util.scm > format/util.scm
	guile format.scm no-format/unity.scm > format/unity.scm
