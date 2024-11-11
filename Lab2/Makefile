.PHONY: all format clean

all : format clean rb-tree-unit rb-bag-unit rb-bag-property

clean:
	-rm *.log 2>/dev/null

rb-tree-unit:
	guile no-format/rb-tree-unit.scm

rb-bag-unit:
	guile no-format/rb-bag-unit.scm

rb-bag-property:
	guile no-format/rb-bag-property.scm

format:
	guile format.scm no-format/testing.scm > format/testing.scm
	guile format.scm no-format/rb-tree.scm > format/rb-tree.scm
	guile format.scm no-format/rb-tree-unit.scm > format/rb-tree-unit.scm
	guile format.scm no-format/rb-bag.scm > format/rb-bag.scm
	guile format.scm no-format/rb-bag-unit.scm > format/rb-bag-unit.scm
	guile format.scm no-format/rb-bag-property.scm > format/rb-bag-property.scm
