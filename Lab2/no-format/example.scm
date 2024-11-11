(include "rb-bag.scm")

(display
	(rbmset->list (append-rbmset "My" (append-rbmset "Name" (append-rbmset "Is" ( create-rbmset "Drobysh Dmitry"))))))
