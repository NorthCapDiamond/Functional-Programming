# Gulie

Выбранный язык : Guile (Scheme)

Фреймворк для тестирования : SRFI-64

Линтер : None ( guile-lint???)

Книга : The Scheme programming language,Fourth edition.

R. Kent Dybvig

https://www.scheme.com/tspl4/

Я никогда не занимался функциональным программированием, поэтому решил взять язык, который изначально создавался для обучения, оставаясь при этом минималистичным. Конкретно Guile был выбран из соображения применимости после прохождения курса. Язык является частью экосистемы GNU, представляет механизм для работы с другими языками ( C, например ). Это значительно упрощает создание гибридных приложений 

Идея на lab4:
2d игра. + 1 человек с erlang.
Guile поддерживает работу с медиа: Cairo, SDL2, SFML

Code example:

```
#!/usr/bin/guile -s 
!#

(use-modules (srfi srfi-64)
             )

(display "Hello, Dmitry Drobysh\n")

(define x 1)

(test-begin "1 Pass 1 Fail")
(test-equal x 1)
(test-equal x 2)
(test-end "1 Pass 1 Fail")

```
