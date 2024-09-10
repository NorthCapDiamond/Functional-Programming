# Guile

Дробыш Дмитрий Александрович P34082

## Выбранный язык 

Guile (Scheme)

## Фреймворк для тестирования

SRFI-64

## Линтер 

guile-lint?

## Книга 

The Scheme programming language,Fourth edition.

R. Kent Dybvig

https://www.scheme.com/tspl4/

## Explain

Я никогда не занимался функциональным программированием, поэтому решил взять язык, который изначально создавался для обучения, оставаясь при этом минималистичным. Конкретно Guile был выбран из соображения применимости после прохождения курса. Язык является частью экосистемы GNU, представляет механизм для работы с другими языками ( C, например ). Это значительно упрощает создание гибридных приложений.

Guile — это реализация языка программирования Scheme, который является диалектом языка Lisp.

B основном используется для следующих задач:

1. Системное программирование: Он может быть использован для написания системных утилит и инструментов, благодаря своей гибкости и мощным функциям. Пример: gdb.

2. Программирование на уровне приложений: Он может использоваться для разработки приложений ( + web ).

3. Образование: Scheme часто используются в учебных курсах по программированию из-за их простоты и выразительности.

4. Инструменты и утилиты: Guile может быть использован для создания утилит, скриптов и инструментов автоматизации.

Идея на lab4:
2d игра. + 1 человек с erlang ( Client-Server ).
Guile поддерживает работу с медиа: Cairo, SDL2, SFML

Code example:

```
#!/usr/bin/guile -s 
!#

(use-modules (srfi srfi-64))

(display "Hello, Dmitry Drobysh\n")

(define x 1)

(test-begin "1 Pass 1 Fail")
(test-equal x 1)
(test-equal x 2)
(test-end "1 Pass 1 Fail")

```
## Links
- https://www.gnu.org/software/guile/
- https://jeko.frama.io/en/hello.html
- https://user42.tuxfamily.org/guile-lint/index.html
- https://habr.com/ru/companies/tbank/articles/267015/  



