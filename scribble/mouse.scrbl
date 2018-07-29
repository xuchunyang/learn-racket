#lang scribble/base

@title{On the Cookie-Eating Habits of Mice}

If you give a mouse a cookie, he's going to ask for a
glass of milk.

@include-section["milk.scrbl"]
@include-section["straw.scrbl"]

@section{Itemizations}

@centered{@bold{Notice to Mice}}

@itemlist[@item{We have cookies for you.}
          @item{If you want to eat a cookie,
                you must bring your own straw.}]

@section{Tables}

@tabular[#:sep @hspace[1]
         (list (list @bold{Animal} @bold{Food})
               (list "mouse"       "cookie")
               (list "moose"       "muffin"))]

@section{Text Mode vs. Racket Mode for Arguments}

@itemlist[#:style 'ordered
          @item{foo}
          @item{bar}
          @item{baz}]

@italic{Yummy!}
@italic["Yummy!"]
@elem[#:style 'italic]{Yummy!}
@(elem #:style 'italic "Yummy!")

1 plus 2 is @(number->string (+ 1 2))

@section|{@ syntax Basic}|

@(require scriblib/figure)

"hello"

@verbatim|{@(number->string (+ 1 2))}|

@section{Pictures}

@(require pict)

This cookie has lost its chocolate chips:
@(colorize (filled-ellipse 40 40) "beige").
