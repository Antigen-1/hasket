#lang scribble/manual
@require[@for-label[hasket racket/base]]

@title{hasket}
@author{zhanghao}

@defmodule[hasket]

@section{简介}

这个语言目前主要供作者自用。很多功能正在完善或经常改变。

大部分功能都是主要为函数式编程服务的，不过也可以和副作用混杂使用（如@tech{pipeline}）。

源码使用Apache-2.0 or MIT协议分发。

@section{函数}

@defproc[(Left (exception exn?)) any]
@defproc[(Right (value any/c)) any]

这两个函数分别用于在@tech{pipeline}中报错和返回，没有提供直接处理其返回值的工具，因此一般来说在@tech{pipeline}以外使用这两个函数是无意义的。

@section{结构体}

@defstruct[(exn:fail:hasket exn:fail) ([position (listof exact-nonnegative-integer?)])]

这个结构体是为了让用户更方便地处理异常。
@tech{pipeline}中如果使用@racket[Left]报告了一个异常，不管最后是被catch还是返回，最终用户接触到的都是这个类型的结构体。
原来的exception的内容保留为@racket[message]和@racket[continuation-marks]两个字段。
新的@racket[position]字段则提供了一个易于定位的位置编码（详情见源码）。

@section{语法}

@defform[(>>> value catch-or-step ...)
         #:literals ($)
         #:grammar ([catch-or-step catch step]
                    [catch ($ step ...)])]

这个语法实现了@deftech{pipeline}。
@racket[value]可以是任意值；@racket[step]则接受这个任意值，而必须使用@racket[Left]和@racket[Right]返回。
@racket[(>>> value)]等同于@racket[value]。

@deffrom[(lambda/match/curry maybe-contract (match-clause body ...) ...)
         #:literals (!)
         #:grammar ([maybe-contract (! contract-expr)])]
@defform[(curry/n procedure arity)
         #:contracts ([arity exact-nonnegative-integer?]
                      [procedure (and/c procedure?
                                        (lambda (p) (< (procedure-arity-mask p) 0))
                                        (lambda (p) (>= arity (- -1 (procedure-arity-mask p)))))])]

帮助用户实现haskell-style的“匹配+柯里化”函数。

@section{兼容性}

这门语言导出了@racket[racket/base]的内容作为primitives。
同时它与racket社区的其他许多工具可以无缝衔接。
更准确来说，这门语言只是对racket作了扩展。
