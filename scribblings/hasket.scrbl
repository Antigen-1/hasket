#lang scribble/manual
@require[@for-label[hasket (except-in racket/base #%app) racket/contract racket/function]]

@title{hasket}
@author{zhanghao}

@defmodule[hasket #:lang]

@section{简介}

这个语言目前主要供作者自用。很多功能正在完善或经常改变。

大部分功能都是主要为函数式编程服务的，不过也可以和副作用混杂使用（如@tech{pipeline}）。

源码使用Apache-2.0 or MIT协议分发。

@section{函数}

@defproc[(Left (exception exn?)) any]
@defproc[(Right (value any/c)) any]

这两个函数分别用于在@tech{pipeline}中报错和返回，没有提供直接处理其返回值的工具，因此一般来说在@tech{pipeline}以外使用这两个函数是无意义的。

@defproc[(mapP (proc (-> any/c any))) (-> any/c any)]
@defproc[(joinP (m any/c)) any]

@racket[mapM]和@racket[joinM]。其中@racket[mapP]是使用@racket[curry]柯里化的。

@section{结构体}

@defstruct[errorR ([value any/c])]

这个结构体是为了让用户更方便地处理异常。
@tech{pipeline}中如果使用@racket[Left]报告了一个异常，不管最后是被catch还是返回，最终用户接触到的都是这个类型的结构体。
@racket[value]这个字段一般为@racket[at]结构体的实例。

@defstruct[at ([value any/c] [position (listof exact-nonnegative-integer?)])]

@racket[value]字段为传递给@racket[Left]的值。
@racket[position]字段则提供了一个易于定位的位置编码（详情见源码）。

@section{语法}

@defform[#:literals ($)
         (>>> value catch-or-step ...)
         #:grammar ([catch-or-step catch step]
                    [catch ($ step ...)])]
@defform[#:literals ($)
         (>>>/steps catch-or-step ...)
         #:grammar ([catch-or-step catch step]
                    [catch ($ step ...)])]

这些语法实现了@deftech{pipeline}。
@racket[value]可以是任意值；@racket[step]则接受这个任意值，而必须使用@racket[Left]和@racket[Right]返回。

@racket[>>>/steps]的功能是建立复合的@racket[step]。
@racket[>>>]则将@racket[step]组合起来，形成易于与@racket[racket]交互的@tech[pipeline]。

@defform[#:literals (!)
         (lambda/curry/match maybe-name maybe-contract (match-clause body ...) ...)
         #:grammar ([maybe-name (code:line #:name name)]
                    [maybe-contract (! contract-expr)])]
@defform[(curry/n procedure arity)
         #:contracts ([arity exact-nonnegative-integer?]
                      [procedure (and/c procedure?
                                        (lambda (p) (< (procedure-arity-mask p) 0))
                                        (lambda (p) (>= arity (- -1 (procedure-arity-mask p)))))])]

帮助用户实现haskell-style的“匹配+柯里化”函数。

这里主要就第一个作说明：

@itemlist[
          @item{@racket[maybe-name]可指定@racket[object-name]返回值，但由于柯里化，@racket[curry]会加前缀@racket[curried:]（默认为@racket['curried:temp]）。}
          @item{@racket[maybe-contract]指定的是未柯里化的函数的行为。}
          ]

@defform[(#%app . pair)
         #:grammar ([pair proc+args procs]
                    [proc+args (proc arg ...)]
                    [procs (first-proc . second-proc)])]

支持了composition。

@section{兼容性}

这门语言导出了@racket[racket/base]的内容作为primitives。
同时它与racket社区的其他许多工具可以无缝衔接。
更准确来说，这门语言只是对racket作了扩展。

@section{更新}

@itemlist[
          @item{2023.12.9 使用@racket[hasket-left]替换了原来的异常，这样就能保留异常的类型了。依然不直接使用@racket[errorR]，主要是保证数据抽象。}
          @item{2023.12.10 使用@racket[errorR]替换了@racket[hasket-left]，同时取消了对@racket[errorR]的内容限制。使用@racket[typed/racket]来处理position。此外为@racket[lambda/curry/match]添加了命名支持。}
          @item{2023.12.11 添加了haskell-style的复合函数，添加了@racket[joinP]和@racket[mapP]，修复了一些bug。}
          @item{2023.12.12 实现了@racket[step]的composition。}
          ]
