#lang scribble/manual
@require[@for-label[hasket (except-in racket/base #%app) racket/contract racket/function racket/match]]

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

@defform[#:kind "catch syntax"
         ($ catch-or-step ...)
         #:grammar ([catch-or-step catch step])]
@defform[#:kind "pipeline syntax"
         (>>> value catch-or-step ...)
         #:grammar ([catch-or-step catch step])]
@defform[#:kind "compound step syntax"
         (>>>/steps catch-or-step ...)
         #:grammar ([catch-or-step catch step])]

这些语法实现了@deftech{pipeline}。
@racket[value]可以是任意值；step则接受这个任意值，而必须使用@racket[Left]和@racket[Right]返回。

@racket[>>>/steps]的功能是建立复合的step；另外，如果在@racket[>>>/steps]中使用@racket[$]，catch的保护不会超出@racket[>>>/steps]。
@racket[>>>]则将step组合起来，形成易于与@racket[racket]交互的@tech{pipeline}。

另外要注意，@racket[$]是一个关键字，请不要@italic{shallow}其绑定。在@tech{pipeline}以外使用@racket[$]是一个语法错误。

@defform[#:literals (!)
         (lambda/curry/match maybe-name maybe-contract match-clause ...)
         #:grammar ([maybe-name (code:line #:name name)]
                    [maybe-contract (! contract-expr)])]
@defform[(curry/n procedure arity)
         #:contracts ([arity exact-nonnegative-integer?]
                      [procedure (and/c procedure?
                                        (lambda (p) (bitwise-bit-set? (procedure-arity-mask p) arity)))])]

帮助用户实现haskell-style的“匹配+柯里化”函数。

这里主要就第一个作说明：

@itemlist[
          @item{@racket[maybe-name]可指定@racket[object-name]返回值，但由于柯里化，@racket[curry]会加前缀@racket[curried:]（默认为@racket['curried:temp]）。}
          @item{@racket[maybe-contract]指定的是未柯里化的函数的行为。}
          ]

@defform[#:literals (lambda lambda/curry/match curry/n)
         (#%app . pair)
         #:grammar ([pair proc+args procs]
                    [proc+args (proc arg ...)]
                    [procs (then-proc ... . first-proc)]
                    [first-proc id (lambda p ...) (lambda/curry/match p ...) (curry/n p ...)])]

支持了函数组合，实际上是@racket[compose1]的别名。第一个函数对racket的reader作了一些妥协。

@section{优化}

@subsection{pipeline}

我们对@tech{pipeline}围绕@racket[>>>]和@racket[>>>/steps]实现了一个优化器。
这个优化器是通过用来实现@racket[>>>]和@racket[>>>/steps]的、未优化的@tech{pipeline}自举实现的。

对step list的通用优化包括以下几个@italic{passes}：

@itemlist[
          @item{step list中的@racket[>>>/steps]被递归优化。如果优化的结果是一个没有catch的compound step，则其step list将被inline进上级step list，否则原位保留优化结果。}
          @item{step list中的catch被递归优化且原位保留。}
          @item{step list中第一个@racket[Left]之后的step被消除。}
          @item{step list中末尾的catch被替换为@racket[Right]。}
          ]

对top-level step list（即@racket[>>>]的step list）的优化则包括：

@itemlist[
          @item{step list的通用优化（注意在这里step list末尾的catch都已被替换为@racket[Right]）。}
          @item{去除末尾的@racket[Right]。}
          ]

其他优化：

@itemlist[
          @item{对于@racket[>>>]，如果step list为空则被优化为输入的@racket[value]。}
          @item{对于@racket[>>>/steps]，如果step list为空则被优化为@racket[Right]。}
          ]

主要是通过@racket[>>>]、@racket[>>>/steps]、@racket[Left]、@racket[Right]和@racket[$]这些@deftech{hints}消除一些不必要的（un）boxing和step，
但也因此带来了一个问题：有一些step——甚至是一些有问题的step（包括语法问题和运行时的问题）——会被消除。

在这里作者不推荐使用者依赖这个设计缺陷（当然也可以理解为一个特性）来设计程序。
但作者也不会对此作出更改，因为根本上来讲它只会消除而不会引入安全问题。
使用者如果觉得有必要请自行添加相关检查。

另外作者并未实现一套完整的@italic{evaluation model}，只是通过那些@tech{hints}作优化。因此简单地封装这些函数和宏即可关闭优化器。

@section{兼容性}

这门语言导出了@racket[racket/base]的内容作为primitives。
同时它与racket社区的其他许多工具可以无缝衔接。
更准确来说，这门语言只是对racket作了扩展。

@section{更新}

@itemlist[
          @item{2023.12.9 使用@racket[hasket-left]替换了原来的异常，这样就能保留异常的类型了。依然不直接使用@racket[errorR]，主要是保证数据抽象。}
          @item{2023.12.10 使用@racket[errorR]替换了@racket[hasket-left]，同时取消了对@racket[errorR]的内容限制。使用@racket[typed/racket]来处理position。此外为@racket[lambda/curry/match]添加了命名支持。}
          @item{2023.12.11 添加了haskell-style的复合函数，添加了@racket[joinP]和@racket[mapP]，修复了一些bug。}
          @item{2023.12.17 为@tech{pipeline}实现了一个优化器，同时导出了@racket[$]。扩展了函数组合的语法。}
          @item{2023.12.19 简化了@racket[curry/n]的@racket[contract]和适用范围。}
          @item{2023.12.22 扩展了@racket[lambda/curry/match]使其支持完整的@racket[match-lambda**]语法。}
          ]
