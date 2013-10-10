#lang scribble/doc
@(require scribble/manual
          scribble/struct
          (for-label scheme/base
                     "../inference.ss"))

@title[#:style '(toc) #:tag "inference"]{@bold{Inference Collection:} Reference Manual}

PLT Scheme Inference Collection
@(make-element 'newline '())
Reference Manual
@(make-element 'newline '())
Edition 2.0

December 2008

@(author+email @tt{M. Douglas Williams} "m.douglas.williams@gmail.com")

The PLT Scheme Inference Collection implements an inference engine that supports  both forward-chaining (data-driven) and backward chaining (goal-driven) for developing rule-based systems in @link["http://www.plt-scheme.org"]{PLT Scheme}.  The inference engine:

@itemize{
  @item{Provides an efficient rule-based inference engine.} 
  @item{Supports forward-chaining for data-driven inferencing.}
  @item{Supports backward-chaining for goal-driven inferencing.}
  @item{Makes inferences directly on Scheme data - lists, assoication lists, vectors, and structures.}
  }

The source code is distributed with the inference collection and licensed under the GNU Lesser General Public License, Version 2.1 @cite["LGPL"].

All of the functionality defined in this manual is exported by the inference collection and accessed using the following form:

@defmodule[(planet williams/inference/inference)]

© 2006-2008, M. Douglas Williams

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.2 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license is included in the section entitled "GNU Free Documentation License".

@local-table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["using.scrbl"]
@include-section["env-basic.scrbl"]
@include-section["control-basic.scrbl"]
@include-section["assertions.scrbl"]
@include-section["rulesets.scrbl"]

@(bibliography
  (bib-entry #:key "LGPL"
             #:title "GNU Lesser General Public License, Version 2.1"
             #:author "Free Software Foundation, Inc."
             #:date "February 1999"
             #:url "http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html")
  (bib-entry #:key "FDL"
             #:title "GNU Free Documentation License, Version 1.2"
             #:author "Free Software Foundation, Inc."
             #:date "November 2002"
             #:url "http://www.gnu.org/copyleft/fdl.html")
  (bib-entry #:key "Williams90"
             #:title "Construction of Dynamic Stochastic Simulation Models Using Knowledge-Based Techniques"
             #:is-book? #t
             #:author "M.D. Williams"
             #:date "1990")
  (bib-entry #:key "Williams96"
             #:title "Design of a Knowledge-Based Simulation Environment"
             #:author "M.D. Williams, E.C. Gorman, and S.G. Shiva"
             #:location "Simulation 67:2, pp. 121-135"
             #:date "August 1996")
  )

@index-section[]
