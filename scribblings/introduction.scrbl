#lang scribble/doc
@(require scribble/manual
          scribble/struct)

@title[#:tag "introduction"]{Introduction}

The PLT Scheme Inference Collection implements an inference engine that supports  both forward-chaining (data-driven) and backward chaining (goal-driven) for developing rule-based systems in @link["http://www.plt-scheme.org"]{PLT Scheme}.  The inference engine:

@itemize{
  @item{Provides an efficient rule-based inference engine.} 
  @item{Supports forward-chaining for data-driven inferencing.}
  @item{Supports backward-chaining for goal-driven inferencing.}
  @item{Makes inferences directly on Scheme data - lists, assoication lists, vectors, and structures.}
  }

The source code is distributed with the inference collection and licensed under the GNU Lesser General Public License, Version 2.1 @cite["LGPL"].

The motivation behind the PLT Scheme Inference Collection is to provide the inference engine for knowledge-based simulation in @link["http://www.plt-scheme.org"]{PLT Scheme}.  It is based on a knowledge-based simulation system originally written in Symbolics Common Lisp @cite["Williams90"] @cite["Williams96"].  This is not as much a port of the earlier work as it is a complete re-engineering of the system into PLT Scheme.

@section{Routines Available in the Inference Collection}

The PLT Scheme Inference Collection cover a range of functionality for developing and executing rule-based systems:

@itemize{
  @item{Inference Environments (Basic)}
  @item{Inference Control}
  @item{Assertions}
  @item{Rule Sets}
  @item{Rules
        @itemize{
          @item{Data-Driven (Forward Chaining)}
          @item{Goal-Driven (Backward Chaining)}
          }}
  @item{Rule Network}
  @item{Conflict Resolution Strategy}
  @item{Inference Environment (Hierarchical)
        @itemize{
          @item{Import}
          @item{Export}
          }}
  }

The use of these functions is described in this manual.  Each chapter provides detailed definitions of the functions, with example code.

@section{The Inference Collection is Free Software}

The PLT Scheme Inference Collection is free software---this means that anyone is free to use it and redistribute it in other free programs.  The inference collection is not in the public domain---it is copyrighted and there are conditions on its distribution.  Specifically, the PLT Scheme Inference Collection is distributed under the GNU Lesser General Public License, Version 2.1.  A copy of the LGPL is provided with the software.

@section{Obtaining the Inference Collection}

The preferred method for obtaining the PLT Scheme Inference Collection is via @link["http://planet.plt-scheme.org/"]{PLaneT}, PLT Scheme's centeralized package distribution system.  The PLaneT identifier for the PLT Scheme Inference Collection is @scheme[(planet williams/inference/inference)], which will download and install the infernece collection.  See Chapter 2 for an example.

The source code is maintained on the @link["http://schematics.sourceforge.net"]{Schematics} project website and Subversion repository at @link["http://sourceforge.net"]{SourceForge}.

Note that Version 2.0 and higher of the PLT Scheme Inference Collection requires PLT Scheme Version 4.0 or higher.

@section{No Warranty}

The PLT Scheme Inference Collection is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  It is your responsibility to validate the behavior of the software and their accuracy using the source code provided.  See the GNU Lesser General Public License, Version 2.1 @cite["LGPL"] for more details.

