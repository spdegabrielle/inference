#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label scheme/base
                     "../inference.ss"))

@title[#:tag "rulesets"]{Rule Sets and Rules}

@local-table-of-contents[]

@section{Rule Sets}

A @deftech{rule set} is a collection of rules that together solve some (portion of a) problem.

@defform[(define-ruleset name)]{
Defines a ruleset with the given @scheme[name]. }

A rule set must be activated in an inference environment to be used in a inference.  (See @scheme[activate] in Section 4.1, Rule Set Activation).

Multiple rule sets may be active in any given inference environment.

Currently, there is no way to deactivate a rule set.

@section{Rules}

@subsection{Forward Chaining (Data-Driven) Rules}

@subsection{Backward Chaining (Goal-Driven) Rules}

@section{Pattern Clauses}

@subsection{Binding Pattern Clauses}

@subsection{Existential Pattern Clauses}

@section{Patterns}

@subsection{List Patterns}

@subsection{List Pattern Example---Ancestors}

@subsection{Association List Patterns}

@subsection{Association List Pattern Example---Ancestors}

@subsection{Vector Patterns}

@subsection{Vector Pattern Example---Ancestors}

@subsection{Structure Patterns}

@subsection{Structure Pattern Example---Ancestors}

@section{Conflict Resolution Strategies}

@subsection{Depth First}

@subsection{Depth First Example}

@subsection{Breadth First}

@subsection{Breadth First Example}

@subsection{Rule Order}

@subsection{Rule Order Example}

@subsection{Specificity}

@subsection{Specificity Example}

@subsection{Simplicity}

@subsection{Simplicity Example}

@subsection{Complexity}

@subsection{Complexity Example}

@subsection{Random}

@subsection{Random Example}
