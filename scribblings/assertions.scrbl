#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label scheme/base
                     "../inference.ss"))

@title[#:tag "assertions"]{Facts and Assertions}

@local-table-of-contents[]

In a rule-based system, an @deftech{assertion} represents a fact that has been asserted. Assertions are an important aspect of forward chaining inference and most of the operations on assertions are defined in Section 4.2, Forward Chaining Control.

The discussion of assertions in this chapter is limited to the @scheme[assertion] structure.

@section{Facts}

In the PLT Scheme Inference Collection, facts are represented by structured Scheme objects.  Currently, facts can be represented by:

@itemize{
  @item{lists}
  @item{association lists}
  @item{vectors}
  @item{structures}
  }

For list, association list, and vector facts must have an initial element that is a symbol. This initial symbol defines the @deftech{kind} of the fact---I didn't want to use the term class since that could be confused with classes from the PLT Scheme class system. There is no inherent semantics associated with facts. They are just structured data that are matched against patterns in rules.

The folowing sections give examples of facts from the simple ancestors example that show facts in the context of assertions.

@subsection{List Facts}

A @deftech{list fact} is a fact represented by a Scheme list structure. The fact must be a non-empty list of atoms with the first element being a symbol demoting the kind of fact represented.  The requirement for atomic elements differentiates a list fact from an association list fact. List facts are matches against list patterns by position.

The following is an example of the assertion of list facts from the ancestors example.

@schemeblock[
(assert '(parents penelope jessica jeremy))
(assert '(parents jessica mary-elizabeth homer))
(assert '(parents jeremy jenny steven))
(assert '(parents steven loree john))
(assert '(parents loree #f jason))
(assert '(parents homer stephanie #f))
]

@subsection{Association List Facts}

An @deftech{association list fact} is a fact represented by a Scheme list structure with at least one named field. The fact must be a non-empty list whose first element is a symbol denoting the kind of fact represented, followed by any number (including zero) of atomic elements, followed by at least one pair whose first element is a symbol. Association list fact are matched against association list patterns by position for the initial atomic elements and by name for the rest.

The following is an example of the assertion of association list facts from the ancestors example.

@schemeblock[
(assert '(parents penelope (mother . jessica) (father . jeremy)))
(assert '(parents jessica (mother . mary-elizabeth)
                          (father . homer)))
(assert '(parents jeremy (mother . jenny) (father . steven)))
(assert '(parents steven (mother . loree) (father . john)))
(assert '(parents loree (mother . #f) (father . jason)))
(assert '(parents homer (mother . stephanie) (father . #f)))
]

@subsection{Vector Facts}

A @deftech{vector fact} is a fact represented by a Scheme vector. The fact must be a non-empty vector whose first element is a symbol denoting the kind of fact represented. Vector facts are matched against vector patterns by position.

The following is an example of the assertion of vector facts from the ancestors example.

@schemeblock[
(assert #(parents penelope jessica jeremy))
(assert #(parents jessica mary-elizabeth homer))
(assert #(parents jeremy jenny steven))
(assert #(parents steven loree john))
(assert #(parents loree #f jason))
(assert #(parents homer stephanie #f))
]

@subsection{Structure Facts}

A @deftech{structure fact} is a fact represented by a Scheme structure. The fact must be a structure instance. Structure facts are matched against structure patterns by position.

The following is an example of the assertion of structure facts from the ancestors example.  Note that the structure definition must include an inspector to allow it fields to be examined.

@schemeblock[
(defstruct parents (name mother father) (make-inspector))

(assert (make-parents 'penelope 'jessica 'jeremy))
(assert (make-parents 'jessica 'mary-elizabeth 'homer))
(assert (make-parents 'jeremy 'jenny 'steven))
(assert (make-parents 'steven 'loree 'john))
(assert (make-parents 'loree #f 'jason))
(assert (make-parents 'homer 'stephanie #f))
]

@section{The @scheme[assertion] Structure}

@schemeblock[
(struct assertion
        (_id
         _fact
         _reason))

  id : exact-positive-integer?
  fact : fact?
  reason : any/c
]
Defines an assertion.

@itemize{
  @item{@schemefont{id}---a unique id within the inference environement in which it was asserted. This is automatically generated.}
  @item{@schemefont{fact}---the asserted fact.}
  @item{@schemefont{reason}---the reason given for the fact being asserted. This is generally the rule instance whose execution result in the assertion, or false, @scheme[#f].}
  }

@defproc[(make-assertion (fact fact?) (reason any/c)) assertion?]{
Returns a new assertion using @scheme[fact] and @scheme[reason] and the next-assertion-id from the current inference environment. This call does not propogate the assertion through the rule network.}

