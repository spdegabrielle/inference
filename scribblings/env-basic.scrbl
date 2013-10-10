#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label scheme/base
                     "../inference.ss"))

@title[#:tag "env-basic"]{Inference Environments (Basic)}

@local-table-of-contents[]

An @tech{inference environment} encapsulates the state of an executing inference system. This state information includes the following:

@itemize{
  @item{Rule network (nodes, data index, goal, index, rule node list)}
  @item{Assertion index (2 levels) and next assertion id}
  @item{Exit continuation}
  @item{Agenda}
  @item{Current rule, trace flag, and conflict resolution strategy}
  }

It is a good practice to create a new inference environment for all of your inference systems. The @scheme[with-new-inference-environment] facilitates this and is used in all of the examples in this document. The primary advantage is to ensure a clean environment for each new execution of the inference system.

The PLT Scheme Inference Collection supports multiple inference environments existing at the same time.  

This chapter describes the features of basic simulation environments.

@section{The @scheme[inference-environment] Structure}

The @scheme[inference-environment] structure defines an inference environment.

@schemeblock[
(struct inference-environment (_data-index
                               _goal-index
                               _rule-nodes
                               _exit
                               _next-assertion-id
                               _assertion-index
                               _trace
                               _agenda
                               _rule
                               _strategy
                               _parent
                               _rules-fired))

  _data-index : hash-eq?
  _goal-index : hash-eq?
  _rule-nodes : (listof rule-node?)
  _exit : (or/c procedure? false/c)
  _next-assertion-id : exact-positive-integer?
  _assertion-index : hash-eq?
  _trace : boolean?
  _agenda : (mlistof rule-instance?)
  _rule : (or/c rule-instance? false/c)
  _strategy : (one-of/c 'depth 'breadth 'order 'simplicity 'complex 'random)
  _parent : (or/c inference-environment? false/c)
  _rules-fired : exact-nonnegative-integer?
]

@(margin-note finger "See Section 3.3 Current Inference Environment Fields for more information on accessing the fields of the current inference environment.")

@itemize{
  @item{@schemefont{data-index}---an eq? hash table that provides an index of (non-goal) match nodes for each fact kind. This field is maintained internally.}
  @item{@schemefont{goal-index}---an eq? hash table that provides an index of goal match nodes for each fact kind. This field is maintained internally.}
  @item{@schemefont{rule-nodes}---a list of the rule nodes for the activated rule sets. This field is maintained internally.}
  @item{@schemefont{exit}---the continuation to exit the inference currently running in the environment, or false, @scheme[#f], if no inference is currently running in the environment. This field is used internally.}
  @item{@schemefont{next-assertion-id}---the id to be used for the next assertion.  This is auto-incremented each time an assertion is created. This field is maintained internally.}
  @item{@schemefont{assertion-index}---the level-1 assertion index for the environment.  Each entry in the level-1 assertion index is a level-2 index. This field is used internally.}
  @item{@schemefont{trace}---if true, @scheme[#t], information on the current inference (e.g., assertions, deletions, and rule firings) is printed.  The default is false, @scheme[#f]. This may be specified by the user.}
  @item{@schemefont{agenda}---a list of the rule instances that are currently eligible to fire. This list is maintained in a priority order.  The ordering is controlled by the current conflict resolution strategy.  This field is used internally.}
  @item{@schemefont{rule}---the currently executing rule instance, or false, @scheme[#f], if no rule is currently being executed.}
  @item{@schemefont{strategy}---the conflict resolution strategy used to order the agenda. This determines the order in which rules or equal priority are fired. The available conflict resolution strategies are:
        @itemize{
          @item{@schemefont{depth}---depth first. New rule instances are added to the agenda after all other rule instances of equal (or higher) priority.}
          @item{@schemefont{breadth}---breadth first. New rule instances are added at to the agenda before all of ther rule instance ot equal (or lower) priority.}
          @item{@schemefont{order}---rule order. Rule instances of equal priority on the agenda are ordered according to the order of the rules in their corresponding rule sets.}
          @item{@schemefont{simplicity}---simplest first. Rule instance of equal priority on the agenda are ordered such that the simplest rule instances (i.e., the ones with the fewest preconditions) are first.}
          @item{@schemefont{complex}---most complex first. Rule instance of equal priority on the agenda are ordered such that the most complex rule instances (i.e., the ones with the most preconditions) are first.}
          @item{@schemefont{random}---random.  Rules instances of equal priority on the agenda are in random order.}
          }
        This may be specified by the user.}
  @item{@schemefont{parent}---the parent inference environment, or false, @scheme[#f], if this is a top-level inference environment. (See Chapter ???.)}
  @item{@schemefont{rules-fired}---the number of rules instances that have been executed by the current inference.}
  }

@defproc[(make-inference-environemnt (parent (or/c inference-environment false/c) #f))
         inference-environment?]{
Returns a new inference environment with the given @scheme[parent] inference environment. The newly created inference environment is empty. That is, there are no rule sets activated, no facts are asserted, user specifiable fields have their default values, and no inference is running in the environment.}
                                
@defidform[default-inference-environment]{
Contains the inference environment that is used as the default value for the current inference environment (see Section 3.2).}

@section{Current Inference Environment}

The @tech{current inference environment} is the inference environment that is the current focus for most of the inference calls. It is both thread and continuation specific.

@defparam[current-inference-environment env inference-environment?]{
Gets or sets the current inference environment. A guard procedure ensures that the value of @scheme[current-inference-environment] is indeed an inference environment, as determined by @scheme[inference-environment?], otherwise a type error is raised.}

@defform[(with-inference-environment env
            body ...+)]{
Evaluates its body with the current inference environment bound to the value of @scheme[env].}

@defform[(with-new-inference-environment
            body ...+)]{
Evaluates its body with the current inference environment bound to a newly created inference environment. This is the most common way to create and use a new inference environment.}

@defform[(with-new-child-inference-environment
            body ...+)]{
Evaluates its body with the current inference environment bound to a newly created inference environment that is a child of the current inference environement.}
                       
@section{Current Inference Environment Fields}

The most common way of accessing the current inference environment fields is using the following procedures.

@defproc*[(((current-inference-data-index (data-index hash-eq?)) void?)
           ((current-inference-data-index) hash-eq?))]{
Sets or gets the @scheme[data-index] field of the current inference environment.}

@defproc*[(((current-inference-goal-index (data-index hash-eq?)) void?)
           ((current-inference-goal-index) hash-eq?))]{
Sets or gets the @scheme[goal-index] field of the current inference environment.}

@defproc*[(((current-inference-rule-nodes (rule-nodes (listof rule-node?))) void?)
           ((current-inference-rule-nodes) (listof rule-node?)))]{
Sets or gets the @scheme[rule-nodes] field of the current inference environment.}

@defproc*[(((current-inference-exit (exit (or/c procedure? false/c))) void?)
           ((current-inference-exit) (one-of/c procedure? false/c)))]{
Sets or gets the @scheme[exit] field of the current inference environment.}

@defproc*[(((current-inference-next-assertion-id (next-assertion-id exact-positive-integer?)) void?)
           ((current-inference-next-assertion-id) exact-positive-integer?))]{
Sets or gets the @scheme[next-assertion-id] field of the current inference environment.}

@defproc*[(((current-inference-assertion-index (assertion-index hash-eq?)) void?)
           ((current-inference-assertion-index) hash-eq?))]{
Sets or gets the @scheme[assertion-index] field of the current inference environment.}

@defproc*[(((current-inference-trace (trace boolean?)) void?)
           ((current-inference-trace) boolean?))]{
Sets or gets the @scheme[trace] field of the current inference environment.}

@defproc*[(((current-inference-agenda (agenda (mlistof rule-instance?))) void?)
           ((current-inference-agenda) (mlistof rule-instance?)))]{
Sets or gets the @scheme[agenda] field of the current inference environment.}

@defproc*[(((current-inference-rule (rule (or/c rule-instance? false/c))) void?)
           ((current-inference-rule) (or/c rule-instance? false/c)))]{
Sets or gets the @scheme[rule] field of the current inference environment.}

@defproc*[(((current-inference-strategy
             (strategy (one-of/c 'depth 'breadth 'order 'simplicity 'complex 'random))) void?)
           ((current-inference-strategy)
            (one-of/c 'depth 'breadth 'order 'simplicity 'complex 'random)))]{
Sets or gets the @scheme[strategy] field of the current inference environment.}

@defproc*[(((current-inference-parent (parent (or/c inference-environment? false/c))) void?)
           ((current-inference-parent) (or/c inference-environment? false/c)))]{
Sets or gets the @scheme[parent] field of the current inference environment.}

@defproc*[(((current-inference-rules-fired (trace exact-nonnegative-integer?)) void?)
           ((current-inference-rules-fired) exact-nonnegative-integer?))]{
Sets or gets the @scheme[rules-fired] field of the current inference environment.}

