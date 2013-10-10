#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label scheme/base
                     "../inference.ss"))

@title[#:tag "control-basic"]{Inference Control (Basic)}

@local-table-of-contents[]

The PLT Scheme Inference Collection provides functions for basic inference control.

The underlying structure of the inference engine is a rule network whose nodes store information about the state of the inference. For forward chaining, (non-goal) match nodes store the results of unifying assertions (i.e., facts) against rule precondition patterns. Goal match nodes are used to initiate backward chaining. A chain of join nodes stores the results of joining successive precondition matches for a rule. The join chain terminates with a rule node. Matches that propagate to a rule node result in rule instances that are added to the agenda. The order or rule instances on the agenda and based on priority and, for rule instances of equal priority, the current conflict resolution strategy.

The inference loop itself is very simple. It continually removes the first rule instance from the agenda and executes it using the bindings reulting from the successive matches and joins in the join node chain. This continues until either the inference loop is explicitly exited by a call to @scheme[stop-inference] or the agenda is empty.

Most of the forward chaining inference work is done in response to facts being asserted or retracted. When a fact is asserted, it is matched against the patterns (in the match nodes). Matches (and non-matches) are propagated to the join nodes. Successful matches result in a set of bindings for the pattern variables. [Note that non-matches have to be propagated to allow existential processing by the join nodes.] Interior join nodes accept matches from exactly one match node and one prior join node. The matches are pair-wise joined and successful joins (i.e., those with compatible bindings) result in a match that is, in turn, propagated to the next join or rule node.

A variable is bound by the first pattern clause in which it appears in a rule. A variable is local to the pattern in which it is bound and global to any other patterns.  Variable contraints may be checked at either the match node or its paired rule node. If a variable constraint clause involves only local variables (and constants), it is checked at the match node.  Otherwise, the check must be delayed to the paired join node. Existential pattern processing always takes place at the paired join node.

Backward chaining inference uses the same rule network as forward chaining. Backward chaining is initiated either explicitly by a call to @scheme[check], or implicitly by a forward chaining inference when it needs the value of a fact that can be inferred by backward chaining. Facts inferred by a backward chaining inference are propagated through the rule network and thereby affect the forward chaining inference. [While there are explicit forward and backward chaining rules, the inference engine itself is bidirectional and backward chaining and forward chaining as needed in the inference.]

@section{Rule Set Activation}

Rule set @deftech{activation} is the process by which a ruke network corresponding to the rules in a specified ruleset is created and associated with the current inference environment. A rule set must be activated before the rules in the rule set are available to the inference engine. Multiple rule sets may be active simultaneously.

@defproc[(activate (ruleset ruleset?)) void?]{
Activate @scheme[ruleset]. Create a rule network corresponding to @scheme[ruleset] and associate it with the current inference environment. Future assertions in the same inference environment will propogate through this rule network (along with any other rule networks from other activated rule sets). Existing assertions are @bold{not} automatically propagated through the rule network.}

@section{Forward Chaining Control}

The following procedures are defined here (instead of in Chapter 5, Assertions) because they implement most of the actual forward chaining inferencing.

@defproc[(assert (fact fact?) (reason any/c (current-inference-rule))) assertion?]{
Asserts @scheme[fact] in the current inference environment for the given @scheme[reason]. This will create the assertion and propogate it through the rule network. Note that if the fact is already asserted, the reason is updated, but the fact is not reasserted. The assertion instance is returned.}

@defproc[(retract (assertion assertion?)) void?]{
Retracts the @scheme[assertion] in the current inference environment and propogates the retraction through the rule network.}

@defproc[(replace (assertion assertion?) (fact fact?) (reason any/c (current-inference-rule))) assertion?]{
Retracts the @scheme[assertion] in the current inference environment and asserts @scheme[fact] in the current inference environment for the given @scheme[reason], reusing the assertion object.  It is an error for @scheme[fact] to be a different kind of fact than the fact in @scheme[assertion]. [In fact, it is intended to represent a change to that fact, as opposed to a brand new fact.] This is essentially equivalent to @scheme[(retract assertion)] followed by @scheme[(assert fact [reason])].}

@defproc[(query (pattern pattern?)) (listof assertion?)]{
Returns a list of all of the assertions that match @scheme[pattern]. This can be used to retrieve inference data.}

@section{Backward Chaining Control}

Backward chaining is initiated by a call to @scheme[check].

@defproc[(check (fact fact?)) (mlistof match?)]{
Initiates a backward chaining (goal-driven) inference to determine whether or not the @scheme[fact] is true.  A (mutable) list of matches for @scheme[fact].}

The @scheme[check] function is also called internally to initiate backward chaining when the inference engine needs to get the truth value of a fact that has not been asserted and there are backward chaining rule to (potentially) determine it.

@section{Inference Control}

@defproc[(start-simulation) any]{
Implements the forward chaining (data-driven) inference loop.  It asserts the fact @scheme[(start)] and then continually executes rules from the agenda until there are no more rule instances to fire or if the loop is explicitly exited.  Note that the forward chaining (data-driven) inference loop will invoke backward chaining (goal-driven) inferencing as needed.}

@defproc*[(((stop-inference (return-value any/c)) any)
           ((stop-inference) any))]{
Exits the current inference loop, optionally returning @scheme[return-value]}
                                   
By convention, a return value of false, @scheme[#f], means that the inference failed or terminated because there were no more rule instances to execute.  The following function provide explicit function to terminate the inference loop and to indicate success or failure.

@defproc[(succeed) true/c]{
Exits the current inference loop and returns true, @scheme[#t]}

@defproc[(fail) false/c]{
Exits the current inference loop and returns false, @scheme[#f]}
