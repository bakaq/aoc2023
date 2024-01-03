:- module(utils, [
    number_canonchars/2,
    eos//0,
    rest_of_string//1,
    eos_t//1,
    if_//3,
    if__//3,
    line//1,
    lines//1
]).

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(reif)).
:- use_module(library(lists)).
:- use_module(library(lambda)).

% This is a monotonic version of number_chars/2 that uses the canonical
% representation you get if the number argument of number_chars/2 is
% instantiated.
number_canonchars(N, Chars) :-
    number_chars(N, Chars),
    number_chars(N, CanonChars),
    Chars = CanonChars.

eos --> call(eos_raw).
eos_raw([], []).

neos --> call(Rest^Rest^dif(Rest, [])).
rest_of_string(String) --> call(String^[]^true).

eos_t(T) -->
    call(eos_t_raw(T)).

eos_t_raw(T, Rest, Rest) :-
    if_(Rest = [], T = true, T = false).

:- meta_predicate(if_(1,2,2,?,?)).

% This lifts if_/3 to a non-terminal.
% The condition is a reified predicate.
if_(If, Then, Else) -->
    call(if_raw(If, Then, Else)).

if_raw(If, Then, Else, Ls0, Ls) :-
    if_(
        If,
        phrase(Then, Ls0, Ls),
        phrase(Else, Ls0, Ls)
    ).

:- meta_predicate(if__(3,2,2,?,?)).

% This lifts if_/3 to a non-terminal.
% The condition is a reified non-terminal.
if__(If__1, Then, Else) -->
    call(if__raw(If__1, Then, Else)).

if__raw(If__1, Then, Else, Ls0, Ls) :-
    if_(
        phrase_t(If__1, Ls0, Ls1),
        phrase(Then, Ls1, Ls),
        phrase(Else, Ls1, Ls)
    ).

phrase_t(NT__1, Ls0, Ls, T) :-
    phrase(call(call(NT__1, T)), Ls0, Ls).

lines(Lines) -->
    if__(
        eos_t,
        { Lines = [] },
        (
            { Lines = [Line|Lines1] },
            line(Line),
            lines(Lines1)
        )
    ).

line(Line) -->
    if__(
        eos_t,
        { Line = [] },
        (
            [C],
            if_(
                C = '\n',
                { Line = [] },
                (
                    { Line = [C|Cs] },
                    line(Cs)
                )
            )
        )
    ).
