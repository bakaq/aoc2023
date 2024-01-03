:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(time)).
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(iso_ext)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(debug)).

:- use_module(utils).

clpz:monotonic.

% === Actual program ===

digit_t(Digit, T) :- 
    findall(Type, char_type(Digit, Type), CharTypes),
    if_(
        memberd_t(decimal_digit, CharTypes),
        T = true,
        T = false
    ).

digit_t(Val, T) -->
    [D],
    if_(
        digit_t(D),
        { T = true, number_canonchars(Val, [D]) },
        { T = false }
    ).

digit_alt_t(Val, T) -->
    if__(
        digit_t(Val0),
        { T = true, Val = Val0 },
        if__(
            user:digit_text_t(Val1),
            { T = true, Val = Val1 },
            { T = false }
        )
    ).

% Weird semicontext to only consume one letter
digit_text(1), "ne" --> "one".
digit_text(2), "wo" --> "two".
digit_text(3), "hree" --> "three".
digit_text(4), "our" --> "four".
digit_text(5), "ive" --> "five".
digit_text(6), "ix" --> "six".
digit_text(7), "even" --> "seven".
digit_text(8), "ight" --> "eight".
digit_text(9), "ine" --> "nine".

% TODO: Make this monotonic
digit_text_t(Val, T) -->
    (   digit_text(Val0) ->
        { T = true, Val = Val0 }
    ;   if__(
            digit_t(Val1),
            { T = true, Val = Val1 },
            { T = false }
        )
    ).

calibration_line(Part, Value) -->
    line(Line),
    {
        phrase(calibration_first(Part, First), Line),
        phrase(calibration_last(Part, Last), Line),
        #Value #= 10 * #First + #Last
    }.

calibration_first(Part, First) -->
    if_(
        Part = 1,
        { DigitNT = user:digit_t(First0) },
        { DigitNT = user:digit_text_t(First0) }
    ),
    if__(
        DigitNT,
        ({ First = First0}, ...) ,
        calibration_first(Part, First)
    ).
    
calibration_last(Part, Last) -->
    calibration_last_(Part, none, Last0),
    { Last0 = last(Last) }.

calibration_last_(Part, Last0, Last) -->
    if__(
        eos_t,
        { Last = Last0 },
        (
            % https://github.com/mthom/scryer-prolog/issues/2266
            if_(
                Part = 1,
                { DigitNT = user:digit_t(LastNum) },
                { DigitNT = user:digit_text_t(LastNum) }
            ),
            if__(
                DigitNT, 
                { Last1 = last(LastNum) },
                { Last1 = Last0 }
            ),
            calibration_last_(Part, Last1, Last)
        )
    ).

sum_calibration(Part, Sum) --> 
    if__(
        eos_t,
        { #Sum #= 0 },
        (
            calibration_line(Part, Val),
            { #Sum #= #Sum0 + #Val },
            sum_calibration(Part, Sum0)
        )
    ).

main :-
    phrase_from_file(sum_calibration(1, Sum), "01.input"),
    portray_clause(Sum).

main_alt :-
    phrase_from_file(sum_calibration(2, Sum), "01.input"),
    portray_clause(Sum).
