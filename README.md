binaryLogic
===========

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits.
(fillBits, switchEndianess, bytesNeeded, binaryPrefix).


Getting started
---------------

Starting with a simple dezimal to binary conversion and vice versa.

$ the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- dec2bin(42)

[1] 1 0 1 0 1 0

$ bin2dec(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)

[1] 42


Information
-----------

The »binaryLogic::binary« class inherits from the »base::logical« class. This brings some benefits. It is no problem to switch from logical to binary and vice versa.

e.G. 

two <- dec2bin(2); as.logical(two); two == two;

[1] TRUE TRUE

as.binary(two)

[1] TRUE

Some function from Package binaryLogic can also be used on logical vectors like shiftLeft, shiftRight, rotate (see help).
