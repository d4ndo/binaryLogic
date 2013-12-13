binaryLogic
===========

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits.
(fillBits, switchEndianess, bytesNeeded, binaryPrefix).


Getting started
---------------

Starting with a simple conversion. »decimal to binary« and vice versa.

$ the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- dec2bin(42)

[1] 1 0 1 0 1 0

$ bin2dec(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)

[1] 42


Information
-----------

The »binaryLogic::binary« class inherits from the »base::logical« class. This binary class is just not that great at heavy number crunching, but it brings some benefits. Especially if you like to work using vectors in R. It is no problem to switch from logical to binary and vice versa. Some function from Package binaryLogic can also be used on logical vectors like shiftLeft, shiftRight, rotate (see help).

Some operators have a different behavior.
e.g. 

two <- dec2bin(2); two <- as.logical(two); two == two;

[1] TRUE TRUE

The logical == operator compares every element of the vector (Bitwise comparison).

two <- as.binary(two); two == two;

[1] TRUE

The binary == operator compares the value. Even different endianess.

two_B <- as.binary(two, littleEndian=FALSE); two_L <- as.binary(two, littleEndian=TRUE);  two_B == two_L;

[1] TRUE


