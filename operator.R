Operator <- c("[== or !=]", "[&lt;, &lt;= or > , >=]", "[+ or -]", "[*, ^]", "[%/%, %%]","[/]", "[&, &#166;, xor]", "[!]")
Behavior <- c("Comparision by value.", "Comparision by value.", "Operations by value.", "Operations by value.", "Operations by value", " Not supported.", "Bitwise Operations. The smaller vector is filled up  with zeros.", "Indicates logical negation (NOT). Bitwise Operations")
op <- as.data.frame(cbind(Operator,Behavior))

Operator <- c("shiftLeft(binary), shiftRight(binary)", "rotate(binary)", "negate(binary)", "switchEndianess(binary)", "bin2gray(binary)", "gray2bin(gray)")
Behavior <- c("shift Operation.", "shift Operation.", "Indicates arithmetic negation. value <- value * (-1)", " ", "convert binary to gray code", "convert gray code to binary")
op2 <- as.data.frame(cbind(Operator,Behavior))