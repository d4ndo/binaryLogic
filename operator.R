Operator <- c("[== or !=]", "[&lt;, &lt;= or > , >=]", "[+ or -]", "[*, ^]", "[/]", "[&, &#166;, xor]", "[!]")
Behavior <- c("Comparision by value.", "Comparision by value.", "Operations by value.", "Operations by value.", " Not supported.", "Bitwise Operations. The smaller vector is filled up  with zeros.", "Indicates logical negation (NOT). Bitwise Operations")
op <- as.data.frame(cbind(Operator,Behavior))

Operator <- c("shiftLeft(binary), shiftRight(binary)", "rotate(binary)", "negate(binary)", "switchEndianess(binary)")
Behavior <- c("shift Operation.", "shift Operation.", "Indicates arithmetic negation. value <- value * (-1)", " ")
op2 <- as.data.frame(cbind(Operator,Behavior))