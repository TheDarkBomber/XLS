
# Table of Contents

1.  [XLS – The X Low-Level Language for Systems Developement](#org10d60d2)
2.  [Syntax overview](#org98d9081)
    1.  [Numerical literals](#orgab1c98e)
    2.  [Comments](#orga8185f1)



<a id="org10d60d2"></a>

# XLS – The X Low-Level Language for Systems Developement

XLS is a procedure-oriented low-level programming language that compiles to LLVM bytecode.
As a result of compilation to LLVM bytecode, XLS inherits the portability features of LLVM.


<a id="org98d9081"></a>

# Syntax overview


<a id="orgab1c98e"></a>

## Numerical literals

In XLS, numerical literals are simple. `534` refers to the number literal 534 in base 10.
XLS also offers the ability to express number literals in other radices. For example,
`0d534` refers to the number literal 534 in base 10. :p

`0xFF` refers to the number literal FF in base 16, which is equal to 255 in base 10.
XLS also supports the following radices:
```xls
    0b11; // Binary, base 2
    0q33; // Quaternary, base 4
    0s55; // Heximal, base 6
    0d99; // Decimal, base 10
    0xFF; // Hexadecimal, base 16
    0nZZ; // Niftimal, base 36.

    // Numerical literals are case insensitive
    0xFF == 0xff;
    0nZZ = 0nzz;
```

<a id="orga8185f1"></a>

## Comments

Comments are performed using the standard C/C++ syntax.
```xls
    // This is a comment.
    // And so is this.
    extern coldcc clygro(argument1, argument2); // This function is defined in clygro.xls
```

