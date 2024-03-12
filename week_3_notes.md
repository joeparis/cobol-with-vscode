## Conditional Expressions

Three common types of COBOL conditional expressions are:

* general relation conditions
* class conditions
* sign conditions

`IS <test or condition>`

### Conditional Names

Used to say, the input we are taking needs to pass this test.

Conditional names are declared in the working storage section with an 88 level number. It must be level 88.


```cobol
01 USA-STATE        PIC X(2) VALUE SPACES.
   88 STATE         VALUE 'TX'.
...
...
PROCEDURE DIVISION.
...
...
MOVE 'AZ' TO STATE.
...
...
IF STATE DISPLAY 'The State is Texas!'
  ELSE DISPLAY 'The State is not Texas'
END-IF.
...
....
MOVE 'TX' TO STATE.
...
...
IF STATE DISPLAY 'The State is Texas!'
  ELSE DISPLAY 'The State is not Texas'
END-IF.
```

The second line of code specifies a conditional name, `STATE`, that must equal `TX`. When we `MOVE 'AZ' TO STATE.` and then later test `IF STATE` it will evaluate to False as `STATE` must equal `TX`. The next test will evaluate to True as we have set `STATE` to `TX`.

More useful if we were to put all 50 states in `STATE` to prevent entry of a non-state.

## Conditional Operators

1. **Equal to**: The `=` operator is used to test if two values are equal.
   Example: `IF A = B THEN ...`

2. **Not equal to**: The `NOT =` or `<>` operator is used to test if two values are not equal.
   Example: `IF A NOT = B THEN ...` or `IF A <> B THEN ...`

3. **Less than**: The `<` operator is used to test if one value is less than another.
   Example: `IF A < B THEN ...`

4. **Greater than**: The `>` operator is used to test if one value is greater than another.
   Example: `IF A > B THEN ...`

5. **Less than or equal to**: The `<=` operator is used to test if one value is less than or equal to another.
   Example: `IF A <= B THEN ...`

6. **Greater than or equal to**: The `>=` operator is used to test if one value is greater than or equal to another.
   Example: `IF A >= B THEN ...`

These conditional operators are commonly used in COBOL's `IF` statements, `EVALUATE` statements, and other control structures to perform comparisons and make decisions based on the results. Additionally, COBOL provides a range of logical operators (e.g., `AND`, `OR`, `NOT`) that can be used to combine multiple conditional expressions within a single statement.

```cobol
IF FACIAL-EXP = 'HAPPY' THEN
    DISPLAY 'I am glad you are happy'
ELSE DISPLAY 'What can I do to make you happy'
END-IF.
```

If there are more than two paths and `EVALUATE` statement may be more practical.

```cobol
EVALUATE FACIAL-EXP
 WHEN 'HAPPY'
  DISPLAY "I am glad you are happy'
 WHEN 'SAD'
  DISPLAY 'What can I do to make you happy'
 WHEN 'PERPLEXED'
  DISPLAY 'Can you tell me what you are confused about'
 WHEN 'EMOTIONLESS'
  DISPAY 'Do you approve or disapprove'
END-EVALUATE.
```

Kind of like a `switch`.

Use `SEARCH` when iterating over tables trying to find a specific entry.

``` SEARCH FACIAL-EXP
WHEN 'HAPPY' STOP RUN
END-SEARCH.
```

We can:

* test if this number is equal to that number
* content of a data item is numeric 
*   ... or is not alphabetic lower
*     ... or is kanji
* a number is positive, negative, or zero
* and more!

lab 6

* [Conditional expressions in z/OS 4.1](https://www.ibm.com/docs/en/mufz/4.1?topic=reference-conditional-expressions)
* [Conditional expressions in z/OZ 5.1](https://www.ibm.com/docs/en/mufz/5.1?topic=reference-conditional-expressions)

## Arithmetic Expressions

> ...an arithmetic expression can be built up with numbers, variables that represent numbers, the figurative constant of zero, and any combination of those enclosed within parenthesis, including any combination of all of that that I just mentioned, having mathematical operations performed on them.

```cobol
(((NUMBER-OF-PEOPLE - NUMBER-OF-STAFF) * 3) / 8)
```

is how many pizzas you need, in order to feed a crowd of this many people assuming we're not feeding the staff, each person will want three slices and there are eight slices per pizza. 

The `COMPUTE` command can make code more readable by combining a series of arithmetic operations in one place.

```cobol
COMPUTE Z = A + B / C**D - E
```

```cobol
COMPUTE X Y Z = A + B / C**D - E
```

The above will assign the result to all three variables.

### Limitations

* exponents in fixed-point expressions cannot be more than nine digits
* while the data descriptions of operands in an expression do not need to be the same (you can multiply a 2-digit number by a 4-digit number, for example) the maximum size of each operand is either 18 or 19 decimal digits depending on the compiler options used
    * if you are using either very big numbers or a very high degree of precision make sure you know if you are using compatibility or extended mode for arithmetic

<hr>

### ChatGPT says:

In COBOL, compatibility mode and extended mode refer to different options for arithmetic operations and handling of arithmetic exceptions.

1. **Compatibility Mode**:
   - In compatibility mode, arithmetic operations follow the rules defined by earlier versions of COBOL standards, such as COBOL-85.
   - Under compatibility mode, arithmetic operations behave in a manner consistent with older COBOL implementations, which may have specific rules for handling arithmetic overflows, underflows, and truncation.
   - Compatibility mode is suitable for maintaining compatibility with existing COBOL programs that rely on the behavior of earlier COBOL standards.
   - To specify compatibility mode, you may use compiler directives or options specific to your COBOL compiler.

2. **Extended Mode**:
   - Extended mode provides enhanced features and flexibility for arithmetic operations, often aligning with newer COBOL standards or compiler extensions.
   - Under extended mode, arithmetic operations may behave differently compared to compatibility mode, offering improved precision, handling of large numbers, and more robust error detection and handling.
   - Extended mode may support features such as IEEE floating-point arithmetic, which provides standardized behavior for floating-point operations.
   - Using extended mode can lead to more consistent behavior across different platforms and compilers, as it aligns with modern standards and practices.
   - Like compatibility mode, enabling extended mode typically involves compiler-specific directives or options.

When choosing between compatibility mode and extended mode for arithmetic operations in COBOL, consider factors such as the requirements of your application, compatibility with existing codebases, and adherence to modern standards and best practices. If your application relies heavily on existing COBOL code and behavior, compatibility mode may be preferable. However, if you require enhanced arithmetic capabilities and improved compatibility with modern environments, extended mode may be more suitable. Additionally, consult the documentation of your COBOL compiler for specific guidance on enabling and using compatibility and extended modes.

<hr>

```cobol
01 TLIMIT-TBALANCE.
   05 TLIMIT                PIC S9(9)V99 COMP-3 VALUE ZERO.
   05 TBALANCE              PIC S9(9)V99 COMP-3 VALUE ZERO.
```

<hr>

### ChatGPT says:

In COBOL, `PIC S9(9)V99 COMP-3 VALUE ZERO` is a picture clause that defines a numeric data item. Let's break down each component:

1. **PIC**: PIC stands for "Picture" and is used to specify the format of data items in COBOL.

2. **S9(9)V99**: This part of the picture clause defines the numeric data item. Here's what each part means:
   - `S`: Indicates that the data item is signed, meaning it can represent positive or negative numbers. In this case, the sign is represented by a separate digit (the rightmost digit).
   - `9(9)V99`: Specifies the numeric format. Here:
     - `9(9)`: Indicates that the data item can hold up to nine digits for the integral part of the number.
     - `V99`: Specifies that the data item has a decimal point and can hold up to two digits for the fractional part of the number.

3. **COMP-3**: COMP-3 (also known as packed-decimal or packed-decimal binary) is a storage format for numeric data in COBOL. It represents each decimal digit using a nibble (half a byte) and stores the sign separately. COMP-3 is typically used for numeric fields that require efficient storage and processing.

4. **VALUE ZERO**: This clause initializes the data item with the value zero. When the program executes, the data item defined by this picture clause will be automatically initialized to zero.

Putting it all together, `PIC S9(9)V99 COMP-3 VALUE ZERO` defines a signed numeric data item that can hold up to nine digits with two decimal places, stored in packed-decimal format. The data item is initialized to zero when the program starts executing.

<hr>

lab 7

## Data Types

### Numeric Sata

Choose the best representation of numerical data in memory for efficiency.

#### COMP-1

Single-precision floating-point number representation.

For numbers that are close to zero or very large $10^{38}$.

Limited to seven (7) significant decimal digits.

#### COMP-2

Double-precision floating-point representation.

Zero to $10^{307}$.

15 significant digits.

#### COMP-3

Binary coded decimal (BCD).

Most common numeric representation in COBOL.

Somewhat unique and native to mainframe computers and the z-architecture. 

Good handling of fractional values.

Eliminates rounding errors by exactly representing values with decimal places and goes up to 31 decimal digits.

<hr>

##### Copilot says:

Binary Coded Decimal (BCD) is a class of binary encodings of decimal numbers where each decimal digit is represented by a fixed number of bits, usually four or eight.

In a four-bit encoding, decimal numbers 0 to 9 are represented by their binary equivalents ranging from 0000 to 1001. Decimal numbers 10 to 15 are invalid in BCD and are not used.

Here's a simple example:

The decimal number 1957 in BCD would be represented as follows:

1 -> 0001 (in binary) 9 -> 1001 (in binary) 5 -> 0101 (in binary) 7 -> 0111 (in binary)

So, 1957 in BCD is 0001 1001 0101 0111.

BCD is used in systems where numerical data is entered and displayed in human-readable format, as it simplifies the process of conversion between binary and decimal. It's also used in financial calculations to maintain precision and prevent rounding errors, as it can exactly represent decimal fractions.

In the context of COBOL and mainframe computers, BCD is often used in the COMP-3 data type. This is a packed decimal format where each decimal digit is represented by four bits, and the sign is represented by the lower four bits of the rightmost byte, making it very space-efficient.

<hr>

#### COMP-4

Integers only.

<hr>

##### Copilot says:

A COMP-4 field defined as `PIC 9(4)` would be stored in two bytes, because mainframes can store up to 4 decimal digits in a single byte. On other systems, it might be stored in a different way.

COMP-4 fields can store both positive and negative numbers. The sign is stored in the leftmost bit of the field (also known as the high-order bit).

<hr>

#### COMP-5

The same as COMP-4, but with it you can specify the position of a decimal point. It's got space efficiency of COMP-4 with the exactness of COMP-3 the drawback there is that cannot exceed 18 decimal digits.

<hr>

##### Copilot says:

In general, COMP-5 is more portable than COMP-4, because it behaves the same way on all systems. However, it may be less efficient than COMP-4 on systems that can store data in a more compact format.

<hr>

### Text

Extended Binary Coded Decimal Interchange Code (EBCDIC) and ASCII.

EBCDIC has eight bits per character, ASCII uses seven.

And the characters are distributed across the possible combinations of those bit
differently.


Today the only place you're likely to see EBCDIC code is if you're on an IBM Mainframe, most everywhere else uses ASCII or Unicode. However, just keep this in mind if you're working on programs written a long time ago, because every once in a while, you'll go to load some data and it will show up looking all weird. Your first instinct should be to say, is it possible that this data is EBCDIC and I'm trying to load it in ASCII or vice versa? because that happens quite often.

Lab 8 (data encoding error)

**What happens when an internal packed decimal field is not described using COMP-3?** Without using COMP-3 to describe the field, the COBOL program treats the data as DISPLAY data (EBCDIC format). This lab demonstrates what happens during program execution without using COMP-3.

```bash
touch CBL{0001..0010}.cbl
```

is equivalent to:

```powershell
1..10 | ForEach-Object { New-Item -ItemType File -Path ("CBL" + ("{0:D4}" -f $_) + ".cbl") }
```

* `1..10` generates a sequence of numbers from 1 to 10.
* `ForEach-Object` is a loop that executes the following script block for each number in the sequence.
* `New-Item -ItemType File -Path ("CBL" + ("{0:D4}" -f $_) + ".cbl")` creates a new file with the specified name. The name is constructed by concatenating "CBL", the current number formatted as a four-digit number `("{0:D4}" -f $_)`, and ".cbl". The `-f` operator formats the number as a string, and `{0:D4}` specifies that it should be four digits long, padding with zeros if necessary.

1.	Submit the job, id.JCL(CBL0010J)

2.	Observe that the compile of the COBOL source was successful, however, also observe that the execution of the job failed.  How can you tell?  

    There's no CC code next to CBL0010J(JOB#), instead there is an ABENDU4038 message.  U4038 is a common user code error typically involving a mismatch between the external data and the COBOL representation of the data.

3.	Read the execution SYSOUT message carefully.  The SYSOUT message mistakenly believes the records are 174 characters in length while the program believes the records are 170 characters in length.

    Packed decimal (COMP-3) expands into two numbers where only one number would typically exist.  If the program reads a packed decimal field without describing the field as COMP-3, then program execution becomes confused about the size of the record because the PIC clause, S9(7)V99, is expecting to store seven numbers plus a sign digit when only three word positions are read.  Therefore, execution reports a four-record length position discrepancy. 

4.	Edit id.CBL(CBL0010) to identify and correct the source code problem.*

5.	Submit id.JCL(CBL0010J) and verify correction is successful with a CC 0000 code.

*Lab Hints*

The ACCT-LIMIT PIC clause in the ACCT-FIELDS paragraph should be the same as the PIC clause for ACCT-BALANCE.

## Intrinsic Functions

The most recent release of enterprise COBOL for z/OS includes 70 intrinsic functions.

Categories:

* math
* statistics
* date/time
* financial
* character-handling
* general

General form: `FUNCTION` functionName `(`args`)`

```cobol
01    Item-1   Pic x(30)   Value   "Hello world!".
01    Item-2   Pic x(30).
...
      Display Item-1
      Display Function Upper-case(Item-1)
      Display Function Lower-case(Item-1)
      Move Function Upper-case(Item-1) to Item-2
      Display Item-2
```

"1st street" with lowercase st would be a different entry than "1ST" street with an uppercase ST, which happens quite often. If we just uppercase everything, we can be sure that all the data coming in is case insensitive.

```cobol
Compute x - Function Sum(a b (c . d))
```

```cobol
01    YYYMMDD        Pic 9(8).
01    Integer-Form  Pic S9(9).
...
      Move Function Current-Date(1:8) to YYYYMMDD
      Compute Integer-Form = Function Integer-of-Date(YYYYMMDD)
      Add 90 to Integer-Form
      Compute YYYYMMDD = Function Date-of-Integer(Integer-Form)
      Display 'Due Date: ' YYYYMMDD
```

COBOL epoch: January 1st 1601; `Integer-of-Data` returns the number of days since the epoch.