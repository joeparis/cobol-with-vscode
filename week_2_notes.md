## Data Division

All the data that will be used in the program gets set up in the DATA DIVISION.

```cobol
*-------------
DATA DIVISION.
*-------------
FILE SECTION.
FD  PRINT-LINE RECORDING MODE F.
*FD -- describes the layout of PRINT-LINE file,
*including level numbers, variable names, data types and lengths
*
01  PRINT-REC.
    05  ACCT-NO-O      PIC X(8).
    05  FILLER         PIC X(02) VALUE SPACES.
*    FILLER -- COBOL reserved word used as data name to remove
*    the need of variable names only for inserting spaces
*
    05  LAST-NAME-O    PIC X(20).
    05  FILLER         PIC X(02) VALUE SPACES.
*    SPACES -- used for structured spacing data outputs rather
*    than using a higher PIC Clause length as in CBL0001.cobol,
*    which makes a good design practice and a legible output
*
    05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
*    The repeated $ characters revert to spaces and then one $
*    in front of the printed amount.
*
    05  FILLER         PIC X(02) VALUE SPACES.
    05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
    05  FILLER         PIC X(02) VALUE SPACES.
FD  ACCT-REC RECORDING MODE F.
01  ACCT-FIELDS.
    05  ACCT-NO            PIC X(8).
    05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
    05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
    05  LAST-NAME          PIC X(20).
    05  FIRST-NAME         PIC X(15).
    05  CLIENT-ADDR.
        10  STREET-ADDR    PIC X(25).
        10  CITY-COUNTY    PIC X(20).
        10  USA-STATE      PIC X(15).
    05  RESERVED           PIC X(7).
    05  COMMENTS           PIC X(50).
```

> We can see that by looking at this segment of code right here, in this data division we've got level indicators, each with their own descriptive entry. `PRINT-REC` has a level indicator of `01`, meaning it's the highest, and then the data definitions are below it at `05`. The file `ACCT-REC` has its own set of variables with their own level indicators down here. Don't you just love that structure? It's basically saying that data this file needs are the account fields. Then below it, it spells out exactly what those account fields are. 

In the code below we see some level-77 variables, `WHO`, `WHERE`, `WHY`, etc. declared in the DATA DIVISION. In the PROCEDURE DIVISION we set the values of those variables, then compute and set the gross pay. Next, we display the values along with some descriptive text.

`MOVE` and `COMPUTE` are reserved words used to alter the value of variables.

```cobol
*-------------
DATA DIVISION.
*-------------
WORKING-STORAGE SECTION.
****** Variables for the report
* level number
* |  variable name
* |  |          picture clause
* |  |          |
* V  V          V
77  WHO        PIC X(15).
77  WHERE      PIC X(20).
77  WHY        PIC X(30).
77  RATE       PIC 9(3).
77  HOURS      PIC 9(3).
77  GROSS-PAY  PIC 9(5).

* PIC X(15) -- fiftheen alphanumeric characters
* PIC 9(3)  -- three-digit value
*------------------
PROCEDURE DIVISION.
*------------------
****** COBOL MOVE statements - Literal Text to Variables
    MOVE  "Captain COBOL" TO WHO.
    MOVE "San Jose, California" TO WHERE.
    MOVE "Learn to be a COBOL expert" TO WHY.
    MOVE 19 TO HOURS.
    MOVE 23 TO RATE.
* The string "Captain COBOL" only contains 13 characters,
* the remaining positions of variable WHO are filled with spaces
* The value 19 only needs 2 digits,
* the leftmost position of variable HOURS is filled with zero
****** Calculation using COMPUTE reserved word verb
    COMPUTE GROSS-PAY = HOURS * RATE.
* The result of the multiplication only needs 3 digits,
* the remaining leftmost positions are filled with zeroes
****** DISPLAY statements
    DISPLAY "Name: " WHO.
    DISPLAY "Location: " WHERE
    DISPLAY "Reason: " WHY
    DISPLAY "Hours Worked: " HOURS.
    DISPLAY "Hourly Rate: " RATE.
    DISPLAY "Gross Pay: " GROSS-PAY.
    DISPLAY WHY " from " WHO.
    GOBACK.
```

## File Handling in COBOL

`SELECT` and `ASSIGN` clauses in the `ENVIRONMENT` division, `FD` statements in the `DATA` division, and from the `PROCEEDURE` division, `OPEN`, `CLOSE`, `READ INTO`, and `WRITE FROM` statements.

```cobol
*--------------------
ENVIRONMENT DIVISION.
*--------------------
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT PRINT-LINE ASSIGN TO PRTLINE.
    SELECT ACCT-REC   ASSIGN TO ACCTREC.
```

The above shows a file control paragraph that associates internal filenames with external dataset names.

`PRINT-LINE` is an internal file name, `PRTLINE` is the external dataset or file.

```cobol
*-------------
DATA DIVISION.
*-------------
FILE SECTION.
FD  PRINT-LINE RECORDING MODE F.
*FD -- describes the layout of PRINT-LINE file,
*including level numbers, variable names, data types and lengths
*
01  PRINT-REC.
    05  ACCT-NO-O      PIC X(8).
    05  FILLER         PIC X(02) VALUE SPACES.
*    FILLER -- COBOL reserved word used as data name to remove
*    the need of variable names only for inserting spaces
*
    05  LAST-NAME-O    PIC X(20).
    05  FILLER         PIC X(02) VALUE SPACES.
*    SPACES -- used for structured spacing data outputs rather
*    than using a higher PIC Clause length as in CBL0001.cobol,
*    which makes a good design practice and a legible output
*
    05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
*    The repeated $ characters revert to spaces and then one $
*    in front of the printed amount.
*
    05  FILLER         PIC X(02) VALUE SPACES.
    05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
    05  FILLER         PIC X(02) VALUE SPACES.
FD  ACCT-REC RECORDING MODE F.
01  ACCT-FIELDS.
    05  ACCT-NO            PIC X(8).
    05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
    05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
    05  LAST-NAME          PIC X(20).
    05  FIRST-NAME         PIC X(15).
    05  CLIENT-ADDR.
        10  STREET-ADDR    PIC X(25).
        10  CITY-COUNTY    PIC X(20).
        10  USA-STATE      PIC X(15).
    05  RESERVED           PIC X(7).
    05  COMMENTS           PIC X(50).
```

The `DATA DIVISION` is where we define level numbers, variable names, types, and lengths.

The image above shows that `ACCT-REC` has `ACCT-NO`, `ACCT_LIMIT`, `ACCT_BALANCE` fields, among others.

Code running on an IBM Z mainframe will have access to both z/OS datasets as well as any Unix files that reside on the system.

A *dataset* is made up of *records* of a pre-defined length. Like a DB table.

A *block* is a group of records and make for efficient reading of datasets. Think cacheline.

We can load a block of records into a *buffer*. 

We set the overall block size in the buffer with the `BLOCK CONTAINS` clause.

Earlier, we use the assign clause to describe a dataset source file. We've said `SELECT ACCOUNT-REC ASSIGN` (with a dash), `ASSIGN TO ACCTREC` (without a dash).

<hr>

### ChatGPT says:

Yes, that's a common convention, but it's not a strict rule. Here's a breakdown of the typical usage of levels in COBOL:

1. **Level 01**: Used to define the highest-level group or record in the data hierarchy. It represents the entire record or a major group of related fields.

2. **Level 02 to Level 49**: Used to define subordinate groups or segments within the overall record structure. These levels are often used for logical grouping of fields, representing subsets of data within the larger structure defined at Level 01.

3. **Level 88**: Used for condition names or symbolic names representing specific values within fields. These are not used to define data structures but rather to provide meaningful names for conditions.

4. **Level 66**: Used for redefines clauses, which allow multiple data items to share the same memory storage. This is typically used for data items with overlapping or alternative representations.

5. **Level 77**: Used for standalone data items or constants. These are typically used for individual fields or simple data items.

6. **Level 78 to Level 99**: These levels are not often used in modern COBOL programming and are considered reserved for future use.

In practice, levels 02 to 49 are often used for subgrouping of fields within the overall record structure, while levels 77 and higher are used for individual fields or simple data items. However, there is some flexibility in how you structure your data hierarchy, and the choice of level numbers ultimately depends on the specific requirements and design of your program. The most important thing is to maintain clarity, consistency, and logical organization within your data hierarchy.

<hr>

```cobol
SELECT ACCOUNT-REC ASSIGN TO ACCTREC
```

If we're running this in z/OS using JCL, we need to have a JCL `DD` (**D**ata **D**eclaration) statement to link `ACCOUNT-REC` to that actual dataset.

The JCL required for the COBOL compiler to make the connection back to the actual dataset looks something like this:

```jcl
//ACTREC DD DSN=MY.DATA,DISP=SHR
```

This says that for `ACCTREC` look in `MY.DATA`. The `DISP=SHR` means that it expects that dataset to exist before we try to use it and that other people can access it at the same time.

> So we'll start out with `ACCOUNT-REC`. That's what we reference within our procedure division in the actual code (our local name for the data). `ACCOUNT-REC` is what's defined in the environment division's input/output section. `//ACCTREC`, seven characters, then gets connected via a `DD` statement to `MY.DATA`, the actual dataset on the disk. 

The COBOL compiler assumes that the filenames you provide are correct. It has no way of checking what's actually in your JCL or actually on the file system. If any part of that chain is incorrect you will get a runtime error. 

This is why it's important to have a good naming scheme: chasing those types of errors is no fun at all.

A JCL primer: [What is JCL?](https://www.ibm.com/docs/en/zos-basic-skills?topic=sdsf-what-is-jcl)

Also, [What is TSO?](https://www.ibm.com/docs/en/zos-basic-skills?topic=interfaces-what-is-tso) and [What is ISPF?](https://www.ibm.com/docs/en/zos-basic-skills?topic=interfaces-what-is-ispf)



## Procedure Division

```cobol
*------------------
PROCEDURE DIVISION.
*------------------
OPEN-FILES.
    OPEN INPUT  ACCT-REC.
    OPEN OUTPUT PRINT-LINE.
OPEN-FILES-END.
*OPEN-FILES-END -- consists of an empty paragraph suffixed by
*-END that ends the past one and serves as a visual delimiter
*
WRITE-HEADERS.
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
    MOVE WS-CURRENT-YEAR  TO HDR-YR.
    MOVE WS-CURRENT-MONTH TO HDR-MO.
    MOVE WS-CURRENT-DAY   TO HDR-DAY.
    WRITE PRINT-REC FROM HEADER-1.
    WRITE PRINT-REC FROM HEADER-2.
    MOVE SPACES TO PRINT-REC.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
    WRITE PRINT-REC FROM HEADER-3.
    WRITE PRINT-REC FROM HEADER-4.
    MOVE SPACES TO PRINT-REC.
```

Here we open `ACCT-REC` for input and `PRINT-LINE` for output.

Good citizens close their files when finished.

<hr>

### ChatGPT says:

When you MOVE `FUNCTION CURRENT-DATE` TO `WS-CURRENT-DATE-DATA`, COBOL automatically populates the fields within `WS-CURRENT-DATE-DATA` with the corresponding components of the current date and time. COBOL takes care of allocating the appropriate space for each component based on the definitions provided in the `WORKING-STORAGE SECTION`.

In COBOL, the behavior of the `WRITE` statement regarding output positioning depends on how it's configured within the program. By default, the `WRITE` statement advances the output to a new line before writing, but you can control the positioning using the `BEFORE` or `AFTER` phrases.

Here's a brief explanation:

- **Default Behavior**: If you don't specify `BEFORE` or `AFTER`, the `WRITE` statement advances the output to a new line before writing the data item specified in the `WRITE` statement.

- **BEFORE**: If you specify `BEFORE`, the `WRITE` statement writes the data item without advancing to a new line. It writes the data immediately before the current output position.

- **AFTER**: If you specify `AFTER`, the `WRITE` statement writes the data item without advancing to a new line. It writes the data immediately after the current output position.

So, if you want the `WRITE` statement to start output on a new line, you can either rely on the default behavior (which advances to a new line) or explicitly use the `AFTER` phrase to ensure it starts on a new line.

Here's an example of a `WRITE` statement that ensures output begins on a new line:

```cobol
WRITE PRINT-REC AFTER ADVANCING 1 LINE
```

In this example, the `AFTER ADVANCING 1 LINE` phrase ensures that the output starts on a new line before writing `PRINT-REC`.

<hr>

```cobol
READ-NEXT-RECORD.
    PERFORM READ-RECORD
*    PERFORM -- in this case transfers control to another
*    paragraph of the code, executes it and returns control to
*    the following line.
*
    PERFORM UNTIL LASTREC = 'Y'
*     here PERFORM allows a loops to be entered
*
        PERFORM WRITE-RECORD
        PERFORM READ-RECORD
    END-PERFORM
    .
*
CLOSE-STOP.
    CLOSE ACCT-REC.
    CLOSE PRINT-LINE.
    GOBACK.
*
READ-RECORD.
    READ ACCT-REC
        AT END MOVE 'Y' TO LASTREC
    END-READ.
*
WRITE-RECORD.
    MOVE ACCT-NO      TO  ACCT-NO-O.
    MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
    MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
    MOVE LAST-NAME    TO  LAST-NAME-O.
    WRITE PRINT-REC.
```

`PERFORM` is a looping structure.

`PERFORM` is also how we call a function?

1. Read a record to prime the pump
1. Loop until `LASTREC = 'Y'`
1. Write out the record that was read
1. Read the next record

The period at the end of the `READ-NEXT-RECORD` paragraph is an implicit end of the paragraph and is necessary if the fil we are reading from is empty and thus we immediatly exit.

Lab 4.

## COBOL Program Structure

COBOL is a structured programming language.

COBOL programs need to be transparent, readable, and easily diagnosed. While there is some OOP in COBOL it's recommended to keep toi structured programming concepts.

As we saw, COBOL passes control with `PERFORM` and paragraphs. How can we best make use of this structure? Consider the following code snippet:

```cobol
OPEN OUTPUT PRINT-LINE.

MOVE 'THE NUMBER IS: ' TO MSG-HEADER OF PRINT-REC.

ADD 1 TO COUNTER GIVING COUNTER.
MOVE COUNTER TO MSG-TO-WRITE.
WRITE PRINT-REC.

ADD 1 TO COUNTER GIVING COUNTER.
MOVE COUNTER TO MSG-TO-WRITE.
WRITE PRINT-REC.

...

CLOSE PRINT-LINE.
STOP RUN.
```

We are setting up the output of `PRINT-LINE` by setting the message header of `PRINT-REC` to "THE NUMBER IS: ", incrementing `COUNTER`, appending `COUNTER` to `MSG-TO-WRITE`, and then writing `PRINT-REC`.

Then we repeat those steps.

The only clue we have as to how many times to do this is that the name of the program is `TOTEN.CBL` 

Instead, let's move those lines of code into a new paragraph called `WRITE-NEW-RECORD` and call it as needed with `PERFORM`.

```COBOL
    OPEN OUTPUT PRINT-LINE.

    MOVE 'THE NUMBER IS: ' TO MSG-HEADER OF PRINT-REC.

    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.
    PERFORM WRITE-NEW-RECORD.

    CLOSE PRINT-LINE.
    STOP RUN.

WRITE-NEW-RECORD.
    ADD 1 TO COUNTER GIVING COUNTER.
    MOVE COUNTER TO MSG-TO-WRITE.
    WRITE PRINT-REC.
```

This is called an **out-of-line perform statement** because it requires jumping to the paragraph and back again.

This code is still pretty repetitive.

Loops!!

The following contains an **inline perform statement.**

```COBOL
    OPEN OUTPUT PRINT-LINE.

    MOVE 'THE NUMBER IS: ' TO MSG-HEADER OF PRINT-REC.

    PERFORM VARYING COUNTER FROM 01 BY 1 UNTIL COUNTER EQUAL 11 
    ADD 1 TO COUNTER GIVING COUNTER
    MOVE COUNTER TO MSG-TO-WRITE
    WRITE PRINT-REC
    END-PERFORM.

    CLOSE PRINT-LINE.
    STOP RUN.
```

(note: no periods)

Loop (inline) vs function call (out-of-line).

`GOTO` exists. That is all.

## What Are Paragraphs and How Do We Use Them?

Plan your programs in paragraphs.

If you were to start white-boarding the overall flow of a program, drawing words and boxes, and connecting those boxes with arrows, a lot of those boxes will get implemented as paragraphs.

Paragraphs are defined in the `PROCEDURE DIVISION` and start on column 8.

The declaration of a paragraph is terminated with a period.

The paragraph itself contains one or more sentences and is terminated either by the start of another paragraph or the end of the program.

From an application architecture perspective, you really want to spend some time upfront charting out the flow of your program and deciding what part will get handled in what paragraph. 

Good uses for paragraphs:

1. You find yourself typing the same couple of lines over and over again.
1. 

Paragraphs can come before or after the code that calls it, but you should try to order your paragraphs close to the order they will be executed at runtime.

You may see paragraphs name with numbers such as `1000-OPEN-FILES`, `2000-READ-NEXT-RECORD`, etc. to provide logical grouping of functionality. "This paragraph belongs in the 1000's so call it `1001-OPEN-CSV`." This was done to make each paragraph easier to find. With modern editors' ability to easily jump between the paragaphs of a program this isn't necessary.

You may also see empty paragraphs like what follows:

```cobol
1000-OPEN-FILES.
    OPEN INPUT  ACCT-REC.
    OPEN OUTPUT PRINT-LINE.
1000-OPEN-FILES-END.

2000-READ-NEXT-RECORD.
    PERFORM 4000-READ-RECORD
    PERFORM UNTIL LASTREC = 'Y'
    PERFORM 5000-WRITE-RECORD
    PERFORM 4000-READ-RECORD
    END-PERFORM
2000-READ-NEXT-RECORD-END
```

This provides a visual indicator of the end of a paragraph and can make reading code easier. It also comes into play when using the `PERFORM THROUGH` keyword (more on this later).

One of the simplist ways of setting up a loop is with `TIMES`.

```cobol
PERFORM 10 TIMES
    MOVE FIELD-A TO FIELD-B
    WRITE RECORD
END-PERFORM
```

or

```cobol
PERFORM MY-NEW-PARAGRAPH COUNTER TIMES.
```

Then there's `THROUGH` which can also be spelled `THRU`.

```cobol
1000-PARAGRAPH-A
    PERFORM 2000-PARAGRAPH-B THRU
            3000-PARAGRAPH-C.
*
2000-PARAGRAPH-B.
    ...
*
3000-PARAGRAPH-C.
    ...
*
4000-PARAGRAPH-D.
    ...
```

This allows us to `PERFORM` a series of paragraphs. Another reason to carefully consider the order in which you write them.

We can also `PERFORM UNTIL` which will evaluate the check and if that condition is **not** met it will take some action. This is usually done until a variable hits a certain value or two variables are the same.

In this example we do the check then optionally run the code.

```cobol
MOVE 0 TO COUNTER.
PERFORM UNTIL COUNTER = 10
    ADD 1 TO COUNTER GIVING COUNTER
    MOVE COUNTER TO MSG-TO-WRITE
    WRITE PRINT-REC
END-PERFORM.
```

There is also `PERFORM VARYING`:

```cobol
PERFORM VARYING COUNTER FROM 01 BY 1 UNTIL COUNTER EQUALS 11.
```

If you want to get "fancy" you can nest performs:

```cobol
PERFORM VARYING COUNTER FROM 01 BY 1 UNTIL COUNTER EQUAL 11
    AFTER COUNTER-2 FROM 01 BY 1 UNTIL COUNTER-2 EQUAL 5
    ...
...
END-PERFORM.
```

Every time we increment `COUNTER` by one we will step `COUNTER-2` from 1-5 in increments of one, so we would get: 1;1, 2, 3, 4, 5,2;1, 2, 3, 4, 5, 3;1, 2, 3, 4, 5, and so on. 

<hr>

### Paragraphs vs. Functions

A COBOL program is structured into divisions, sections, paragraphs, and sentences. Here's how paragraphs relate to functions in other languages:

1. **Encapsulation of Logic:** Like functions in other languages, COBOL paragraphs encapsulate a specific unit of logic or a sequence of statements that perform a particular task. They allow you to organize and modularize your code by breaking it into smaller, more manageable parts.

2. **Reusability:** You can invoke a paragraph from multiple locations within the same program, similar to how you can call a function from different parts of a program in other languages. This promotes code reuse and helps maintain consistency across your program.

3. **Parameters and Return Values:** While COBOL paragraphs can accept parameters passed through the CALL statement and can pass data back using shared variables, they do not have explicit parameters or return values like functions in languages such as C, Java, or Python. Instead, COBOL typically relies on the use of data items defined in the WORKING-STORAGE SECTION or LINKAGE SECTION for passing information between paragraphs.

4. **Invocation:** COBOL paragraphs are invoked using the CALL statement, which transfers control to the specified paragraph. The called paragraph executes its logic and control returns to the calling paragraph upon completion. This is similar to how functions are called in other languages.

5. **Separation of Concerns:** Just like functions in other languages, COBOL paragraphs help in organizing code by separating different functionalities into distinct units. This promotes readability, maintainability, and ease of understanding.

However, it's important to note that COBOL paragraphs have their own unique characteristics and limitations compared to functions in other languages. For example, paragraphs are not standalone entities like functions but are part of a larger program structure defined within divisions and sections. Additionally, COBOL does not support recursion, so paragraphs cannot call themselves directly.

<hr>

## Program Linkage

How do we call another program?

`CALL` keyword followed by the name of the program.

```cobol
CALL 'PROGA1' ...
...
MOVE 'PROGA2' to PROGRAM-NAME.
CALL PROGRAM-NAME ...
```

The way a calling program calls a called program is through something called **linkage**.

The program being called as a section called the linkage section (here it defines month, day, and year all suffixed with `-LS`). Because it has a linkage section it knows it will be called and prepares itself with a `PROCEDURE DIVISION` that has the `USING` keyword. It's going to use those three variables from the linkage section (`MONTH-LS`, `DAY-LS`, `YEAR-LS`) along with the three variables (`YEAR-WS`, `MONTH-WS`, `DAY-WS`) in the `WORKING-STORAGE SECTION`.

The calling program sets up three variables (`MONTH-OUT`, `DAY-OUT`, `YEAR-OUT`) which in this case match the variables in the linkage section.

`CALL 'DATESUB' USING MONTH-OUT, DAY-OUT, YEAR-OUT.` calls the program giving it those three variables to store the output it generates.

There are ways of passing actual data as opposed to references to data, mechanisms for getting a return value from a called program and all other goodies.

```cobol
05  REPORT-HEADER-DATE-OUT.
  10  MONTH-OUT	      PICTURE 99.	
  10                  PICTURE X    VALUE '/'.
  10  DAY-OUT         PICTURE 99.	
  10                  PICTURE X    VALUE '/'.
  10  YEAR-OUT        PICTURE 9999.	

CALL 'DATESUB' USING MONTH-OUT, DAY-OUT, YEAR-OUT.

IDENTIFICATION DIVISION.
PROGRAM-ID.    DATESUB.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 HOLD-DATE-WS.
  05 YEAR-WS  PIC 9999
  05 MONTH-WS PIC 99.
  05 DAY-WS   PIC 99.
LINKAGE SECTION.
01 MONTH-LS     PIC 99.
01 DAY-LS       PIC 99.
01 YEAR-LS      PIC 9999.

PROCEDURE DIVISION USING MONTH-LS, DAY-LS, YEAR-LS.

  MOVE FUNCTION CURRENT-DATE TO HOLD-DATE-WS
  MOVE YEAR-WS TO YEAR-LS
  MOVE MONTH-WS TO MONTH-LS
  MOVE DAY-WS TO DAY-LS
  EXIT PROGRAM.
```

<hr>

### `PERFORM` vs. `CALL`

In COBOL, both the PERFORM and CALL statements are used to transfer control to another section of code, but they have different purposes and behaviors:

1. PERFORM Statement:
    * The `PERFORM` statement is used to execute a section of code within the same COBOL program.
    * It typically invokes a paragraph or section within the same program.
    * `PERFORM` is often used to implement looping constructs or to call reusable sections of code multiple times within the same program.
    * The control returns to the next statement after the `PERFORM` once the performed section of code completes execution.
    * The syntax of the `PERFORM` statement is:
        ```cobol
        PERFORM paragraph-name [THROUGH paragraph-name]
        ```
2. CALL Statement:
    * The CALL statement is used to invoke another COBOL program or a specific paragraph within another program.
    * It allows for modularization and code reuse by calling external programs or subroutines.
    * CALL is typically used to invoke functions, subroutines, or external programs that perform specific tasks.
    * Control returns to the calling program after the called program completes execution.
    * The syntax of the CALL statement is:
        ```cobol
        CALL program-name [USING data-list] [RETURNING identifier]
        ````
Here are some key differences between `PERFORM` and `CALL`:

* **Scope:** `PERFORM` is used to execute sections of code within the same program, while `CALL` is used to invoke other programs or paragraphs from external sources.

* **Modularization:** `CALL` facilitates modularization by allowing programs to be split into smaller, reusable components stored in separate programs or paragraphs, whereas `PERFORM` is typically used for code reuse within the same program.

* **Data Passing:** `CALL` supports passing data between programs using the `USING` phrase, allowing for communication between different program units. `PERFORM` generally relies on shared data items defined in the `WORKING-STORAGE SECTION` or `LINKAGE SECTION` for data passing.

* **Return Values:** `CALL` can return control and, optionally, data values to the calling program using the `RETURNING` phrase, whereas `PERFORM` does not return data values to the caller.

In summary, `PERFORM` is used for internal control flow and code reuse within the same program, while `CALL` is used for invoking external programs or paragraphs and facilitating modularity and code reuse across multiple programs.

<hr>

## File Output and Reports

Previously we looked at the `SELECT` and respective `ASSIGN TO` programmer-chosen names.

```cobol
*--------------------
ENVIRONMENT DIVISION.
*--------------------
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT PRINT-LINE ASSIGN TO PRTLINE.
    SELECT ACCT-REC   ASSING TO ACCTREC.
```

We used `PRTLINE` for the internal COBOL filename for the output.

In the file descriptor marked by `FD` within the file control paragraph, we start to connect it all together.

```cobol
*-------------
DATA DIVISION.
*-------------
FILE SECTION.
FD  PRINT-LINE RECORDING MODE F.
01  PRINT-REC.
    05 ACCT-NO-O        PIC X(8).
    05 FILLER           PIC X(02) VALUE SPACES.
    05 LAST-NAME-O      PIC X(20).
    05 FILLER           PIC X(02) VALUE SPACES.
    05 ACCT-LIMIT-O     PIC $$,$$$,$$9.99
    05 FILLER           PIC X(02) VALUE SPACES.
    05 ACCT-BALANCE-O   PIC $$,$$$,$$9.99
    05 FILLER           PIC X(02) VALUE SPACES.
```

The `FD` entry defines the layout of the record withing that `PRNTLINE` internal file.

You can see we're saying `FD` is a file. The highest level descriptor is `PRINT-REC` and then within that `PRINT-REC`, we have the individual fields defined.

First, `ACCT-NO-O`, presumably "O" for output, that's the value of eight alphanumeric characters.

Then `FILLER`, a COBOL reserved word. We use `FILLER` to allocate memory space, typically between words or variables. Filler can be empty, it can be dashes or any literal. In this example, it's eight characters of account number then two spaces. You can see that we spelled out spaces there with S-P-A-C-E-S. That's a handy shortcut so you don't actually have to type out the spaces.

Consider this partial program.

```cobol
 WORKING-STORAGE SECTION.
*
 01  HEADER-1.
     05  FILLER         PIC X(20) VALUE 'FINANCIAL REPORT FOR'.
     05  FILLER         PIC X(60) VALUE SPACES.
* 
 01  HEADER-2.
     05  FILLER         PIC X(05) VALUE 'YEAR '.
     05  HDR-YR         PIC 9(04).
     05  FILLER         PIC X(02) VALUE SPACES.
     05  FILLER         PIC X(06) VALUE 'MONTH '.
     05  HDR-MO         PIC X(02).
     05  FILLER         PIC X(02) VALUE SPACES.
     05  FILLER         PIC X(04) VALUE 'DAY '.
     05  HDR-DAY        PIC X(02).
     05  FILLER         PIC X(56) VALUE SPACES.
* 
 01  HEADER-3.
     05  FILLER         PIC X(08) VALUE 'ACCOUNT '.
     05  FILLER         PIC X(02) VALUE SPACES.
     05  FILLER         PIC X(10) VALUE 'LAST NAME '.
     05  FILLER         PIC X(15) VALUE SPACES.
     05  FILLER         PIC X(06) VALUE 'LIMIT .
     05  FILLER         PIC X(06) VALUE SPACES.
     05  FILLER         PIC X(08) VALUE 'BALANCE '.
     05  FILLER         PIC X(40) VALUE SPACES.
* 
 01  HEADER-4.
     05  FILLER         PIC X(08) VALUE '--------'.
     05  FILLER         PIC X(02) VALUE SPACES.
     05  FILLER         PIC X(10) VALUE '----------'.
     05  FILLER         PIC X(15) VALUE SPACES.
     05  FILLER         PIC X(10) VALUE '----------'.
     05  FILLER         PIC X(02) VALUE SPACES.
     05  FILLER         PIC X(13) VALUE '-------------'.
     05  FILLER         PIC X(40) VALUE SPACES.
```

There is no logic here, it merely defines what the data looks like.

```cobol
 01 WS-CURRENT-DATA-DATA.
     05  WS-CURRENT-DATE.
         10-WS-CURRENT-YEAR         PIC 9(04).
         10-WS-CURRENT-MONTH        PIC 9(02).
         10-WS-CURRENT-DAY          PIC 9(02).
    05  WS-CURRENT-TIME.
         10-WS-CURRENT-HOURS        PIC 9(02).
         10-WS-CURRENT-MINUTES      PIC 9(02).
         10-WS-CURRENT-SECONDS      PIC 9(02).
         10-WS-CURRENT-MILLISECONDS PIC 9(02).
```

The high-level variable name is `WS-CURRENT-DATA-DATA` (`WS` for working storage). Within that are two sections, `WS-CURRENT-DATE` and `WS-CURRENT-TIME`. The date is divided up into the year, month, and day and the time is divided up into hours, minutes, seconds, and milliseconds.

```cobol
 WRITE-HEADERS
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
    MOVE WS-CURRENT-YEAR  TO HDR-YR.
    MOVE WS-CURRENT-MONTH TO HDR-MO.
    MOVE WS-CURRENT-DAY   TO HDR-DAY.
    WRITE PRINT-REC FROM HEADER-1.
    WRITE PRINT-REC FROM HEADER-2.
    MOVE SPACES TO PRINT-REC.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
    WRITE PRINT-REC FROM HEADER-3.
    WRITE PRINT-REC FROM HEADER-4.
    MOVE SPACES TO PRINT-REC.
```

`CURRENT-DATE` is an *intrinsic function*.

`MOVE SPACES TO PRINT-REC.` is necessary to clear any stray characters that may be left behind. We want to start the next series of moves and writes assuming we have a blank record.

