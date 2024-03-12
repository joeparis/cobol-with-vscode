## Basic COBOL Syntax

Column-dependent

5 key areas of a 72-character line

<pre>
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | ... | 72 | 73 | ......... | 80 |
|        Sequence       | ^ |                 |                         |                |
|         Number        | | |        A        |            B            | Identification |
|          Area         | | |       Area      |           Area          |      Area      |
|<--------------------->| | |<--------------->|<----------------------->|<-------------->|
                          |
                      Indicator
                        Area
</pre>

**Sequence Number Area (columns 1-6):** used to give context to a series of statements, can be blank.

**Indicator Area (column 7):** multi-purpose:

  * asterisk (*): line is a comment
  * dash (-): line is a continuation of the previous line
  * D: debugging line
  * slash (\): source code listing formatting  

**A Area (columns 8-11):** divisions, sections, paragraphs, level indicators, and other elements that give COBOL programs their structure.

**B Area (columns 12-72):** statements, sentences, and clauses that make up a COBOL program.

**Identification Area (columns 73-80):** Ignored by the compiler, can be used by the programmer for any purpose. OFten left blank.

### COBOL Reserved Words

* [Enterprise COBOL for z/OS 6.3](https://www.ibm.com/docs/en/cobol-zos/6.3?topic=appendixes-reserved-words)
* [Enterprise COBOL for z/OS 6.4](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=appendixes-reserved-words)

### Enterprise COBOL for z/OS Documentation

[Enterprise COBOL for z/OS Documentation](https://www.ibm.com/docs/en/cobol-zos/6.4)

## What Are COBOL Divisions?

COBOL has a hierarchical structure.

Divisions --> Sections --> Paragraphs --> Sentences --> Statements

A **statement** is a single directive, usually starting with a reserved word.

A **sentence** is a logical container for multiple statements. 

Example using a period (.) as an implicit scope terminator.

```cobol
ADD 2 TO TOTAL.
````

Example using an explicit scope terminator.

```cobol
IF ITEM = "B"
     ADD 2 TOP TOTAL
END-IF
```
A **paragraph** is a user-defined container used to give the program structure. A paragraph generally represents a specific action made up of smaller steps. Paragraphs can be called by name elsewhere in the program (think function).

**Sections** contain paragraphs. There are many types of sections, with both user-defined and predefined names. Certain information must be present in specific sections.

There are four (4) COBOL **divisions**:

* **Identification:** contains the name of the program, the name of the programmer, when the program was written, how it should be used, ect.
* **Environment:** contains to major sections - one that lets you set the type of computer environment required to run the program and the other sets up mappings between the files in your program and the files on the actual datasets. It's the link between your program and the system it's running on. 
* **Data:** sets up all the data that will be used within your program including files, data from other programs, the type of storage or memory used while the program is running, and what it will give up when the program ends.
* **Procedure:** contains all the sections and paragraphs. The instructions that comprise the program.

## COBOL Variables

In COBOL, the name given to a variable is known as the *data name* and must be 30 characters or less in length and can contain letters, numbers, or hashes.

Ex.

* `balance`
* `inventory12345`
* `NorthAmericansalesin2020-final`

Variables must be declared along with their type: numeric, alphabetic, or alphanumeric. We must also specifiy how many letters or numbers we think we will be using.

This is done in the **picture** or **pic** clause which sets the length and datatype of a programmer-selected data name.

```cobol
PIC 9
```

This declares a single numeric variable with length one. The 9 means numeric - it is ***not*** the value of the variable nor its length. Because no length was specified we get the default length of one.

```cobol
PIC 9(4)
```

This declares four numeric variables. For example, the year "2024".

```cobol
PIC A
```

A single alphabetic character.

```
PIC X(8)
```

Eight aplhanumeric characters (X for alphanumeric).

The **maximum length** of a numeric picture clause is 18, while an alphabetic pic can be up to 255 characters.

There are other types of pic clauses.

```cobol
PIC 9(4)V99
```

Four digits, a decimal point (the V), followed by two more digits.

```cobol
PIC $9,999V99
```

Currency up to $9,999.99.

Constants are called **literals**. There are some *figurative literals* built in.

* `ZERO` / `ZEROS`
* `SPACE` / `SPACES`
* `LOW-VALUE`
* `HIGH-VALUE`
* `NULL` / `NULLS`

