# Prolog intro

For the Artificial Intelligence course we are diving into prolog programming

## Table of contents

1. [Prolog intro](#prolog-intro)
    - [Table of contents](#table-of-contents)
    - [Set up](#set-up)
    - [Getting started](#getting-started)
    - [Debugging](#debugging)
2. [Prolog info](#prolog-info)

## Set up

1. Have Prolog installed on your computer.
2. You can check the version with the following command:

    `swipl --version`

3. Install the prolog VS Code extension: `arthurwang.vsc-prolog`
4. Check the `prolog.executablePath` and make sure it's pointing to the swipl executable. On Windows the file path should be specified with `/` only.
5. Configure `prolog.load.document` on save when pressing `Ctrl+S` to complie the active `.pl` file. Set the when-expression to `resourceExtname == ".pl"`

### Getting started

1. Initiate Prolog with `swipl`
2. All predicates (`?-`) should end with `.`, you can add them on the next line if you forget to add it.
3. End the prolog CLI with `?- halt.`

### Debugging

When the terminal outputs the warning:

`Warning: Singleton variables: [First]`

It usually warns you that the variable `'First'` is unused. Replacing it with `_` should remove this warning.

## Prolog info

Prolog (short for **Pro**gramming in **Log**ic) is a declarative programming language primarily used for solving problems involving logic, reasoning, and symbolic processing. Its unique strengths lie in pattern matching, automatic backtracking, and the ease of expressing logical relationships. It is especially well-suited for tasks where complex rule-based systems or search spaces need to be explored.

### Common Applications of Prolog:

1. **Artificial Intelligence (AI)**:

    - **Expert Systems**: Prolog is widely used in building expert systems where complex knowledge is encoded using rules. For example, medical diagnosis systems can infer diseases based on a set of symptoms.
    - **Natural Language Processing (NLP)**: Prolog is used for parsing and understanding human language. Its ability to handle recursive structures (such as sentences) makes it ideal for writing parsers and grammar analyzers.
    - **Knowledge Representation and Reasoning**: Prolog is good at representing knowledge in the form of facts and rules, and it allows for reasoning over this knowledge to make inferences, deduce new facts, or draw conclusions.

2. **Automated Theorem Proving**:

    - Prolog’s roots in formal logic make it an excellent tool for tasks that require proving mathematical theorems automatically, solving logical puzzles, or verifying the correctness of systems.

3. **Constraint Logic Programming (CLP)**:
    - Prolog is extended to **Constraint Logic Programming** (e.g., CLP(FD) for finite domains), where it is used to solve problems with constraints, such as scheduling, resource allocation, and optimization problems (like the traveling salesman problem or Sudoku).
4. **Data Mining and Machine Learning**:
    - Prolog can be used for tasks like **Inductive Logic Programming (ILP)**, where it learns new rules and patterns from existing data based on logical inference. This is useful in fields like bioinformatics or relational databases.
5. **Problem Solving and Search**:

    - Prolog's natural ability to explore a search space using backtracking makes it suitable for solving puzzles, game-playing (like chess or checkers), and graph-related problems (e.g., finding paths in graphs).

6. **Symbolic Reasoning**:

    - Prolog excels in applications that involve manipulating symbols, terms, or abstract objects rather than numeric computation. Examples include algebraic systems, symbolic differentiation, and generating proofs.

7. **Database Systems**:
    - Prolog is used in deductive databases where relationships between facts can be inferred using logical rules. Unlike traditional SQL databases, deductive databases allow for richer queries based on logical inference.
8. **Education and Research**:

    - Prolog is commonly used in academia and research, particularly in the fields of AI, cognitive science, and logic programming. Its logical foundations provide a good way to explore computational logic and algorithms based on reasoning.

9. **Parsing and Compilers**:

    - Prolog is also used for parsing structured input, such as language grammars, code compilers, and interpreters. It can efficiently handle grammars using its built-in support for **Definite Clause Grammars (DCG)**.

10. **Games and Simulation**:
    - Prolog's ability to handle complex rules makes it suitable for creating games (like adventure games) and simulations that require a lot of logic and decision-making.

### Key Features of Prolog That Make These Applications Possible:

1. **Declarative Programming Paradigm**: Instead of specifying how to solve a problem (like in imperative languages), Prolog focuses on **what** the solution should look like. This is useful for problems where the relationships between data need to be specified, and Prolog automatically searches for the solution.
2. **Pattern Matching and Unification**: Prolog’s core operation is **unification**, which automatically matches structures, making it easy to express complex relationships between data.

3. **Backtracking**: Prolog systematically explores possible solutions to a problem by backtracking to try alternative paths, making it ideal for search and decision-making tasks.

4. **Inference Engine**: Prolog’s logic engine automatically applies rules and facts to infer new knowledge or find solutions, making it suitable for reasoning tasks.

5. **Recursion**: Prolog naturally supports recursion, which is essential for problems involving hierarchical structures or repetitive tasks, such as parsing, tree traversal, and recursive problem solving.

### Examples of Prolog in Use:

-   **Expert Systems**: A system for medical diagnosis that asks questions about symptoms and infers the possible disease based on a rule set.

    ```prolog
    disease(flu) :- fever, cough, body_ache.
    fever.
    cough.
    body_ache.
    ```

    Query:

    ```prolog
    ?- disease(flu).
    true.
    ```

-   **Family Tree and Genealogy**: Prolog can easily model family relationships like parent, ancestor, sibling, etc., and deduce new relationships.

    ```prolog
    parent(john, mary).
    parent(mary, ann).
    ancestor(X, Y) :- parent(X, Y).
    ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
    ```

    Query:

    ```prolog
    ?- ancestor(john, ann).
    true.
    ```

-   **Solving Puzzles**: Prolog can solve puzzles like Sudoku or logic puzzles due to its ability to handle constraints and backtracking.

In conclusion, Prolog is a powerful language for logic-based, rule-driven applications, especially those involving AI, reasoning, and symbolic processing. Its declarative nature makes it a good fit for problems that involve expressing relationships and constraints, and its built-in inference engine handles the task of finding solutions.
