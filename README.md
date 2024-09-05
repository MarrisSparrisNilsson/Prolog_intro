# Prolog_intro

For the Artificial Intelligence course we are diving into prolog programming

## Set up

1. Have Prolog installed on your computer.
2. You can check the version with the following command:

   `swipl --version`

3. Install the prolog VS Code extension: `arthurwang.vsc-prolog`
4. Check the `prolog.executablePath` and make sure it's pointing to the swipl executable. On Windows the file path should be specified with `/` only.
5. Configure `prolog.load.document` on save when pressing `Ctrl+S` to complie the active `.pl` file. Set the when-expression to `resourceExtname == ".pl"`

## Getting started

1. Initiate Prolog with `swipl`
2. All predicates (`?-`) should end with `.`, you can add them on the next line if you forget to add it.
3. End the prolog CLI with `?- halt.`
