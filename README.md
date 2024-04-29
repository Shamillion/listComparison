# listComparison
------

This program is an example of a simple Haskell application with a graphical interface implemented using the Threepenny library. Electron was used to compile the program into a separate self-contained application.

The program compares lists with a standard list and writes matching items to a file.

You should have installed Haskell Cabal and Nodejs.

To deploy the project, you need to perform the following steps:
1. Clone this repository.
2. Open terminal and go to the root folder of the project.
3. Compile the project with the 
   ```haskell
   cabal build
   ```
   command.
4. Run the program with the 
   ```haskell
   ./node_modules/.bin/electron electron.js
   ```

