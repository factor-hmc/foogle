# foogle web app

# Installation
1. Clone this repo 
2. Install [nix](https://nixos.org/nix/)
3. Run the Factor script `make-database.factor` to generate a database file.
4. Run `$ nix-build` in the root to build the project.

# Execution
Open the file in `./result/bin/foogle-exe.jsexe/index.html` in a web browser 
to run the webapp.

# Notes
Currently, it is very slow. Also, you have to manually give it the database by 
using the file upload box on the page.
