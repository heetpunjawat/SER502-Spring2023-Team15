# SER502-Spring2023-Team15
Private Github repository for SER502 Spring2023 Team 15 Project
---------------------------------------------------------------------
# All the installations steps and testing is specific to MacOS only

### Tools used
VScode, SWI Prolog, Python 
---------------------------------------------------------------------
## Follow all the installation steps below

### Installation of homebrew on MacOS 
First install homebrew on your MacOS.
Step 1 - Open the terminal
Step 2 - Enter the follwoing command in the terminal 

% /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

After entering this command, the latest version of homebrew will be installed on your MacOS. 
---- 
### Installation of command line SWI Prolog on MacOS
Step 1 - Open the terminal 
Step 2 - Enter the following command in the terminal 

% brew install swi-prolog

After entering this command, Command line SWI Prolog will be installed on your MacOS.
----
### Installation of SLY for python 
Step 1 - Open the terminal 
Step 2 - Enter any one of the two following commands. If one doesnt work, then enter the second one.

% pip install sly     
OR        
% python3 -m pip install sly 
----
## Installation complete 
If you any installation error occurs, look up on the internet on how to install the above 
packages and applications for your computer.
---------------------------------------------------------------------
## Building the project and running the Code (Tested only on MacOS)

Step 1 - Open the terminal 
Step 2 - Select the terminal directory to this project folder 
Step 3 - Change the terminal directory to the "src" folder of this project 
Step 4 - Run the following command in the terminal 

% python lexer.py --evaluate testFinal.phx  

The output will be displayed in the terminal itself. 
A file called testfinal.phxtokens will also be geenrated in the src folder.
This file contains all the tokens from the testFinal.phx file that we tested.

Instead of using testFinal.phx, you can also use other test files such as testPrint.phx,
testWhile.phx, testBasicForLoop.phx.
--------------------------------------------------------------------- 
## Link to the youtube video:
