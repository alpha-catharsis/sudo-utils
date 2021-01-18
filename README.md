# Introduction

This package provides some useful interactive and non-interactive 
functions for working with *sudo* shell program.

## Quickstart

Just invoke ```sudo-utils-shell-command``` command through ```M-x``` to 
execute a shell command via sudo. 

## API

This package exposes the following functions:

* ```(sudo-utils-shell-command COMMAND)```. This interactive function prompt 
for the shell COMMAND to be executed via sudo. Additionally, if sudo requires 
a password for the specified COMMAND, it also prompts the user for his password. 
Then it executes the COMMAND via sudo asynchronously and displays the result 
in the minibuffer or in a separate buffer, depending on lenght of the output.
User default shell is used for the execution of COMMAND.

* ```(sudo-utils-program PROGRAM-AND-ARGS)```. This interactive function prompt 
for the program with arguments PROGRAM-AND-ARGS to be executed via sudo. 
Additionally, if sudo requires a password for the specified program, it also
prompts the user for his password.  Then it executes the program via sudo 
asynchronously and displays the result in the minibuffer or in a separate
buffer, depending on lenght of the output.

* ```(sudo-utils-shell-command-async COMMAND CALLBACK &optional PASSWORD SHELL)```. 
This non-interative function executes asynchronously shell command COMMAND as
root via sudo. If user PASSWORD is required to execute PROGRAM via sudo, it 
shall be provided as optional argument. At the end of COMMAND exectution, the 
CALLBACK function is called if not nil. The callback function has signature
```(callback EXITCODE BUFER)``` where:

    * EXITCODE is the exitcode of the program.

    * BUFFER is a buffer containing the program stardard output and
standard error.

* ```(sudo-utils-exec-program-async PROGRAM CALLBACK &optional PASSWORD &rest ARGS)```. 
This non-interative function executes asynchronously program PROGRAM with 
arguments ARGS as root via sudo. If user PASSWORD is required to execute PROGRAM
via sudo, it shall be provided as optional argument. At the end of program
exectution, the CALLBACK function is called if not nil. The callback function 
has signature ```(callback EXITCODE BUFER)``` where:

    * EXITCODE is the exitcode of the program.

    * BUFFER is a buffer containing the program stardard output and
standard error.

* ```(sudo-utils-exec-program-sync PROGRAM &optional PASSWORD &rest ARGS)```. 
This non-interative function executes synchronously program PROGRAM with 
arguments ARGS as root via sudo. It returns a list containing the program exit
code and output as string. If user PASSWORD is required to execute PROGRAM via
sudo, it shall be provided as optional argument.

* ```(sudo-utils-shell-command-sync COMMAND &optional PASSWORD SHELL)```. 
This non-interative function executes synchronously shell command COMMAND as
root via sudo. It returns a list containing the program exit code and output
as string. If user PASSWORD is required to execute PROGRAM via sudo, it shall be
provided as optional argument. If SHELL is not provided, the user default shell
is used.

* ```(sudo-utils-allowed-programs &optional PASSWORD)```. This non-interactive 
function returns an alist with two keys:

    * *passwd* key, that is associated to the list of programs that can be 
    executed via sudo providing the user password. If the user can run any
    command via sudo providing his password, the list contains the sole "ALL"
    element.

    * *nopasswd* key, that is associated to the list of programs that can be
    executed via sudo without providing the user password. If the user can run
    any command via sudo without providing his password, the list contains the
    sole "ALL" element.

  If optional argument PASSWORD is not passed, the returned alist might be
  empty depending on sudo configuration.

* ```(sudo-utils-can-p PROGRAM)```. This non-interactive function returns *t* 
if the user can run PROGRAM via sudo, otherwise returns *nil*.

* ```(sudo-utils-can-run-p)```. This non-interactive function returns *t* if 
the user can run sudo, otherwise it returns *nil*.

* ```(sudo-utils-can-run-program-p PROGRAM &optional PASSWORD)```. This 
non-interactive function returns *t* if the user can run PROGRAM program via
sudo, *nil* if he cannot and *unknown* symbol if it is not possible to
determine if the user can. Passing the optional argument PASSWORD allows to
obtain always a known result, i.e. result can be only *t* or *nil*.

* ```(sudo-utils-require-password-p PROGRAM &optional PASSWORD)```. This 
non-interactive function returns *t* if the password is required for running
program PROGRAM, *nil* if it is not required and *unknown* symbol if it is not
possible to determine if the password is required. Passing the optional argument
PASSWORD allows to obtain always a known result, i.e. result can be only *t* or *nil*.

* ```(sudo-utils-abs-path)```. This non-interactive function returns the
absolute path of sudo shell program. If the path of sudo cannot be found
(sudo has to be in user's path), this function returns *nil*.

## Keybindings

The package does not introduce any new keybinding. It is suggested to bind
```sudo-utils-shell-command``` to a key for executing commands via sudo more
quickly.

## Contact

Please report bugs, requests and suggestions to 
Alpha Catharsis <alpha.catharsis@gmail.com> or on Github.
