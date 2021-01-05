# Introduction

This package provides some useful interactive and non-interactive
functions for working with *sudo* shell program.

## Quickstart

Just invoke ```alpha-sudo-shell-command``` command through ```M-x``` to
execute a shell command via sudo. 

## API

This package exposes the following functions:

* ```(alpha-sudo-shell-command COMMAND)```. This interactive function prompt
for the shell COMMAND to be executed via sudo. Additionally, if sudo requires
a password for the specified COMMAND, it also prompt the user for his password.
Then it executes the COMMAND via sudo and display the result in the minibuffer
or in a separate buffer, depending on lenght of the output.

* ```(alpha-sudo-exec COMMAND SENTINEL &optional PASSWORD)```. This
non-interactive function executes the shell COMMAND via sudo. If
user PASSWORD is required to execute COMMAND via sudo, it shall be 
provided as optional argument. At the end of the command exectution, the 
SENTINEL function is called. The sentinel function has signature 
```(sentinel PROGRAM EXITCODE OUTPUT)``` where:

    * PROGRAM is name of the program executed via sudo

    * EXITCODE is the exitcode of the program

    * OUTPUT is a string obtained by merging program stardard output and
    standard error

* ```(alpha-sudo-allowed-programs)```. This non-interactive function returns an
alist with two keys:

    * *passwd* key, that is associated to the list of programs that can be 
    executed via sudo providing the user password. If the user can run any
    command via sudo providing his password, the list contains the sole "ALL"
    element.

    * *nopasswd* key, that is associated to the list of programs that can be
    executed via sudo without providing the user password. If the user can run
    any command via sudo without providing his password, the list contains the
    sole "ALL" element.

* ```(alpha-sudo-can-p PROGRAM)```. This non-interactive function returns *t* 
if the user can run PROGRAM via sudo, otherwise returns *nil*.

* ```(alpha-sudo-require-password-p PROGRAM)```. This non-interactive function
returns *t* if running PROGRAM via sudo requires the user password,
returns *nil* if the user password is not required, and raises an error if
the user cannor run PROGRAM via sudo.

* ```(alpha-sudo-path)```. This non-interactive function returns the path of 
sudo shell program. If the path of sudo cannot be found (sudo has to be in 
user's path), this function returns *nil*.

## Keybindings

The package does not introduce any new keybinding. It is suggested to bind
```alpha-sudo-shell-command``` to a key for executing commands via sudo more
quickly.

## Notes

At the moment this package has been tested on a limited setup and it is not
guaranteed to work for every linux/configuration. If you encounter problems,
please report them.

## Contact

Please report bugs, requests and suggetions to 
Alpha Catharsis <alpha.catharsis@gmail.com> or on Github.
