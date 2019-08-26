# Bivalence
<p align="center">
<img src="bivalence.png">
</p>

Bivalence is a programming language designed to make writing programs more logical. Rather than try to prove your program is correct, your program proves to you if it is correct. Every expectation is checked during the runtime of the program, forcing your code to adapt to the unexpected. 

1. [Features](#Features)
2. [Examples](#Examples)
3. [Installation](#Installation)
4. [Contributing](#Contributing)

## Features
### Native Bindings
Other languages have a difficult interface of calling C functions. Native functions are baked into Bivalence in such a way that adding calls seems natural. In fact, the language can be used as more of a C program verifier. 

### TODO
* Safe standard library
Nearly everything could have behavior that is not expected. Even something as simple as addition can run into things such as integer overflows. The safe standard library is aimed to not only handle garbage collect, but also:
** Thread safety: Concurrency pattern prevents most deadlock, and every object is thread safe.
** Memory corruption: Non-bounds checked array access has always been a problem. An exception (and inability to use pointers) is not much better than a segfault. All standard library functions simply return false on what would normally cause either of these. The case is forced to be handled, hence bounds are always gracefully checked.
* VM save states
Running on a VM allows for saving the state of a program and loading it at a future point. This allows for much easier testing as after patches can be applied, the user no longer need to provide specific input to rereach the state of code that a bug may have occurred. 

## Examples
### Hello World
### Native Calls

## Installation 
All configuration is contained in configure.sh. This can be modified with the desired configuration for the VM and compiler.
```
. configure.sh
./install.sh
```

## Contributing
Anyone is free to contribute. Please read the [license](LICENSE) and the [guidelines](guidelines.org) before contributing.
