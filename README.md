# jiyu
A compiler for fun.

## Quick Setup
Clone this repository:
```
git clone https://github.com/machinamentum/jiyu.git
cd jiyu
```
Build LLVM and jiyu:
##### Windows
```
docs\setup_windows.bat
mkdir build
cd build
cmake .. -G "Visual Studio 15 2017 Win64" -Thost=x64
cmake --build .
cd ..
```
##### macOS
```
docs/setup_unix.sh
mkdir build
cd build
cmake ..
cmake --build .
cd ..
```
`docs\setup_windows.bat` and `docs/setup_unix.sh` fetch, build, and install LLVM into \<jiyu\>/llvm. This may take awhile, go enjoy a walk on the beach or something!

## Examples
The `tests` directory contain several isolated code examples for verifying compiler functionality. For a more in-depth example, see [jiyu_game](https://github.com/machinamentum/jiyu_game).
