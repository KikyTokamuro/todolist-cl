# todolist-cl

Todolist with web UI, written in Common Lisp

![preview](./screenshots/preview.png)

## Dependencies
 - [hunchentoot](https://github.com/edicl/hunchentoot) - Web server
 - [spinneret](https://github.com/ruricolist/spinneret) - HTML5 generator 
 - [jonathan](https://github.com/Rudolph-Miller/jonathan) - JSON encoder and decoder
 - [mito](https://github.com/fukamachi/mito) - ORM


## Run
```sh
git clone https://github.com/KikyTokamuro/todolist-cl
sbcl --load todolist.asd
* (ql:quickload :todolist)
* (todolist:start-server) ; For start todolist in "localhost:8080"
* (todolist:stop-server) ; For stop todolist
* (todolist:restart-server) ; For restart todolist
```

## Building EXE on Windows and run (may be not working in future releases)
1. Download sqlite3.dll from [sqlite.org](https://www.sqlite.org/download.html) and put to project directory
2. Open PowerShell and build:
```powershell
sbcl --load .\todolist.asd `
    --eval '(push :hunchentoot-no-ssl *features*)' `
    --eval '(ql:quickload :todolist)' `
    --eval "(sb-ext:save-lisp-and-die #p\`"todolist.exe\`" :toplevel #'(lambda () (todolist:start-server) (sb-thread:join-thread (find-if (lambda (th) (search \`"hunchentoot-listener\`" (sb-thread:thread-name th))) (sb-thread:list-all-threads)))) :executable t)"
```
3. Run todolist.exe

## License
```
MIT License

Copyright (c) 2022-2023 Daniil Arkhangelsky (Kiky Tokamuro)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

