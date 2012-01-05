This project is meant to accompany the written piece [CL-Chan (a CLSQL and Hunchentoot crash course) - Part 1](http://langnostic.blogspot.com/2011/08/cl-chan-clsql-and-hunchentoot-crash.html).

If you want to poke around with it, you'll need to set some stuff up.

1. Create a mysql database and a user with all privileges on it
2. Change the line starting with "`(connect`" to match user and DB information to the previous step
3. Change the expression starting with `(setf formlets:*public-key*`so that it contains your [ReCaptcha](http://www.google.com/recaptcha) public and private keys.
3. Load `:cl-who`, `:hunchentoot`, `:clsql` and [`:formlets`](https://github.com/Inaimathi/formlets) into your lisp
4. Load "cl-chan.lisp"
5. Run `(create-test-database)`

You should then be able to browse to `http://localhost:4242/board` to see a basic interface message-board interface.
