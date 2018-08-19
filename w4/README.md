# w4
-Task2 is implemented in src file. All of the required functions are shown in the .hs file. Moreover, the 4.2.3 part is implemented without state monad as it is not required. If we wanted to implement it with state then this would be implemented in the insert function.
-Task3 is implemented in the benchmark file with Task3.hs being its source code. In order to produce the given .html file someome should just run:
stack bench --benchmark-arguments '--output=benchmark.html' 
in the command line. Also, the expected html file is also included, which shows the expected O(log n) complexity, as we notice the linear increase in time regarding n, with n being the height of the tree Perfect. Thus, if we consider that the size of the tree is 2^n then we get what we expected.
-Task4 source file is in the src file under the name Task4.hs. The corresponding executable is in the app file, while there is Lib.hs in src file which implements the Task4.hs but with input from stdin, as required.
