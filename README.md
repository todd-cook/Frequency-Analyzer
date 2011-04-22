Frequency-Analyzer
==================

This Frequency-Analyzer library provides many of the basic methods for dissecting and analyzing
a cryptogram or a body of text. Frequency analysis is an important technique for cryptanalysis
and computational linguistics.

Provided Examples
-----------------
* Automatic decryption and best candidate selection of substitution ciphers
* Scala Swing GUI for casual users
* How to create a standalone executable jar file using Maven
* Running Scala Spec tests inside Maven test phase
* Scala test integration with JUnit
* Scala Test Spec as BDD (Behavior Driven Development)
* Cobertura report on unit test code coverage
* Maven generation of Scaladocs

How To Run the GUI
------------------
1. Download the project
2. Download and set up [Maven 2](http://maven.apache.org)
3. Navigate to the project directory, and run these commands:

* cd FrequencyAnalyzer;
* mvn clean package;
* cd target;
* java -jar frequency-analyzer-gui-dist.jar;

Two jar files are created in the target directory:

 frequency-analyzer-dist.jar   (7.5M)
 frequency-analyzer.jar        (152k)

`frequency-analyzer-dist.jar` is a standalone executable jar, and the application may be launched by
just double-clicking on the jar file. This jar also contains all the Scala and Scala swing libraries.

`frequency-analyzer.jar` is the library jar and it only contains the project's core files.


Related links
-------------
* `Scala Test` <http://scalatest.org>
* `Frequency Analysis` <http://en.wikipedia.org/wiki/Frequency_analysis>
* `Maven` <http://maven.apache.org>
* `MigLayout` <http://miglayout.com>

 To Do:
------
* Enhance the Scala to Java interface so that this library provides a common library to Java programs.
* Provide a separate standalone Java program example that uses the library (in progress).
* More unit tests.
* Expand multilingual character support.
