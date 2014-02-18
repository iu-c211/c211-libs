c211-libs
=========

A collection of libraries designed for the C211 course at IU.

To install these tools, you can use raco from the command line:

'''
raco pkg install github://github.com/iu-c211/c211-libs/master
'''

You may find it easier to just run the following in DrRacket:

'''
(require pkg)
(install "github://github.com/iu-c211/c211-libs/master")
'''

It should run the install, tell you the execution thread finished, and now you
should have the c211-libs install for racket!

To use them, just require them at the top of the file:

'''
(require c211-libs/image)
(require c211-libs/tree)
(require c211-libs/matrix)
'''

To update, either run:

'''
raco pkg update c211-libs
'''

or File -> Install Package -> type
"github://github.com/iu-c211/c211-libs/master" -> Press OK

Everything is open source, and the codebase is available through Github.

You may find the Wombat documentation helpful, much of the syntax is shared
between the libraries.

'''
http://www.cs.indiana.edu/cgi-pub/c211/wombat/docs/c211-image.htm
http://www.cs.indiana.edu/cgi-pub/c211/wombat/docs/c211-tree.htm
http://www.cs.indiana.edu/cgi-pub/c211/wombat/docs/c211-matrix.htm
'''

If you would like to contribute to the libraries, or with documentation, feel
free to let sjkirby know.