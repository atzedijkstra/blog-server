# blog-server
A playfield and demo (blog) for use of the snap web server framework

# Installation & running
Build using cabal. Location of executable is mentioned by cabal build.
The following app is build:
* blog-server

Then either:
* File stage.tgz (holding state and log) can be unpacked into the
  directory from which the server is run to provide a sample initial
  state, existing users are 'jan' and 'piet' with the same password as
  name. Or,
* do a 'mkdir log'

Then run by (e.g.):
* .cabal-sandbox/bin/blog-server -p 8000
* open a browser at http://localhost:8000

# Main used libraries
* Snap: http://snapframework.com/
* Digestive functors: https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs
* Blaze: http://jaspervdj.be/blaze/tutorial.html
* Acid state: http://acid-state.seize.it/
* Safecopy: http://acid-state.seize.it/safecopy
