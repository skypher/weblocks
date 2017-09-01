
## Build the documentation

- install [Roswell](https://github.com/roswell/roswell)

- install the [cldomain](http://40ants.com/cldomain/) Sphinx extension
  to build Common Lisp projects' documentation:

    ros install 40ants/cldomain

- create a
  [Python virtual env](https://virtualenv.pypa.io/en/stable/installation/#installation):

    virtualenv env
    source env/bin/activate

- and install the Python dependencies:

    pip install -r docs/requirements.txt

- now build the doc:

    make html

- and open the generated doc with your browser:

    firefox build/html/index.html
