FROM ubuntu:trusty

RUN apt-get update
RUN apt-get install -y \
    sbcl \
    git \
    build-essential \
    automake \
    libcurl4-openssl-dev \
    libev4
RUN git clone -b v17.12.10.86 https://github.com/roswell/roswell.git /roswell
RUN cd /roswell && ./bootstrap && ./configure && make install

RUN ros install sbcl-bin/1.4.3
RUN ros install ccl-bin/1.11 && ros use ccl-bin/1.11

# Version fixed for more build stability
# TODO: update it from time to time
RUN ros install svetlyak40wt/qlot/stable

# Install fixed ASDF version, to have it same on all lisp implementations
RUN ros install asdf/3.3.1.1

# Set UTF-8 locale to overcome style warning:
# "Character decoding error..."
RUN locale-gen en_US.UTF-8
ENV LC_ALL=en_US.UTF-8


ENV PATH=~/.roswell/bin/:$PATH

RUN mkdir /projects
ENV CL_SOURCE_REGISTRY='/projects//'

COPY run-tests.sh /run-tests.sh
CMD /run-tests.sh
