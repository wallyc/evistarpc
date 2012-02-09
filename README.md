EVISTARPC
=========

The Erlang Vista RPC driver (a.k.a. evistarpc) provides an interface to the Vista RPC broker. More information on Vista can be found at [worldvista.org] and on OpenVista at [medsphere.org]. Please see the index.html file located the doc directory to learn more and for an example session using the Erlang shell.

This code was built and tested using the Erlang R14B04 release for 64 bit linux and OpenVista.

Installation
------------

Download the code:
	git clone git://github.com/wallyc/evistarpc.git
	cd into the evistarpc directory

Check the Host and Port values for the Vista RPC Broker:
The default values are Host=127.0.0.1 & Port=9201. If the values are different for your Vista installation/server you will need to alter the values on line 10 in the src/evistarpc.app.src file.

Compile:
	make


Getting Started
---------------

See doc/index.html for an example of use and API information.

   [1]: http://worldvista.org
   [2]: http://medsphere.org


