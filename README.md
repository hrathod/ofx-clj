# ofx-clj

A Clojure wrapper for the OFX4J library for parsing OFX documents.

## Usage

The wrapper exposes two functions to fetch the data from
an OFX file.  There is currently no functionality for
writing OFX data to a file.

Use the wrapper in the following way:

(ns mypackage.core
  (require '[ofx-clj.core :as ofx]))

; get the entire OFX file as a map
(def data (ofx/parse "/path/to/data.ofx"))

; get all the banking messages from the map
(ofx/find-all-transactions data "banking")

Two sample OFX files are included in the "resources" directory.

## License

Copyright (C) 2012 Himanshu Rathod

Distributed under the Eclipse Public License, the same as Clojure.
