# CL-JSON-LD

CL-JSON-LD is a library that implements JSON-LD functionality,
as described on [the JSON-LD website](http://www.json-ld.org/).

Note that this is not the [CL-JSON library](http://common-lisp.net/project/cl-json/).

The implementation of this library was inspired by and closely follows [the pyld library](https://github.com/digitalbazaar/pyld).

## Authors

The library was created by **RD Projekt Sp. z o.o. Sp. k.** You can reach us at <info@rdprojekt.pl> or <http://www.rdprojekt.pl/>.

## License

Copyright (c) 2014 RD Projekt Sp. z o.o. Sp. k.

Use is subject to license terms.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Installation

Use `cl-json-ld.asd` file to install the library.

If you have [Quicklisp](http://www.quicklisp.org/) installed, you can use it to download dependencies automatically:
```lisp
(push "/path/to/cl-json-ld/" asdf:*central-registry*)
(ql:quickload :cl-json-ld)
```

Depending on whether you have ST-JSON or Yason installed for JSON
handling, one of those will be used. If no library is installed,
ST-JSON will be downloaded by Quicklisp.

## JSON-LD tests

You can run the JSON-LD test suite by executing:
```lisp
(asdf:operate 'asdf:test-op :cl-json-ld-tests)
```

## Usage

General notes: 

* Whenever a function accepts options, options is a plist.
* JSON objects are objects in our internal format (objects are hashmaps,
lists are lists) converted from either ST-JSON or Yason. You can use 
`jsd-read` to read them from string or `jsd-to-string` to write them to string.

The module exports the following functions:

### from-rdf (input &optional options)

Converts RDF input into ST-JSON.

If a string is passed as input, it's assumed to be 
in the "application/nquads" format and converted to the internal dataset
format.

The result is a JSON object.

If you want more control over formats, you can use either the `register-rdf-parser` function
or the `json-ld-processor` class
and its methods: `processor-from-rdf` and `processor-register-rdf-parser`.

### to-rdf (input &optional options)

Converts JSON input into a string 
(if :format option equals "application/nquads")
or to the internal dataset format.

Input can be a URL or a JSON object.
You can pass `:document-loader` in options to use a custom URL loader.

### expand (input &optional options)

Performs JSON-LD expansion.

Input can be a URL or a JSON object.
You can pass `:document-loader` in options to use a custom URL loader.

Output is a JSON object.

### compact (input ctx &optional options)

Performs JSON-LD compaction.

Input is the same as for the `expand` function.

Ctx can be a JSON object or a URL.

You can pass `:document-loader` in options to use a custom URL loader.

Output is a JSON object.

### flatten (input ctx &optional options)

Performs JSON-LD flattening.

Input is the same as for the `expand` function.

Ctx can be a JSON object or a URL.

You can pass `:document-loader` in options to use a custom URL loader.

Output is a JSON object.

### normalize (input &optional options)

Performs JSON-LD normalization.

Input can be a URL or a JSON object.

You can pass `:document-loader` in options to use a custom URL loader.

Output is a string (if `:format` option equals `"application/nquads"`) 
or internal dataset format.

### frame (input frame &optional options)

Performs JSON-LD framing.

Frame and input can be URLs or JSON objects.

You can pass `:document-loader` in options to use a custom URL loader.

Output is a JSON object.

### set-document-loader (load-document)

Set `load-document` function as the default document loader.

### get-document-loader ()

Return the default document loader function.

### jsd-read (source)

Read a string or stream and return internal JSON representation.

### jsd-to-string (element)

Write a JSON element (in internal JSON representation) to string.

### jsd-make (args)

Make an internal JSON representation object with keys and values as alternating values of args.
Example:
```lisp
(jsd-make "foo" 3
          "bar" 5)
```
