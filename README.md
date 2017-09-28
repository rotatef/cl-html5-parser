cl-html5-parser: HTML5 parser for Common Lisp
=============================================

## Abstract

cl-html5-parser is a HTML5 parser for Common Lisp with the following features:

* It is a port of the Python library [html5lib](http://code.google.com/p/html5lib/).
* It passes all relevant tests from html5lib.
* It is not tied to a specific DOM implementation.


## Requirements

* SBCL or ECL.
* CL-PPCRE and FLEXI-STREAMS.

Might work with CLISP, ABCL and Clozure CL, but many of the tests don't pass there.


## Usage


### Parsing

Parsing functions are in the package HTML5-PARSER.

```
parse-html5 source &key encoding strictp dom
    => document, errors
```

Parse an HTML document from source. Source can be a string, a pathname
or a stream. When parsing from a stream encoding detection is not
supported, encoding must be supplied via the encoding keyword
parameter.

When strictp is true, parsing stops on first error.

Returns two values. The primary value is the document node. The
secondary value is a list of errors found during parsing. The format
of this list is subject to change.

The type of document depends on the dom parameter. By default it's an
instance of cl-html5-parser's own DOM implementation. See the DOM
paragraph below for more information.

```
parse-html5-fragment source &key container encoding strictp dom
    => document-fragment, errors
```

Parses a fragment of HTML. Container sets the context, defaults to
"div". Returns a document-fragment node. For the other parameters see
`PARSE-HTML5`.


### Example
```common-lisp
(html5-parser:parse-html5-fragment "Parse <i>some</i> HTML" :dom :xmls)
==> ("Parse " ("i" NIL "some") " HTML")
```

### The DOM

Parsing HTML5 is not possible without a
[DOM](http://en.wikipedia.org/wiki/Document_Object_Model). cl-html5-parser
defines a minimal DOM implementation for this task. Functions for
traversing documents are exported by the HTML5-PARSER package.

Alternatively the parser can be instructed to to convert the document
into other DOM implementations using the dom parameter. The conversion
is done by simply calling the generic function
transform-html5-dom. Support for other DOM implementations can be
added by defining new methods for this generic function. The dom
parameter is either a symbol or a list where the car is a symbol and
the rest is key arguments. Below is the currently supported target
types.


### Namespace of elements and attributes

The HTML5 syntax has no support for namespaces, however the standard
defines special rules to set the expected namespace for SVG and MathML
elements and the following attributes: `xlink:actuate`,
`xlink:arcrole`, `xlink:href`, `xlink:role`, `xlink:show`,
`xlink:title`, `xlink:type`, `xml:base`, `xml:lang`, `xml:space`,
`xmlns`, `xmlns:xlink`. Please note that this only applies to SVG and
MathML elements. Attributes of HTML elements will never get a
namespace.

#### Examples

```html
<html xml:lang='en'><svg xml:lang='en></svg></html>
```

* Element `html` with namespace `http://www.w3.org/1999/xhtml`
* Attribute with name `xml:lang` (no prefix)
* Element `svg` with namespace `http://www.w3.org/2000/svg`
* Attribute with prefix `xml`, local name `lang`, namespace `http://www.w3.org/XML/1998/namespace`

```common-lisp
(html5-parser:parse-html5 "<!doctype html><html xml:lang='en' xml@lang='en'>" :dom :xmls-ns)
==>
(("html" . "http://www.w3.org/1999/xhtml")
 (("xmlU00003Alang" "en") ("xmlU000040lang" "en")) ("head" NIL) ("body" NIL))
```

On an HTML element `xml:lang` and `xml@lang` are just attributes with
unusual characters in their name. In the HTML DOM these names are kept
as is, but when converting to XML they are escaped, to ensure the XML
becomes valid. This escaping can be reversed with
`HTML5-PARSER:XML-UNESCAPE-NAME`.

```common-lisp
(html5-parser:parse-html5 "<!doctype html><svg xml:lang='en' xml@lang='en' xlink:href='#' xlink:to='#'></svg>" :dom :xmls-ns)
==>
(("html" . "http://www.w3.org/1999/xhtml") NIL ("head" NIL)
 ("body" NIL
  (("svg" . "http://www.w3.org/2000/svg")
   (("xml:lang" "en") ("xmlU000040lang" "en") ("xlink:href" "#")
    ("xmlns:xlink" "http://www.w3.org/1999/xlink") ("xlinkU00003Ato" "#")))))
```

In this case the `xml:lang` and `xmlns:xlink` is one of those
attributes with known namespace when used on SVG and MathML
elements. However `xlink:to` is not the list, even if it's defined in
the xlink standard.

### :XMLS or (:XMLS &key namespace comments)

Converts a node into a simple
[XMLS](http://common-lisp.net/project/xmls/)-like list structure.
If node is a document fragment a list of XMLS nodes a returned. In
all other cases a single XMLS node is returned.

If namespace argument is true, tag names are conses of name and
namespace URI.

By default comments are stripped. If comments argument is true,
comments are returned as (:COMMENT NIL "comment text"). This extension
of XMLS format.


### :CXML

Convert to [Closure XML Parser](http://common-lisp.net/project/cxml/)
DOM implementation. In order to use this you must load/depend on the
the system cl-html5-parser-cxml.


## License

This library is available under the
[GNU Lesser General Public License v3.0](http://www.gnu.org/licenses/lgpl.html).
