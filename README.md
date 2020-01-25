# docxdjula

### _John Q. Splittist <splittist@splittist.com>_

This project is to [Djula](https://github.com/mmontone/djula/) as [python-docx-template](https://github.com/elapouya/python-docx-template/) is to [Jinja2](https://palletsprojects.com/p/jinja).

That is, it uses as much of Djula as makes sense to provide simple templating facilities for Open Office XML WordprocessingML documents - basically, those `.docx` files produced by or for Microsoft Word.

**docxdjula** is aimed at the usecase of composing templates in Microsoft Word (or equivalent wysiwyg word processor) by placing the Djula forms (variable references, tags etc) within the styled text. Those templates are then compiled, and finally rendered in the context of a dictionary-like structure supplying the values of those variables. The emphasis is on **document assembly**, rather than providing a scaffolding for a consistent look-and-feel for a website.

There are some key differnces from standard Djula use. Because a docx file is actually a collection of (sub)files (known as 'parts'), a template cannot really target a stream, as is done in Djula. Instead, the ultimate target of rendering a document is a file (the `.docx` file). And because the template is composed in the wysiwyg editor, then saved as xml, then treated as unstructured text, a certain amount of care is required in placing tags so that Djula (and **docxdjula**) produces a valid `.docx` file at the end.

Nevertheless, treated with care, **docxdjula** provides a simple but powerful document assembly facility with the power of Common Lisp.

## Example

Create a docx file (`TemplateFile.docx`) with some Djula variables, tags and filters in your favourite word processor:

```
Hello {{ var1 }}.

{%p if var2 %}

Text to include if var2 is true.

{%p else %}

Text to include if var2 is false.

{%p endif %}

Inline list: {% for thing in var3 %} This is {{ thing }}. {% endfor %}

Paragraph list:

{%p for thing in var3 %}

{{ forloop.counter }}.	{{ thing }}{% if forloop.last %}.{% else %};{% endif %}

{%p endfor %}

GOODBYE {{ var1|upper}}.

```

Save the file as `TemplateFile.docx`.

Switch to your favourite repl:


```lisp
(ql:quickload "docxdjula")

(setf djula:*current-compiler* (make-instance 'docxdjula:docx-compiler))

(djula:add-template-directory "/path/to/your/template/directory")

(djula:compile-template* "TemplateFile.docx")

(docxdjula:render-docx-template * "/path/to/OutFile.docx" :var1 "World" :var2 t :var3 '("Uno" 2 "trois"))
```

Open `OutFile.docx` in your word processor.

Profit!

## Aims

The broad aim of this project is to provide a 'work-alike' for the way python-docx-templates is used within [Docassemble](https://docassemble.org).

As regards Djula, there is no desire to enable all its functionality to be used with `.docx` files. As a broad comparison:

### API

_function_ **ADD-TEMPLATE-DIRECTORY**

May be used.

_parameter_ **\*CURRENT-COMPILER\***

Should be set to `(make-instance 'docxdjula:docx-compiler)`.

_function_ **COMPILE-TEMPLATE\***

May be used, once the above compiler setting is made.

_function_ **RENDER-TEMPLATE\***

Do not use. Instead use **DOCXDJULA:RENDER-TEMPLATE\*** `template` `outpath` _&rest_ `template-arguments`, where `outpath` is the pathname-designator for the place where the document produced by rendering `template` in the context of `template-arguments` will be saved.

### Variables

May be used. To avoid confusing yourself and the extended tag syntax, it is best to leave a space between the `{{` and the variable name.

### Tags

May be used with care, and with exceptions (see below).

Because **docxdjula** templates are assumed to be produced in a wysiwyg word processor, whitespace in general (including paragraphs and table rows) is significant. Following the lead of python-docx-template, **docxdjula** provides tag versions that provide for a natural expression of tag logic at the expense of a little extra syntax.

A tag beginning `{%p`, for example, will be treated such that when the template is rendered the paragraph containing that tag is removed. This allows a conditional paragraph to be bracketed by two paragraphs containing `{%p if foop %}` and `{%p endif %}` respectively, and when the document is rendered, the result will look as intended. If the `p` was omitted, the final document would include (probably empty) paragraphs where the `if` and `endif` tags had been. You can see this being used above in the example (and with `for` in the paragraph list).

A similar facility is provided for _table rows_ (`{%tr`), _table cells_ (`{%tc`) and _runs_ (`{%r`). Note that the consequences are undefined (but unlikely to be good) if such a tag is used twice in the same context (e.g. two `{%p`s in the one paragraph).

_tags_ **block**, **extends**, and **super**

Not currently supported. May be supported in future.

_tag_ **comment**

May be used.

_tag_ **cycle**

May be used.

_tag_ **debug**

Partially supported (no fancy debugging information).

_tag_ **filter**

Not currently supported.

_tag_ **firstof**

May be used.

_tag_ **for**

May be used, along with its variables.

_tags_ **if**, **ifchanged**, **ifequal**, **ifnotequal**

May be used.

_tag_ **include**

May be used, with the following restrictions:

* Only the body text of the included document will be imported (i.e. no headers, footers, footnotes, endnotes, comments or style definitions)

* No images, drawings or embedded documents may be included (Note: this restriction may be removed in future)

* The included document should have only one section (no section breaks) (Note: this restriction may be removed in future)

_tags_ **set-language**, **show-language**

May be used.

_tag_ **set-package**

May be used.

_tag_ **set**

May be used.

_tag_ **lisp**

May be used.

_tags_ __** Various Javascript tags**__

Not supported.

_tag_ **autoescape**

Not supported.

### Comments

The `{#` to `#}` comment syntax may be used.

### Verbatim

The `{$` to `$}` verbatim syntax may be used.

### Filters

These are generally supported, noting that the various (`html`-)escaping tags are not helpful.

_filter_ **add**

May be used.

_filter_ **addslashes**

Not supported.

_filter_ **capfirst**

May be used.

_filter_ **cut**

May be used.

_filter_ **date**

May be used.

_filter_ **time**

May be used.

_filter_ **datetime**

May be used.

_filter_ **default**

May be used.

_filter_ **sort**

May be used.

_filter_ **reverse**

May be used.

_filter_ **first**

May be used.

_filter_ **join**

May be used.

_filter_ **last**

May be used.

_filter_ **length**

May be used.

_filter_ **linebreaks**

Not supported.

_filter_ **linebreaksbr**

Not supported.

_filter_ **lower**

May be used.

_filter_ **safe**

Not supported.

_filter_ **slice**

May be used.

_filter_ **format**

May be used.

_filter_ **time**

May be used.

_filter_ **truncatechars**

May be used.

_filter_ **upper**

May be used.

_filter_ **urlencode**

Not supported.

_filter_ **force-esacape**

Not supported.

_filter_ **replace**

May be used.

_filter_ **with**

May be used.

_filter_ **scan**

May be used.

### Template Inheritance

Not currently supported.

# Internationalization

The `{_` to `_}` translation syntax may be used.

_tag_ **trans**

May be used.

_filter_ **trans**

May be used.

_parameter_ **\*djula:current-language\***

May be used.

### Error Handling & Rest of API

TODO


## License

Copyright 2020 John Q. Splittist

Permission is hereby granted, free of charge, to any person obtaining a copy 
of this software and associated documentation files (the "Software"), to deal 
in the Software without restriction, including without limitation the rights 
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
copies of the Software, and to permit persons to whom the Software is furnished 
to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
THE SOFTWARE.

