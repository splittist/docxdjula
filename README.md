# docxdjula

This project is to [Djula](https://github.com/mmontone/djula/) as [python-docx-template](https://github.com/elapouya/python-docx-template/) is to [Jinja2](https://palletsprojects.com/p/jinja).

That is, it uses as much of Djula as makes sense to provide simple templating facilities for Open Office XML WordprocessingML documents - basically, those `.docx` files produced by or for Microsoft Word.

**docxdjula** is aimed at the usecase of composing templates in Microsoft Word (or equivalent wysiwyg word processor) by placing the Djula forms (variable references, tags etc) within the styled text. Those templates are then compiled, and finally rendered in the context of a dictionary-like structure supplying the values of those variables. The emphasis is on **document assembly**, rather than providing a scaffolding for a consistent look-and-feel for a website.

There are some key differnces from standard Djula use. Because a docx file is actually a collection of (sub)files (known as 'parts'), a template cannot really target a stream, as is done in Djula. Instead, the ultimate target of rendering a document is a file (the `.docx` file). And because the template is composed in the wysiwyg editor, then saved as xml, then treated as unstructured text, a certain amount of care is required in placing tags so that Djula (and **docxdjula**) produces a valid `.docx` file at the end.

Nevertheless, treated with care, **docxdjula** provides a simple but powerful document assembly facility with the power of Common Lisp.

## Example

Create a docx file (`TemplateFile.docx`) with some Djula variables, tags and filters in your favourite word processor:


> Hello {{ var1 }}.
> 
> {%p if var2 %}
> 
> Text to include if var2 is true.
> 
> {%p else %}
> 
> Text to include if var2 is false.
> 
> {%p endif %}
> 
> Inline list: {% for thing in var3 %} This is {{ thing }}. {% endfor %}
> 
> Paragraph list:
> 
> {%p for thing in var3 %}
> 
> {{ forloop.counter }}.	{{ thing }}{% if forloop.last %}.{% else %};{% endif %}
> 
> {%p endfor %}
> 
> GOODBYE {{ var1|upper}}.


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

> Hello World.
>
> Text to include if var2 is true.
>
> Inline list: This is Uno.  This is2.  This istrois.
>
> Paragraph list:
>
> 1. Uno;
>
> 2. 2;
>
> 3. trois.
>
> GOODBYE WORLD.

Profit!

## Aims

The broad aim of this project is to provide a 'work-alike' for the way python-docx-templates is used within [Docassemble](https://docassemble.org).

As regards Djula, there is no desire to enable all its functionality to be used with `.docx` files. As a broad comparison:

### API

:heavy_check_mark: _function_ **ADD-TEMPLATE-DIRECTORY**

:heavy_check_mark: _parameter_ **\*CURRENT-COMPILER\***

Should be set to `(make-instance 'docxdjula:docx-compiler)`.

:heavy_check_mark: _function_ **COMPILE-TEMPLATE\***

May be used, once the above compiler setting is made.

:x: _function_ **RENDER-TEMPLATE\***

Do not use. Instead use **DOCXDJULA:RENDER-TEMPLATE\*** `template` `outpath` _&rest_ `template-arguments`, where `outpath` is the pathname-designator for the place where the document produced by rendering `template` in the context of `template-arguments` will be saved.

### Variables

May be used. To avoid confusing yourself and the extended tag syntax, it is best to leave a space between the `{{` and the variable name.

### Tags

May be used with care, and with exceptions (see below).

Because **docxdjula** templates are assumed to be produced in a wysiwyg word processor, whitespace in general (including paragraphs and table rows) is significant. Following the lead of python-docx-template, **docxdjula** provides tag versions that provide for a natural expression of tag logic at the expense of a little extra syntax.

A tag beginning `{%p`, for example, will be treated such that when the template is rendered the paragraph containing that tag is removed. This allows a conditional paragraph to be bracketed by two paragraphs containing `{%p if foop %}` and `{%p endif %}` respectively, and when the document is rendered, the result will look as intended. If the `p` was omitted, the final document would include (probably empty) paragraphs where the `if` and `endif` tags had been. You can see this being used above in the example (and with `for` in the paragraph list).

A similar facility is provided for _table rows_ (`{%tr`), _table cells_ (`{%tc`) and _runs_ (`{%r`). Note that the consequences are undefined (but unlikely to be good) if such a tag is used twice in the same context (e.g. two `{%p`s in the one paragraph).

:heavy_check_mark: :heavy_exclamation_mark: _tags_ **block**, **extends**, and **super**

May be used, but with limitations. In particular, only the main story of the `super` template will be used (no headers, footers, footnotes or endnotes etc).

:heavy_check_mark: _tag_ **comment**

:heavy_check_mark: _tag_ **cycle**

:heavy_exclamation_mark: _tag_ **debug**

Partially supported (no fancy debugging information).

:x: _tag_ **filter**

:heavy_check_mark: _tag_ **firstof**

:heavy_check_mark: _tag_ **for** and variables.

:heavy_check_mark: _tags_ **if**, **ifchanged**, **ifequal**, **ifnotequal**

:heavy_check_mark: _tag_ **include**

May be used, with the following restrictions:

* Only the body text of the included document will be imported (i.e. no headers, footers, footnotes, endnotes, comments or style definitions)

* No images, drawings or embedded documents may be included (Note: this restriction may be removed in future)

* The included document should have only one section (no section breaks) (Note: this restriction may be removed in future)

:heavy_check_mark: _tags_ **set-language**, **show-language**

:heavy_check_mark: _tag_ **set-package**

:heavy_check_mark: _tag_ **set**

:heavy_check_mark: _tag_ **lisp**

:x: _tags_ __**Various Javascript tags**__

:x: _tag_ **autoescape**

### Comments

:heavy_check_mark: The `{#` to `#}` comment syntax may be used.

### Verbatim

:heavy_check_mark: The `{$` to `$}` verbatim syntax may be used.

### Filters

These are generally supported, noting that the various (`html`-)escaping tags are not helpful.

:heavy_check_mark: _filter_ **add**

:x: _filter_ **addslashes**

:heavy_check_mark: _filter_ **capfirst**

:heavy_check_mark: _filter_ **cut**

:heavy_check_mark: _filter_ **date**

:heavy_check_mark: _filter_ **time**

:heavy_check_mark: _filter_ **datetime**

:heavy_check_mark: _filter_ **default**

:heavy_check_mark: _filter_ **sort**

:heavy_check_mark: _filter_ **reverse**

:heavy_check_mark: _filter_ **first**

:heavy_check_mark: _filter_ **join**

:heavy_check_mark: _filter_ **last**

:heavy_check_mark: _filter_ **length**

:x: _filter_ **linebreaks**

But see _filter_ **xlinebreaks**.

:x: _filter_ **linebreaksbr**

:heavy_check_mark: _filter_ **lower**

:heavy_check_mark: _filter_ **safe**

:heavy_check_mark: _filter_ **slice**

:heavy_check_mark: _filter_ **format**

:heavy_check_mark: _filter_ **time**

:heavy_check_mark: _filter_ **truncatechars**

:heavy_check_mark: _filter_ **upper**

:x: _filter_ **urlencode**

:x: _filter_ **force-esacape**

:heavy_check_mark: _filter_ **replace**

:heavy_check_mark: _filter_ **with**

:heavy_check_mark: _filter_ **scan**

### Template Inheritance

Somewhat supported. (See above.)

### Internationalization

:heavy_check_mark: The `{_` to `_}` translation syntax may be used.

:heavy_check_mark: _tag_ **trans**

:heavy_check_mark: _filter_ **trans**

:heavy_check_mark: _parameter_ **\*djula:current-language\***

### Error Handling & Rest of API

TODO

### Questionnaires

One way of collecting the template arguments (to be bound to `djula:*template-arguments*` when rendering a template) is with 'questionnaire', being a Word document containing a table, the first row of which contains a cell with the text "Variable" and another with the text "Answer" (ignoring case). The entries in the corresponding columns are extracted into an alist, with the text in the "variable" cells interned into the `keyword` package. The "answer" cells are treated as plain text. (This may change.)

To make your questionnaire aesthetically pleasing, you can hide the first row and the "answer" column, using the facilities (or workarounds) provided by your word processor, and add whatever other columns (e.g. "Question", "Examples") or formatting you wish.

_function_ **EXTRACT-TEMPLATE-ARGUMENTS** `document`

Extracts variables and values from `document` as above, where `document` can be a pathname, a string denoting the path, or a `DOCXPLORA:DOCUMENT` object.
