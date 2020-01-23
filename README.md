# docxdjula

### _John Q. Splittist <splittist@splittist.com>_

This project is to [Djula](https://github.com/mmontone/djula/) as [python-docx-template](https://github.com/elapouya/python-docx-template/) is to [Jinja2](https://palletsprojects.com/p/jinja).

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

{%p enfor %}

GOODBYE {{ var1|upper}}.

```

Save the file as `TemplateFile.docx`.

Switch to your favourite repl:


```
(ql:quickload "docxdjula")

(setf djula:*current-compiler* (make-instance 'docxdjula:docx-compiler))

(djula:add-template-directory "/path/to/your/template/directory")

(djula:compile-template* "TemplateFile.docx")

(docxdjula:render-docx-template * "/path/to/OutFile.docx" :var1 "World" :var2 t :var3 '("Uno" 2 "trois"))
```

Open `OutFile.docx` in your word processor.

Profit!

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

