;;;; ginjish-tests.lisp

(cl:defpackage #:ginjish-tests
  (:use #:cl #:parachute))

(cl:in-package #:ginjish-tests)

(define-test core-tags-suite)

TestForLoop

simple
"{% for item in seq %}{{ item }}{% endfor %}"
seq = range(10)
"0123456789"

else
"{% for item in seq %}XXX{% else %}...{% endfor %}"
(no seq)
"..."

else_scoping
"{% for item in [] %}{% else %}{{ item }}{% endfor %}"
(item = 42)
"42"

empty_blocks
"<{% for item in seq %}{% else %}{% endfor %}>"
(no env)
"<>"

context_vars
slist = [42, 24]
for seq in [slist, iter(slist), reversed(slist), (_ for _ in slist)]:
"""{% for item in seq -%}
            {{ loop.index }}|{{ loop.index0 }}|{{ loop.revindex }}|{{
                loop.revindex0 }}|{{ loop.first }}|{{ loop.last }}|{{
               loop.length }}###{% endfor %}"""
one, two, _ = tmpl.render(seq=seq).split("###")
(
 one_index,
 one_index0,
 one_revindex,
 one_revindex0,
 one_first,
 one_last,
 one_length,
 ) = one.split("|")
(
 two_index,
 two_index0,
 two_revindex,
 two_revindex0,
 two_first,
 two_last,
 two_length,
 ) = two.split("|")

assert int(one_index) == 1 and int(two_index) == 2
assert int(one_index0) == 0 and int(two_index0) == 1
assert int(one_revindex) == 2 and int(two_revindex) == 1
assert int(one_revindex0) == 1 and int(two_revindex0) == 0
assert one_first == "True" and two_first == "False"
assert one_last == "False" and two_last == "True"
assert one_length == two_length == "2"

cycling
 """{% for item in seq %}{{
            loop.cycle('<1>', '<2>') }}{% endfor %}{%
            for item in seq %}{{ loop.cycle(*through) }}{% endfor %}"""
(seq=list(range(4)), through=("<1>", "<2>"))
"<1><2>" * 4

lookaround
 """{% for item in seq -%}
            {{ loop.previtem|default('x') }}-{{ item }}-{{
            loop.nextitem|default('x') }}|
        {%- endfor %}"""
(seq=list(range(4)))
"x-0-1|0-1-2|1-2-3|2-3-x|"

changed
 """{% for item in seq -%}
            {{ loop.changed(item) }},
        {%- endfor %}"""
(seq=[None, None, 1, 2, 2, 3, 4, 4, 4])
"True,False,True,True,False,True,True,False,False,"

scope
"{% for item in seq %}{% endfor %}{{ item }}"
(seq=list(range(10)))
assert not output

varlen
"{% for item in iter %}{{ item }}{% endfor %}"
(iter=range(5))
"01234"

noniter
"{% for item in none %}...{% endfor %}"
pytest.raises(TypeError, tmpl.render)

recursive
 """{% for item in seq recursive -%}
            [{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]
        {%- endfor %}"""
		    seq=[
		    dict(a=1, b=[dict(a=1), dict(a=2)]),
		    dict(a=2, b=[dict(a=1), dict(a=2)]),
		    dict(a=3, b=[dict(a="a")]),
		    ]
		    )
		   == "[1<[1][2]>][2<[1][2]>][3<[a]>]"
		   )

recursive_lookaround
 """{% for item in seq recursive -%}
            [{{ loop.previtem.a if loop.previtem is defined else 'x' }}.{{
            item.a }}.{{ loop.nextitem.a if loop.nextitem is defined else 'x'
            }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]
        {%- endfor %}"""
		    seq=[
		    dict(a=1, b=[dict(a=1), dict(a=2)]),
		    dict(a=2, b=[dict(a=1), dict(a=2)]),
		    dict(a=3, b=[dict(a="a")]),
		    ]
		    )
		   == "[x.1.2<[x.1.2][1.2.x]>][1.2.3<[x.1.2][1.2.x]>][2.3.x<[x.a.x]>]"
		   )

recursive_depth0
 """{% for item in seq recursive -%}
        [{{ loop.depth0 }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]
        {%- endfor %}"""
		    seq=[
		    dict(a=1, b=[dict(a=1), dict(a=2)]),
		    dict(a=2, b=[dict(a=1), dict(a=2)]),
		    dict(a=3, b=[dict(a="a")]),
		    ]
		    )
		   == "[0:1<[1:1][1:2]>][0:2<[1:1][1:2]>][0:3<[1:a]>]"
		   )

recursive_depth
 """{% for item in seq recursive -%}
        [{{ loop.depth }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]
        {%- endfor %}"""
		    seq=[
		    dict(a=1, b=[dict(a=1), dict(a=2)]),
		    dict(a=2, b=[dict(a=1), dict(a=2)]),
		    dict(a=3, b=[dict(a="a")]),
		    ]
		    )
		   == "[1:1<[2:1][2:2]>][1:2<[2:1][2:2]>][1:3<[2:a]>]"
		   )

looploop
 """{% for row in table %}
            {%- set rowloop = loop -%}
            {% for cell in row -%}
                [{{ rowloop.index }}|{{ loop.index }}]
            {%- endfor %}
        {%- endfor %}"""
(table=["ab", "cd"])
"[1|1][1|2][2|1][2|2]"

reversed_bug
		       "{% for i in items %}{{ i }}"
		       "{% if not loop.last %}"
		       ",{% endif %}{% endfor %}"
(items=reversed([3, 2, 1]))
"1,2,3"

loop_errors
		                   """{% for item in [1] if loop.index
                                      == 0 %}...{% endfor %}"""
pytest.raises(UndefinedError, tmpl.render)
??????????
          """  %}{{ loop }}{% endfor %}"""
	    )
assert tmpl.render() == ""

loop_filter
		       "{% for item in range(10) if item is even %}[{{ item }}]{% endfor %}"
"[0][2][4][6][8]"

 """
            {%- for item in range(10) if item is even %}[{{
                loop.index }}:{{ item }}]{% endfor %}"""
"[1:0][2:2][3:4][4:6][5:8]"

loop_unassignable(self, env):
pytest.raises(
	      TemplateSyntaxError, env.from_string, "{% for loop in seq %}...{% endfor %}"
				   )

scoped_special_var
		    "{% for s in seq %}[{{ loop.first }}{% for c in s %}"
		    "|{{ loop.first }}{% endfor %}]{% endfor %}"
(seq=("ab", "cd"))
"[True|True|False][False|True|False]"

scoped_loop_var
 "{% for x in seq %}{{ loop.first }}"
		    "{% for y in seq %}{% endfor %}{% endfor %}"
(seq="ab")
"TrueFalse"

 "{% for x in seq %}{% for y in seq %}"
		    "{{ loop.first }}{% endfor %}{% endfor %}"
(seq="ab")
"TrueFalseTrueFalse"

recursive_empty_loop_iter
 """
        {%- for item in foo recursive -%}{%- endfor -%}
        """
(dict(foo=[]))
""

call_in_loop
 """
        {%- macro do_something() -%}
            [{{ caller() }}]
        {%- endmacro %}
        {%- for i in [1, 2, 3] %}
            {%- call do_something() -%}
                {{ i }}
            {%- endcall %}
        {%- endfor -%}
        """
()
"[1][2][3]"

scoping_bug
		                """
        {%- for item in foo %}...{{ item }}...{% endfor %}
        {%- macro item(a) %}...{{ a }}...{% endmacro %}
        {{- item(2) -}}
        """
(foo=(1,))
"...1......2..."

unpacking
 "{% for a, b, c in [[1, 2, 3]] %}{{ a }}|{{ b }}|{{ c }}{% endfor %}"
()
"1|2|3"

intended_scoping_with_set
 "{% for item in seq %}{{ x }}{% set x = item %}{{ x }}{% endfor %}"
(x=0, seq=[1, 2, 3])
"010203"

 "{% set x = 9 %}{% for item in seq %}{{ x }}"
 "{% set x = item %}{{ x }}{% endfor %}"
(x=0, seq=[1, 2, 3])
"919293"

TestIfCondition:

simple
"""{% if true %}...{% endif %}"""
()
"..."

elif
 """{% if false %}XXX{% elif true
            %}...{% else %}XXX{% endif %}"""
()
"..."

elif_deep
elifs = "\n".join(f"{{% elif a == {i} %}}{i}" for i in range(1, 1000))
f"{{% if a == 0 %}}0{elifs}{{% else %}}x{{% endif %}}"
for x in (0, 10, 999):
assert tmpl.render(a=x).strip() == str(x)
assert tmpl.render(a=1000).strip() == "x"

else
"{% if false %}XXX{% else %}...{% endif %}"
()
"..."

empty
"[{% if true %}{% else %}{% endif %}]"
()
"[]"

complete
 "{% if a %}A{% elif b %}B{% elif c == d %}C{% else %}D{% endif %}"
(a=0, b=False, c=42, d=42.0)
"C"

no_scope
"{% if a %}{% set foo = 1 %}{% endif %}{{ foo }}"
(a=True)
"1"
"{% if true %}{% set foo = 1 %}{% endif %}{{ foo }}"
()
"1"


TestMacros:

simple
			                """\
{% macro say_hello(name) %}Hello {{ name }}!{% endmacro %}
{{ say_hello('Peter') }}"""
()
"Hello Peter!"

scoping
			                """\
{% macro level1(data1) %}
{% macro level2(data2) %}{{ data1 }}|{{ data2 }}{% endmacro %}
{{ level2('bar') }}{% endmacro %}
{{ level1('foo') }}"""
()
"foo|bar"

arguments
			                """\
{% macro m(a, b, c='c', d='d') %}{{ a }}|{{ b }}|{{ c }}|{{ d }}{% endmacro %}
{{ m() }}|{{ m('a') }}|{{ m('a', 'b') }}|{{ m(1, 2, 3) }}"""
()
"||c|d|a||c|d|a|b|c|d|1|2|3|d"

arguments_defaults_nonsense
pytest.raises(
	      TemplateSyntaxError,
	      env_trim.from_string,
	                  """\
{% macro m(a, b=1, c) %}a={{ a }}, b={{ b }}, c={{ c }}{% endmacro %}""",
)

caller_defaults_nonsense
pytest.raises(
	      TemplateSyntaxError,
	      env_trim.from_string,
	                  """\
{% macro a() %}{{ caller() }}{% endmacro %}
{% call(x, y=1, z) a() %}{% endcall %}""",
)

varargs
			                """\
{% macro test() %}{{ varargs|join('|') }}{% endmacro %}\
{{ test(1, 2, 3) }}"""
()
"1|2|3"

simple_call
			                """\
{% macro test() %}[[{{ caller() }}]]{% endmacro %}\
{% call test() %}data{% endcall %}"""
()
"[[data]]"

complex_call
			                """\
{% macro test() %}[[{{ caller('data') }}]]{% endmacro %}\
{% call(data) test() %}{{ data }}{% endcall %}"""
()
"[[data]]"

caller_undefined
			                """\
{% set caller = 42 %}\
{% macro test() %}{{ caller is not defined }}{% endmacro %}\
{{ test() }}"""
()
"True"

include
env_trim = Environment(
		       loader=DictLoader(
					 {"include": "{% macro test(foo) %}[{{ foo }}]{% endmacro %}"}
					  )
					)
tmpl = env_trim.from_string('{% from "include" import test %}{{ test("foo") }}')
assert tmpl.render() == "[foo]"

def test_macro_api(self, env_trim):
tmpl = env_trim.from_string(
			    "{% macro foo(a, b) %}{% endmacro %}"
			    "{% macro bar() %}{{ varargs }}{{ kwargs }}{% endmacro %}"
			    "{% macro baz() %}{{ caller() }}{% endmacro %}"
			    )
assert tmpl.module.foo.arguments == ("a", "b")
assert tmpl.module.foo.name == "foo"
assert not tmpl.module.foo.caller
assert not tmpl.module.foo.catch_kwargs
assert not tmpl.module.foo.catch_varargs
assert tmpl.module.bar.arguments == ()
assert not tmpl.module.bar.caller
assert tmpl.module.bar.catch_kwargs
assert tmpl.module.bar.catch_varargs
assert tmpl.module.baz.caller

def test_callself(self, env_trim):
tmpl = env_trim.from_string(
			    "{% macro foo(x) %}{{ x }}{% if x > 1 %}|"
			    "{{ foo(x - 1) }}{% endif %}{% endmacro %}"
			    "{{ foo(5) }}"
			    )
assert tmpl.render() == "5|4|3|2|1"

def test_macro_defaults_self_ref(self, env):
tmpl = env.from_string(
		                   """
            {%- set x = 42 %}
            {%- macro m(a, b=x, x=23) %}{{ a }}|{{ b }}|{{ x }}{% endmacro -%}
        """
	)
assert tmpl.module.m(1) == "1||23"
assert tmpl.module.m(1, 2) == "1|2|23"
assert tmpl.module.m(1, 2, 3) == "1|2|3"
assert tmpl.module.m(1, x=7) == "1|7|7"


TestSet

normal
"{% set foo = 1 %}{{ foo }}"
()
"1"
assert tmpl.module.foo == 1

block
"{% set foo %}42{% endset %}{{ foo }}"
"42"
assert tmpl.module.foo == "42"

block_escaping
env = Environment(autoescape=True)
"{% set foo %}<em>{{ test }}</em>{% endset %}foo: {{ foo }}"
(test="<unsafe>")
"foo: <em>&lt;unsafe&gt;</em>"

set_invalid
pytest.raises(
	      TemplateSyntaxError, env_trim.from_string, "{% set foo['bar'] = 1 %}"
				   )
"{% set foo.bar = 1 %}"
exc_info = pytest.raises(TemplateRuntimeError, tmpl.render, foo={})
assert "non-namespace object" in exc_info.value.message

namespace_redefined
"{% set ns = namespace() %}{% set ns.bar = 'hi' %}"
exc_info = pytest.raises(TemplateRuntimeError, tmpl.render, namespace=dict)
assert "non-namespace object" in exc_info.value.message

namespace
"{% set ns = namespace() %}{% set ns.bar = '42' %}{{ ns.bar }}"
()
"42"

namespace_block
"{% set ns = namespace() %}{% set ns.bar %}42{% endset %}{{ ns.bar }}"
()
"42"

init_namespace
			    "{% set ns = namespace(d, self=37) %}"
			    "{% set ns.b = 42 %}"
			    "{{ ns.a }}|{{ ns.self }}|{{ ns.b }}"
(d={"a": 13})
"13|37|42"

namespace_loop
			    "{% set ns = namespace(found=false) %}"
			    "{% for x in range(4) %}"
			    "{% if x == v %}"
			    "{% set ns.found = true %}"
			    "{% endif %}"
			    "{% endfor %}"
			    "{{ ns.found }}"
(v=3)
"True"
(v=4)
"False"

namespace_macro
			    "{% set ns = namespace() %}"
			    "{% set ns.a = 13 %}"
			    "{% macro magic(x) %}"
			    "{% set x.b = 37 %}"
			    "{% endmacro %}"
			    "{{ magic(ns) }}"
			    "{{ ns.a }}|{{ ns.b }}"
()
"13|37"

block_escaping_filtered
env = Environment(autoescape=True)
"{% set foo | trim %}<em>{{ test }}</em>    {% endset %}foo: {{ foo }}"
(test="<unsafe>")
"foo: <em>&lt;unsafe&gt;</em>"

block_filtered
"{% set foo | trim | length | string %} 42    {% endset %}{{ foo }}"
assert tmpl.render() == "2"
assert tmpl.module.foo == "2"

block_filtered_set
def _myfilter(val, arg):
    assert arg == " xxx "
    return val

env_trim.filters["myfilter"] = _myfilter
tmpl = env_trim.from_string(
			    '{% set a = " xxx " %}'
				"{% set foo | myfilter(a) | trim | length | string %}"
				' {% set b = " yy " %} 42 {{ a }}{{ b }}   '
				  "{% endset %}"
				  "{{ foo }}"
				  )
assert tmpl.render() == "11"
assert tmpl.module.foo == "11"


TestWith:

with
		                   """\
        {% with a=42, b=23 -%}
            {{ a }} = {{ b }}
        {% endwith -%}
            {{ a }} = {{ b }}\
        """
assert [x.strip() for x in tmpl.render(a=1, b=2).splitlines()] == [
"42 = 23",
"1 = 2",
]

with_argument_scoping
		                   """\
        {%- with a=1, b=2, c=b, d=e, e=5 -%}
            {{ a }}|{{ b }}|{{ c }}|{{ d }}|{{ e }}
        {%- endwith -%}
        """
(b=3, e=4)
"1|2|3|4|5"
