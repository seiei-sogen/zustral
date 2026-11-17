" Language:    zustral

" quit when a syntax file was already loaded
if exists("b:current_syntax")
	finish
endif
let s:keepcpo = &cpo
set cpo&vim


let b:current_syntax = "zustral"


" Section: Operatoren {{{1
"
syntax keyword zustralOperator not
syntax match   zustralOperator "\<and\>"
syntax match   zustralOperator "\<and\s\+then\>"
syntax match   zustralOperator "\<or\>"
syntax match   zustralOperator "\<or\s\+else\>"
syntax match   zustralOperator "[-+*/<>&]"
syntax keyword zustralOperator **
syntax match   zustralOperator "[/<>]="
syntax match   zustralOperator "="



" Section: Keywords {{{1
"
syntax keyword zustralKeyword return let nil borrow skip generic instance method

" Section: Specials {{{1
"
syntax match zustralSpecial	":="
syntax match zustralSpecial "=>"
syntax match        zustralSpecial       "[;().,]"  


" Section: Function 
" 
" function = "function" identifier "(" [ identifier { "," identifier } ] ")" [ ":" type ] [ "is" ] { statement } "end"
"
"syntax region zustralFunction start="\<function\>\s*\z(\k*\)" end="end\s*;"
"			\ keepend extend transparent fold contains=ALL



" Section: Numbers, including floating point. {{{1
"
" digit = [0-9]
" digits = digit, { digit | "_" };
" integer constant = ["+", "-"], digits;
" float constant = digits, ".", digits, ["e", ["+", "-"], integer constant];
"
syntax match zustralNumber		"\v[+-]?[0-9_]+" " integer constant
syntax match zustralFloat		"\v[+-]?[0-9_]+\.[0-9_]+([eE][+-]?[0-9_]+)?" " float constant


" Section: Boolean Constants {{{1
" 
syntax keyword zustralBoolean	true false

" Section: Preprocessor {{{1
"
" https://austral-lang.org/spec/spec.html#modules 
"
syntax keyword  zustralPreproc		 pragma
syntax keyword  zustralPreproc		 import

" Section: Structures {{{1
"
" https://austral-lang.org/spec/spec.html#record-definition
"
"         \ start="\<record\>"
"         \ end="\<end\>"
"         \ keepend extend transparent fold contains=ALL
syntax match zustralStructure "\<[A-Z][a-zA-Z0-9_]*\>"


" Section: Typedef
"
" https://austral-lang.org/spec/spec.html#union-definition
" https://austral-lang.org/spec/spec.html#typeclass-definition
" 
"syntax match zustralTypedef   "\<typeclass\>"
"syntax match zustralTypedef   "\<union\>"


" Section: Labels {{{1
"
" case = "case" expression "of" { "when" expression "do" statement } "end case"
"
" https://austral-lang.org/spec/spec.html#stmt-case
"
syntax match zustralLabel		"\<end\s\+case\>"
syntax keyword zustralLabel		case when do of

" Section: String and character constants. {{{1
"
" string constant = '"', { any character - '"' | '\"' }, '"'; 
"
syntax region zustralString start=+"+ skip=+\\"+ end=+"+ contains=@Spell


" Section: Todo (only highlighted in comments) {{{1
"
syntax keyword zustralTodo contained TODO FIXME XXX NOTE


" Section: Comments. {{{1
"
" Zustral oneline comments are start with "--" and end with a newline.
" Multiline comments are enclosed in "```\n", "\n```".
"
" comment = "-- ", {any character}, "\n";
" docstring = "```\n", { any character - "```" } ,"\n```";
"
" Single line comments
syntax region  zustralComment 
			\ oneline 
			\ contains=zustralTodo,@Spell
			\ start="--" 
			\ end="$" 
" Multiline comments
syntax region  zustralComment start=+```+ end=+```+ contains=zustralTodo,@Spell keepend



" Section: Zustral Regions {{{1
"
syntax match zustralRegion "\<module\>"
syntax match zustralRegion "\<module\s\+body\>"
syntax match zustralRegion	"\<function\>"
syntax match zustralRegion  /\<end\>/
syntax match zustralRegion   "\<typeclass\>"
syntax match zustralRegion   "\<is\>"
syntax match zustralRegion   "\<record\>"
syntax match zustralRegion   "\<union\>"


" Section: Conditionals {{{1
"
" https://austral-lang.org/spec/spec.html#stmt-if
"
syntax match zustralConditional		"\<if\>"
syntax match zustralConditional		"\<then\>"
syntax match zustralConditional		"\<else\>"
syntax match    zustralConditional	"\<else\s\+if>"
syntax match    zustralConditional	"\<end\s\+if\>"
syntax match    zustralConditional	"\<end\s\+case\>"


" Section: Repeat {{{1
" 
" while = "while", expression, "do", { statement }, "end while";
" for = "for", identifier, "from", expression, "to" expression, "do", { statement }, "end for";
"
" https://austral-lang.org/spec/spec.html#stmt-while
" https://austral-lang.org/spec/spec.html#stmt-for
"
syntax match zustralRepeat "\<while\>"
syntax match zustralRepeat "\<end\s\+while\>"
syntax match zustralRepeat "\<for\>"
syntax match zustralRepeat "\<end\s\+for\>"


" Section: The default methods for highlighting. Can be overridden later. {{{1
"
highlight def link zustralComment	    Comment
highlight def link zustralString	    String
highlight def link zustralNumber	    Number
highlight def link zustralFloat		    Float
highlight def link zustralBoolean	    Boolean
highlight def link zustralRegion	    Function
highlight def link zustralConditional       Conditional
highlight def link zustralRepeat            Repeat
highlight def link zustralLabel             Label
highlight def link zustralOperator          Operator
highlight def link zustralKeyword	    Keyword
highlight def link zustralPreproc           PreProc
highlight def link zustralStructure	    Structure
highlight def link zustralTodo		    Todo
highlight def link zustralTypedef	    Typedef
highlight def link zustralSpecial	    Special


let &cpo = s:keepcpo
unlet s:keepcpo

finish
