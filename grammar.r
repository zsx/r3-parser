REBOL [
   Title: "Rebol 3 parser"
   Type: 'module
   Exports: [scan-source]
]


debug: :print

err: _
pos: _
line-no: _
last-line: _

read-hex: function [
    num [string! binary!]
][
    num: uppercase num
    ret: 0
    for-each d num [
        case [
            all [d >= #"0" d <= #"9"][
                ret: ret * 10 + (to integer! d) - 48 ; 48 = to integer! #"0"
            ]
            all [d >= #"A" d <= #"F"][
                ret: ret * 10 + (to integer! d) - 65 + 10; 65 = to integer! #"A"
            ]
            'else [
                fail spaced ["Invalid hex digit:" d]
            ]
        ]
    ]
    ret
]

syntax-errors: [
    invalid-integer "Invalid integer"
]

abort: func [
    reason [word!]
][
    print ["Aborting due to" reason "at:" pos]
    print ["source:" head pos]
    err: reason
    fail select syntax-errors reason
]

digit: charset "0123456789"
hex-digit: charset "0123456789ABCDEF"
letter: charset [#"a" - #"z"]
byte: complement charset {}
lit-prefix: {'}
space-char: charset "^@^(01)^(02)^(03)^(04)^(05)^(06)^(07)^(08)^(09)^(0A)^(0B)^(0C)^(0D)^(0E)^(0F)^(10)^(11)^(12)^(13)^(14)^(15)^(16)^(17)^(18)^(19)^(1A)^(1B)^(1C)^(1D)^(1E)^(1F)^(20)^(7F)"
non-space: complement space-char
special-char: charset "@%\:'<>+-~|_.,#$"
;word-char: complement charset "@%\,#$:"
non-special-word-char: charset ["!&*=?" #"A" - #"Z" "^^`" #"a" - #"z" #"~"
    #"^(80)" - #"^(BF)" ;old control chars and alternate chars
    #"^(C2)" - #"^(FE)"
]

special-word-char: charset "+-~|_."
leading-word-char: union non-special-word-char special-word-char
word-char: union leading-word-char digit

line-break: [
    [
        "^(0A)^(0D)"
        | "^(0A)"
        | "^(0D)"
    ] last-line: (++ line-no)
]

space: [
    line-break
    | space-char
]

non-space-delimiter: charset {()[]{}^"/;}
delimiter: [
    space
    | non-space-delimiter
    | end
]

required-close-brace: [
    #"}" | pos: (abort 'missing-close-bracket)
]

required-quote: [
    #"^"" | pos: (abort 'missing-close-quote)
]

required-close-angle: [
]

unsigned-integer: context [
    val: 0
    s: _

    rule: [
        copy s [
            some digit ;allow leading zeros
        ]
        (if error? err: try [val: to integer! s][
            ;FIXME: examine the error for better error message
            fail ["invalid integer! at:" pos]
        ])
    ]
]

integer: context [
    val: 0
    s: _

    rule: [
        copy s [
            opt [#"+" | #"-"]
            unsigned-integer/rule
        ]
        (if error? err: try [val: to integer! s][
            ;FIXME: examine the error for better error message
            fail ["invalid integer! at:" pos]
        ])
    ]
]

unsigned-decimal: context [
    fraction: [
        [#"," | #"."]
        any digit
    ]

    exponential: [
        #"E" integer/rule
    ]

    rule: [
        any digit
        [
            [fraction opt exponential]
            | [opt fraction exponential]
        ]
    ]
]

decimal: context [
    val: _
    s: _
    rule: [
        copy s [
            opt [#"+" | #"-"]
            unsigned-decimal/rule
        ]
        (val: to decimal! s)
    ]
]

percent: context [
    val: 0%

    rule: [
        decimal/rule
        #"%"
        (val: to percent! decimal/val / 100)
    ]
]

money: context [
    val: $0.00
    s: _
    sign: _

    rule: [
        opt copy sign [#"+" | #"-"] ;-$2.1, not $-2.1
        #"$"
        copy s unsigned-decimal/rule
        (
            val: to money! to decimal! either blank? sign [s][join-of sign s]
        )
    ]
]

time: context [
    val: 00:00:00
    hour: 0
    minute: 0
    sec: 0.0
    t: _
    intra-day: false

    init: does [
        val: 00:00:00
        hour: 0
        minute: 0
        sec: 0.0
        intra-day: false
    ]

    rule: [
        (init)
        pos:
        opt [integer/rule (hour: integer/val)]
        #":"
        integer/rule (minute: integer/val)
        opt [
            #":"
            [
                integer/rule (sec: integer/val)
                | decimal/rule (sec: decimal/val)
            ]
            opt [
                [
                     "AM"
                     | "PM" (hour: hour + 12)
                ]
                (intra-day: true)
            ]
        ]
        (
            val: make time! reduce [hour minute sec]
            if all [intra-day any [hour < 0 hour >= 24]][
                fail "time format incorrect for intra-day time"
            ]
        )
    ]
]

pair: context [
    val: 0x0
    x: _

    rule: [
        [
            decimal/rule (x: decimal/val)
            #"x"
            decimal/rule 
            (val: to pair! reduce [x decimal/val])
        ]
    ]
]

date: context [
    val: _
    day: _
    month: _
    year: _
    tz: _

    init: does [
        day: _
        month: _
        year: _
        tz: _
    ]

    rule: [
        [
            unsigned-integer/rule (day: unsigned-integer/val)
            #"-"
            unsigned-integer/rule (month: unsigned-integer/val)
            #"-"
            unsigned-integer/rule (year: unsigned-integer/val)
        ]
        | [
            unsigned-integer/rule (day: unsigned-integer/val)
            #"/"
            unsigned-integer/rule (month: unsigned-integer/val)
            #"/"
            unsigned-integer/rule (year: unsigned-integer/val)
        ]
    ]
]

string: context [
    val: _
    nest: 0
    n: _
    s: _
    non-close-paren: complement charset ")"

    named-escapes: [
        "line"     "^(line)"
        "tab"      "^(tab)"
        "page"     "^(page)"
        "escape"   "^(escape)"
        "esc"      "^(esc)"
        "back"     "^(back)"
        "del"      "^(del)"
    ]

    escapes: [
        #"/"        #"^/"
        #"!"        #"^!"
        #"-"        #"^-"
        #"~"        #"^~"
        #"@"        #"^@"
        #"a"        #"^a"
        #"b"        #"^b"
        #"c"        #"^c"
        #"d"        #"^d"
        #"e"        #"^e"
        #"f"        #"^f"
        #"g"        #"^g"
        #"h"        #"^h"
        #"i"        #"^i"
        #"j"        #"^j"
        #"k"        #"^k"
        #"l"        #"^l"
        #"m"        #"^m"
        #"n"        #"^n"
        #"o"        #"^o"
        #"p"        #"^p"
        #"q"        #"^q"
        #"r"        #"^r"
        #"s"        #"^s"
        #"t"        #"^t"
        #"u"        #"^u"
        #"v"        #"^v"
        #"w"        #"^w"
        #"x"        #"^x"
        #"y"        #"^y"
        #"z"        #"^z"
        #"["        #"^["
        #"\"        #"^\"
        #"]"        #"^]"
        #"_"        #"^_"
    ]

    init: does [
        val: _
        nest: 0
        n: _
        b: _
    ]

    rule: [
        [
            #"^"" copy val [
                any [
                    {^^"}
                    | #"^"" break
                    | line-break (abort 'missing-close-quote)
                    | skip
                ]
            ] pos: (unless #"^"" last [abort 'missing-close-quote])
            | #"^{" copy val [
                any [
                    "^^{"
                    | #"^{" (++ nest)
                    | #"^}" (-- nest n: either zero? nest [] [break] [_]) n
                    | line-break
                    | skip
                ]
            ] pos: (unless #"^}" last [abort 'missing-close-brace])
        ](
            remove back tail val ;remove the trailing {"} or "^}"
            ;process escaping
            parse val [
                while [
                    change ["^^("
                        [
                            copy s [ 4 digit | 2 hex-digit] (c: to char! read-hex s)
                            | copy s [some non-close-paren] (c: select named-escapes s if blank? c [abort 'unrecognized-named-escape])
                        ]
                        ")"] c
                    | change ["^^" set b byte] (c: select named-escapes b unless c [c: ""]) c
                    | skip
                ]
            ]
        )
    ]
]

binary: context [
    val: _
    s: _
    rule: [
        "#{" (val: make binary! 1)
            any [
                copy s [
                    digit | pos: (err: 'invalid-hex-digit) fail
                    pos:
                    [
                        digit 
                        | #"}" (err: 'odd-binary-digit) fail
                        | (err: 'invalid-hex-digit) fail
                    ]
                ] (append val read-hex s)
            ]
        required-close-brace
    ]
]

word: context [
    val: _
    s: _
    rule: [
        copy s [ 
            [
                [
                    leading-word-char
                    any [leading-word-char | digit]
                ]
                | some #"/" ;all-slash words
            ]
        ] (val: to word! s)
        ;special words starting with #"<", not a tag
        | "<<" and delimiter (val: '<<)
        | "<=" and delimiter (val: '<=)
        | "<>" and delimiter (val: '<>)
        | "<|" and delimiter (val: to word! "<|")
        | "<-" and delimiter (val: to word! "<-")
        | ">>" and delimiter (val: '>>)

        ; some special chars can be signle-char words
        ; "@%\:,',$" are exceptions
        | #"#" and delimiter (val: _) ;bug???
        | #"<" and delimiter (val: '<)
        | #">" and delimiter (val: '>)
    ]
]

get-word: context [
    val: _
    rule: [
        #":"
        [word/rule (val: to get-word! word/val) | pos: (abort 'invalid-get-word-path)]
    ]
]

set-word: context [
    val: _
    rule: [
        word/rule #":" (val: to set-word! word/val)
    ]
]

lit-word: context [
    val: _
    rule: [
        lit-prefix
        [
            word/rule (val: to lit-word! word/val)
            | pos: (abort 'invalid-lit-word-path)
        ]
    ]
]

issue: context [
    val: _
    rule: [
        #"#"
        [word/rule (val: to issue! word/rule) | pos: (abort 'invalid-issue)]
    ]
]

refinement: context [
    val: _
    s: _
    rule: [
        pos: #"/"
        [
            | some #"/" fail ; all slashes are words
            ;exclude some words that can't be refinement
            | pos: [#"<" | #">"] (abort 'invalid-refinement)
            ;special refinements
            | word/rule (val: to refinement word/val)
        ]
    ]
]

path: context [
    val: _
    comp: _
    rule: [
        (val: make path! 1)
        some [
            [
                word/rule (comp: word/val)
                | group/rule (comp: group/val)
            ] #"/"
        ]
    ]
]

get-path: context [
    val: _
    rule: [
        #":"
        path/rule (val: to get-path! path/val)
    ]
]

set-path: context [
    val: _
    rule: [
        path/rule #":" (val: to set-path! path/val)
    ]
]

lit-path: context [
    val: _
    rule: [ 
        lit-prefix
        [
            [lit-prefix | #":"] (abort 'invalid-lit-path-word)
            | path/rule (val: to lit-path! path/val)
        ]
    ]
]

file: context [
    val: _
    s: _
    rule: [
        #"%" copy s [
            string/rule
            | any [#"/" | [not delimiter]]
        ]
        (val: to file! s)
    ]
]

stack: make block! 32
array: _

group: context [
    val: _
    rule: [
        #"(" (
            insert/only stack array
            array: make group! 1
        )
        rebol/rule
        (
            val: array
            array: take stack
        )
        [#")" | pos: (abort 'missing-close-paren)]
    ]
]

block: context [
    val: _
    rule: [
        #"[" (
            insert/only stack array
            array: make block! 1
        )
        rebol/rule
        (
            val: array
            array: take stack
        )
        [#"]" | pos: (abort 'missing-close-bracket)]
    ]
]

tag: context [
    val: _
    non-angle-bracket: complement charset ">"
    non-quote: complement charset {"}
    rule: [
        #"<"
        copy val [
            some [ ; "<>" is a word!
                [#"^"" any non-quote required-quote] ; ">" can be embedded if it's quoted
                | non-angle-bracket
            ]
        ]
        #">" (val: to tag! val); | pos: (abort 'missing-close-angle-bracket)]
    ]
]

url: context [
    val: _
    url-char: charset [letter digit "%/"]
    rule: [
        copy val [some byte "://" some url-char] (val: to url! val)
    ]
]

char: context [
    val: _
    rule: [
        [
            [ {#"} set val byte required-quote]
            | [ "#{" set val byte "}" ] ;missing a #"}" might not fail, keep matching binary
        ] (val: to char! byte)
    ]
]

construct: context [
    val: _
    rule: [
        #"#"
        block/rule
        (
            switch/default length val [
                2 [
                    val: make val/1 val/2
                ]
                1 [; #[false] #[true] or #[none]
                    unless find [false true none] val/1 [
                        abort 'mal-construct
                    ]
                ]
            ][
                abort 'mal-construct
            ]
        )
    ]
]

email: context [
    val: _
    id: charset [digit letter "_."]
    rule: [
        copy val [id #"@" id some [#"." id]]
        (val: to email! val)
    ]
]

void: context [
    val: _
    rule: [
        #"(" any space #")" (value: ())
    ]
]

comment: context [
    rule: [
        #";" thru "^/"
    ]
]

pre-parse: does [
    clear stack
    array: make block! 1
]

rebol: context [
    rule: [
        any [
            pos:
            ;special-char: charset "@%\:'<>+-~|_.,#$"
            ; sequence is important
            end
            | space
            | comment/rule
            | #"|" and delimiter            (append array '|)
            | "'|" and delimiter            (append array to lit-bar! '|)
            | #"_" and delimiter            (append array _)
            | pair/rule                     (append array pair/val)
            | time/rule                     (append array time/val)         ;before integer
            | percent/rule                  (append array percent/val)      ;before decimal
            | decimal/rule                  (append array decimal/val)      ;before integer
            | integer/rule [and delimiter | pos: (abort 'invalid-integer)]
                                            (append array integer/val)
            | set-path/rule                 (append array set-path/val)     ;before path
            | path/rule                     (append array path/val)         ;before word
            | set-word/rule                 (append array set-word/val)     ;before word
            | word/rule                     (append array word/val)
            | block/rule                    (append/only array block/val)   ;#"["
            ;| void/rule                    (append array block/val)         ;#"("
            | group/rule                    (append/only array group/val)   ;#"("
            | string/rule                   (append array string/val)       ;#"{"
            | char/rule                     (append array char/val)         ;"#{" or {#"}
            | binary/rule                   (append array binary/val)       ;"#{"
            | issue/rule                    (append array issue/val)        ;#"#"
            | file/rule                     (append array file/val)         ;#"%"
            | money/rule                    (append array money/val)        ;#"$"
            | lit-path/rule                 (append array lit-path/val)     ;#"'"
            | get-path/rule                 (append array get-path/val)     ;#":"
            | lit-word/rule                 (append array lit-word/val)     ;#"'"
            | get-word/rule                 (append array get-word/val)     ;#":"
            | refinement/rule               (append array get-word/val)     ;#"/"
            | tag/rule                      (append array tag/val)          ;#"<"
            ;| delimiter                                                    ; non-space delimiters must have been consumed
            ;| skip                          ();invalid UTF8 byte?
            | [and delimiter | pos: (abort 'invalid-word)]
        ]
    ]
]

scan-source: function [
    source [binary! string!]
][
    debug ["scanning:" mold source]
    pre-parse

    ;trace on
    if parse source rebol/rule [
        print ["block:" mold array]
        return array
    ]

    print ["partial block:" mold array]
    fail spaced ["Syntax error at:" pos]
]
