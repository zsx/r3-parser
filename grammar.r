REBOL [
   Title: "Rebol 3 parser"
   Type: 'module
   Exports: [scan-source]
]


debug: :print

err: _
pos: _
line-no: 1
last-line: _
open-at: 0x0

read-hex: function [
    num [string! binary!]
][
    num: uppercase num
    ret: 0
    for-each d num [
        case [
            all [d >= #"0" d <= #"9"][
                ret: ret * 16 + (to integer! d) - 48 ; 48 = to integer! #"0"
            ]
            all [d >= #"A" d <= #"F"][
                ret: ret * 16 + (to integer! d) - 65 + 10; 65 = to integer! #"A"
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
    print ["Aborting due to" reason "at:" line-no "x" (index-of pos) - (index-of last-line)]
    print ["Last open-at:" mold open-at]
    print ["source:" to string! pos]
    err: reason
    fail (select* syntax-errors reason else spaced ["Unknown error" reason])
]

digit: charset "0123456789"
hex-digit: charset "0123456789ABCDEF"
letter: charset [#"a" - #"z"]
sign: charset "+-"
byte: complement charset {}
lit-prefix: {'}
space-char: charset "^@^(01)^(02)^(03)^(04)^(05)^(06)^(07)^(08)^(09)^(0A)^(0B)^(0C)^(0D)^(0E)^(0F)^(10)^(11)^(12)^(13)^(14)^(15)^(16)^(17)^(18)^(19)^(1A)^(1B)^(1C)^(1D)^(1E)^(1F)^(20)^(7F)"
non-space: complement space-char
special-char: charset "@%\:'<>+-~|_.,#$"

count-line-no?: true

line-break: [
    [
        "^(0A)^(0D)"
        | #"^(0A)"
        | #"^(0D)"
    ] last-line: (if count-line-no? [++ line-no])
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

and-delimiter: [
    (count-line-no?: false)
    [and delimiter (count-line-no?: true) | (count-line-no?: true) fail]
]

open-brace: [
    pos: #"^{" (open-at: to pair! reduce [line-no (index-of pos) - (index-of last-line)])
]

open-bracket: [
    pos: #"[" (open-at: to pair! reduce [line-no (index-of pos) - (index-of last-line)])
]

open-quote: [
    pos: #"^"" (open-at: to pair! reduce [line-no (index-of pos) - (index-of last-line)])
]

open-paren: [
    pos: #"(" (open-at: to pair! reduce [line-no (index-of pos) - (index-of last-line)])
]

required-close-brace: [
    #"^}" | pos: (abort 'missing-close-brace)
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
            abort 'invalid-integer
        ])
    ]
]

integer: context [
    val: 0
    s: _

    rule: [
        copy s [
            opt sign
            unsigned-integer/rule
        ]
        (if error? err: try [val: to integer! s][
            ;FIXME: examine the error for better error message
            abort 'invalid-integer
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
            opt sign
            unsigned-decimal/rule
        ]
        (val: to decimal! s)
    ]
]

any-number: context [
    val: _
    rule: [
        decimal/rule (val: decimal/val)
        | integer/rule (val: integer/val)
    ]
]

percent: context [
    val: 0%

    rule: [
        any-number/rule #"%" (val: to percent! any-number/val / 100)
    ]
]

money: context [
    val: $0.00
    s: _
    sign-char: _

    rule: [
        opt copy sign-char sign ;-$2.1, not $-2.1
        #"$"
        copy s unsigned-decimal/rule
        (
            val: to money! to decimal! either blank? sign [s][join-of sign-char s]
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
            #":" any-number/rule (sec: any-number/val)
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
                abort 'invalid-time
            ]
        )
    ]
]

pair: context [
    val: 0x0
    x: _

    rule: [
        [
            any-number/rule (x: any-number/val)
            #"x"
            any-number/rule
            (val: to pair! reduce [x any-number/val])
        ]
    ]
]

date: context [
    val: _
    day: _
    month: _
    year: _
    tz: _
    t: _
    sep: _
    sign-char: _

    init: does [
        day: _
        month: _
        year: _
        t: _
        tz: _
    ]

    named-month: [
        "Jan" opt [#"u" opt [#"a" opt [ #"r" opt #"y"]]]                            (month: 1)
        | "Feb" opt [#"r" opt [#"u" opt [#"a" opt [ #"r" opt #"y"]]]]               (month: 2)
        | "Mar" opt [#"c" opt opt #"h"]                                             (month: 3)
        | "Apr" opt [#"i" opt opt #"l"]                                             (month: 4)
        | "May"                                                                     (month: 5)
        | "Jun" opt #"e"                                                            (month: 6)
        | "Jul" opt #"y"                                                            (month: 7)
        | "Aug" opt [#"u" opt [#"s" opt #"t"]]                                      (month: 8)
        | "Sep" opt [#"t" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]]    (month: 9)
        | "Oct" opt [#"o" opt [#"b" opt [#"e" opt #"r"]]]                           (month: 10)
        | "Nov" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]               (month: 11)
        | "Dec" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]               (month: 12)
    ]

    rule: [
        (init)
        [
            unsigned-integer/rule (day: unsigned-integer/val)
            set sep [#"-" | #"/"]
            [
                unsigned-integer/rule (month: unsigned-integer/val)
                | named-month
            ]
            sep
            unsigned-integer/rule (year: unsigned-integer/val)
            opt [
                #"/"
                time/rule (t: time/val)
                opt [
                    set sign-char sign
                    time/rule (
                        either sign-char = #"-" [
                            tz: negate time/val
                        ][
                            tz: time/val
                        ]
                    )
                ]
            ]
            (
                val: reduce [day month year]
                if t [append val t]
                if tz [append val tz]
                val: make date! val
            )
        ]
    ]
]

string: context [
    val: _
    s: _
    b: _
    c: _
    non-quote: complement charset "^""

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
        b: _
        c: _
    ]
    non-close-brace: complement charset "^}"

    in-brace-rule: [
        any [
            {^^^^}
            | "^^{"
            | "^^}"
            | line-break

            ;unescaped braces
            | open-brace in-brace-rule required-close-brace
            | non-close-brace
        ]
    ]

    rule: [
        (init)
        [
            open-quote copy val [
                any [
                    {^^^^}
                    | {^^"}
                    | line-break (abort 'missing-close-quote)
                    | non-quote
                ]
            ] required-quote
            | open-brace copy val [ in-brace-rule ] required-close-brace
        ](
            ;process escaping
            ;print ["unescapped string:" mold val]
            ;trace on
            parse val [
                while [
                    change {^^^^} {^^}
                    | change ["^^("
                        [
                            copy s [ 4 digit | 2 hex-digit] and ")" (c: to char! read-hex s) ;and ")" is to prevent it matches the "ba" in "back"
                            | copy s [some letter] (c: select named-escapes s if blank? c [abort 'unrecognized-named-escape])
                        ]
                        ")"] c
                    | change ["^^" set b byte (c: any [select escapes b b])] c
                    | skip
                ]
            ]
            ;trace off
            ;print ["escapped string:" mold val]
        )
    ]
]

binary: context [
    val: _
    s: _
    rule: [
        "#" open-brace (val: make binary! 1)
            any [
                copy s [
                    digit | pos: (abort 'invalid-hex-digit)
                    pos:
                    [
                        digit
                        | #"^}" (abort 'odd-binary-digit)
                        | (abort 'invalid-hex-digit)
                    ]
                ] (append val read-hex s)
            ]
        required-close-brace
    ]
]

word: context [
    val: _
    s: _

    regular-word-char: charset ["!&*=?" #"A" - #"Z" #"_" "^`" #"a" - #"z" #"|" #"~"
        #"^(80)" - #"^(BF)" ;old control chars and alternate chars
        #"^(C2)" - #"^(FE)"
    ] ;can appear in anywhere in a word

    num-starter: charset "+-." ;must be followed by a non-digit
    rule: [
        copy s [
            [
                num-starter delimiter ;num-starters can be words by themselves
                | [
                    opt num-starter
                    [regular-word-char | num-starter]
                    any [num-starter | regular-word-char | digit]
                ]
                | some #"/" and-delimiter ;all-slash words
            ]
        ] (val: to word! to string! s)
        ;special words starting with #"<", not a tag
        | "<<" and-delimiter (val: '<<)
        | "<=" and-delimiter (val: '<=)
        | "<>" and-delimiter (val: '<>)
        | "<|" and-delimiter (val: to word! "<|")
        | "<-" and-delimiter (val: to word! "<-")
        | ">>" and-delimiter (val: '>>)
        | ">=" and-delimiter (val: '>=)

        ; some special chars can be signle-char words
        ; "@%\:,',$" are exceptions
        | #"#" and-delimiter (val: _) ;bug???
        | #"<" and-delimiter (val: '<)
        | #">" and-delimiter (val: '>)
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
    rule: [
        pos: #"/"
        [
            some #"/" fail ; all slashes are words
            ;exclude some words that can't be refinement
            | pos: [#"<" | #">"] (abort 'invalid-refinement)
            | integer/rule (val: to refinement! integer/val)
            | word/rule (val: to refinement! word/val)
        ]
    ]
]

path: context [
    val: _

    rule: [
        word/rule (
            val: make path! 1
            append/only val word/val
        )
        some [
            #"/"
            [
                (insert/only stack val) ;this could cause recursive calls to path/rule
                ;(print "calling item/rule from path")
                item/rule (
                    val: take stack
                    append/only val item/val
                )
                | (take stack) pos: (abort 'invalid-path)
            ]
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
    white-char: charset [#"^(00)" - #"^(20)"]
    valid-in-quotes: complement charset {:;^"}
    valid: complement union white-char charset {:;()[]^"}
    rule: [
        #"%" [
            [
                #"^""
                copy s [any valid-in-quotes]
                required-quote
            ] | [
                copy s [any valid]
            ]
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
        open-bracket (
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
    s: _
    c: _
    rule: [
        copy val [
            word/rule
            "://"
            any [
                [#"%" 2 hex-digit]
                | #"/"
                | [not delimiter]
            ]
        ] (
            ; replace all %xx
            parse val [
                while [
                    change [#"%" copy s [2 hex-digit] (c: to char! read-hex s)] c
                    | skip
                ]
            ]

            val: to url! val
        )
    ]
]

char: context [
    val: _
    rule: [
        {#} string/rule (
            unless 1 = length string/val [
                ;print ["length of string is not 1" mold string/val]
                abort 'invalid-char
            ]
            val: first string/val
        )
    ]
]

construct: context [
    val: _
    rule: [
        #"#"
        block/rule
        (
            debug ["block:" mold block/val]
            switch/default length block/val [
                2 [
                    val: bind block/val lib
                    val: make val/1 val/2
                ]
                1 [; #[false] #[true] or #[none]
                    val: switch/default first block/val [
                        true [true]
                        false [false]
                        none [_]
                    ][
                        abort 'mal-construct
                    ]
                ]
            ][
                abort 'mal-construct
            ]
        )
    ]
]

byte-integer: context [
    val: _
    four-or-less: charset "01234"
    five-or-less: charset "012345"
    rule: [
        copy val [
            #"1" opt [digit opt digit]
            | #"2" [
                four-or-less digit
                | #"5" five-or-less
            ]
            | digit opt digit
        ] (val: to integer! val)
    ]
]

tuple: context [
    val: _

    rule: [
        (val: make binary! 7)
        byte-integer/rule (append val byte-integer/val)
        #"." byte-integer/rule (append val byte-integer/val)
        some [#"."
            [
                byte-integer/rule (append val byte-integer/val)
                | pos: (abort 'invalid-tuple)
            ]
        ]

        (val: to tuple! val)
    ]
]

email: context [
    val: _
    leading-email-char: complement union space-char charset "<@"
    non-at: complement union space-char charset "@"
    rule: [
        copy val [
            leading-email-char
            any non-at
            #"@"
            any [not delimiter] ; "to delimiter" causes invalid-rule error ???
        ] (val: to email! val)
    ]
]

void: context [
    val: _
    rule: [
        #"(" any space #")" (value: ())
    ]
]

comment: context [
    val: _
    rule: [
        copy val [
            #";" thru
            [
                "^(0A)^(0D)"
                | #"^(0A)"
                | #"^(0D)"
            ] last-line: (if count-line-no? [++ line-no])
        ] ;(print ["comment found:" mold val])
    ]
]

pre-parse: func [
    source [binary! string!]
][
    clear stack
    array: make block! 1
    pos: source
    last-line: source
    line-no: 1
    err: _
]

item: context [
    val: _
    rule: [
        ; sequence is important
        ;pos: (print ["matching item/rule against^/" copy/part pos 80])
        #"|" and-delimiter              (val: '|)
        | "'|" and-delimiter            (val: to lit-bar! '|)
        | #"_" and-delimiter            (val: _)
        | block/rule                    (val: block/val)                ;    [
        ;| void/rule                    (val: block/val)                ;    (
        | group/rule                    (val: group/val)                ;    (
        | string/rule                   (val: string/val)               ;    {
        | char/rule                     (val: char/val)                 ;    #{ or #"
        | binary/rule                   (val: binary/val)               ;    #{ or 64#
        | construct/rule                (val: construct/val)            ;    #[
        | issue/rule                    (val: issue/val)                ;    #
        | file/rule                     (val: file/val)                 ;    %
        | money/rule                    (val: money/val)                ;    $
        | lit-path/rule                 (val: lit-path/val)             ;    '
        | get-path/rule                 (val: get-path/val)             ;    :
        | lit-word/rule                 (val: lit-word/val)             ;    '
        | get-word/rule                 (val: get-word/val)             ;    :

        | email/rule                    (val: email/val)

        | tuple/rule                    (val: tuple/val)
        | percent/rule                  (val: percent/val)              ;before decimal
        | decimal/rule                  (val: decimal/val)              ;before integer
        | pair/rule                     (val: pair/val)
        | time/rule                     (val: time/val)                 ;before integer
        | date/rule                     (val: date/val)                 ;before integer
        | integer/rule ;[and-delimiter | (abort 'invalid-integer)]
                                        (val: integer/val)
        | url/rule                      (val: url/val)                  ;before set-word
        | set-path/rule                 (val: set-path/val)             ;before path
        | path/rule                     (val: path/val)                 ;before word
        | set-word/rule                 (val: set-word/val)             ;before word
        | word/rule                     (val: word/val)                 ;before refinement, because #"/" could be a word

        | refinement/rule               (val: refinement/val)           ;#"/"
        | tag/rule                      (val: tag/val)                  ;#"<"
    ]
]

rebol: context [
    rule: [
        any [
            pos:
            ; sequence is important
            end
            | space
            | comment/rule
            | item/rule         (append/only array item/val)
            ;| skip                          ();invalid UTF8 byte?
            | [and [#")" | #"]"] | pos: (abort 'invalid-word)]
        ]
    ]
]

scan-source: function [
    source [binary! string!]
][
;    debug ["scanning:" mold source]
    pre-parse source

    ;trace on
    ret: try [parse source rebol/rule]
    ;trace off
;    debug ["block:" mold array]
    if error? ret [
        fail ret
    ]

    if ret [
        return array
    ]
]
